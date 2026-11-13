-- | Ways to say what the wallpaper is. They are all small functions from
-- the situation ('Ctx') to a picture, and they nest:
--
-- > wallpaper = byWorkspace [ ("web", image "~/Pictures/sea.jpg") ]
-- >               (byHour [ (8, slideshow 30 ["~/Pictures/day"]), (21, image "~/Pictures/night.png") ])
module Bgmonad.Wallpapers
  ( image
  , solid
  , slideshow
  , shuffled
  , byWorkspace
  , byOutput
  , byHour
  , expandHome
  ) where

import qualified Control.Exception as E
import Data.Bits (shiftR, xor)
import Data.Char (toLower)
import Data.List (sort, sortOn)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import Data.Word (Word64)
import System.Directory
import System.FilePath (takeExtension, (</>))

import Bgmonad.Core
import Bgmonad.Rivermonad (Workspace (..))

-- | An image file (PNG, JPEG, WebP... whatever gdk-pixbuf loads). @~/@ is
-- expanded and links are followed: if the file is a link and you point it
-- somewhere else, the wallpaper changes within a few seconds.
image :: FilePath -> Wallpaper
image path = Wallpaper $ \_ -> PickImage <$> resolve path

-- | A plain colour, @#rrggbb@.
solid :: String -> Wallpaper
solid c = Wallpaper $ \_ -> pure (PickColor c)

-- | @slideshow minutes paths@: one image after another, each for that
-- many minutes. Paths are images or directories of images (searched every
-- time, so new files join in). @bgmonad next@ and @bgmonad prev@ step
-- through it by hand.
slideshow :: Double -> [FilePath] -> Wallpaper
slideshow = slides False

-- | Like 'slideshow' in a random order: every image is shown once before
-- any repeats.
shuffled :: Double -> [FilePath] -> Wallpaper
shuffled = slides True

slides :: Bool -> Double -> [FilePath] -> Wallpaper
slides random minutes paths = Wallpaper $ \ctx -> do
  files <- concat <$> mapM listImages paths
  let n = length files
      period = max 1 (round (minutes * 60)) :: Integer
      i = ctxSeconds ctx `div` period + fromIntegral (ctxStep ctx)
      (cycleNo, pos) = i `divMod` fromIntegral (max 1 n)
      order
        | random = sortOn (hash (fromIntegral cycleNo)) files
        | otherwise = files
  pure $ if n == 0 then PickColor "#000000" else PickImage (order !! fromIntegral pos)

-- | A wallpaper for some of rivermonad's workspaces (by name) and another
-- one for the rest.
byWorkspace :: [(String, Wallpaper)] -> Wallpaper -> Wallpaper
byWorkspace table rest = Wallpaper $ \ctx ->
  pickWallpaper (maybe rest id (ctxWorkspace ctx >>= \w -> lookup (wsName w) table)) ctx

-- | A wallpaper for some outputs (by name, e.g. @"HDMI-A-1"@) and another
-- one for the rest.
byOutput :: [(String, Wallpaper)] -> Wallpaper -> Wallpaper
byOutput table rest = Wallpaper $ \ctx ->
  pickWallpaper (maybe rest id (lookup (ctxOutput ctx) table)) ctx

-- | Wallpapers for the time of day: every entry starts at its hour (7.5
-- is half past seven) and lasts until the next one; the last one goes on
-- through midnight.
byHour :: [(Double, Wallpaper)] -> Wallpaper
byHour [] = solid "#000000"
byHour table = Wallpaper $ \ctx ->
  let TimeOfDay h m _ = localTimeOfDay (ctxTime ctx)
      now = fromIntegral h + fromIntegral m / 60 :: Double
      sorted = sortOn fst table
      w = case [ x | (start, x) <- sorted, start <= now ] of
        [] -> snd (last sorted)
        xs -> last xs
  in pickWallpaper w ctx

------------------------------------------------------------------------------

expandHome :: FilePath -> IO FilePath
expandHome ('~' : '/' : rest) = (</> rest) <$> getHomeDirectory
expandHome p = pure p

resolve :: FilePath -> IO FilePath
resolve path = do
  p <- expandHome path
  E.try (canonicalizePath p) >>= \case
    Right c -> pure c
    Left e -> const (pure p) (e :: E.IOException)

-- | The images of a directory in name order (not recursive), or the file.
listImages :: FilePath -> IO [FilePath]
listImages path = do
  p <- resolve path
  isDir <- doesDirectoryExist p
  if not isDir
    then pure [p]
    else do
      r <- E.try (listDirectory p)
      pure $ case r of
        Left e -> const [] (e :: E.IOException)
        Right names -> [ p </> n | n <- sort names, map toLower (takeExtension n) `elem` exts ]
  where
    exts = [".png", ".jpg", ".jpeg", ".webp", ".avif", ".jxl", ".bmp", ".tif", ".tiff", ".gif", ".heic", ".svg"]

-- | A hash of a string that depends on a seed (FNV-1a and a final mix).
hash :: Word64 -> String -> Word64
hash seed = finish . foldl step (14695981039346656037 `xor` (seed * 0x9e3779b97f4a7c15))
  where
    step h c = (h `xor` fromIntegral (fromEnum c)) * 1099511628211
    finish h = let a = (h `xor` (h `shiftR` 33)) * 0xff51afd7ed558ccd in a `xor` (a `shiftR` 33)
