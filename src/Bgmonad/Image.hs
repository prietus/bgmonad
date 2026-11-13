-- | Pictures in memory and the C routines that load and draw them.
module Bgmonad.Image
  ( Layer (..)
  , loadLayer
  , blurLayer
  , freeLayer
  , drawLayers
  , copyLayer
  , fillPixels
  , parseColor
    -- * Shared memory
  , shmFile
  , shmMap
  , shmUnmap
  ) where

import Data.Char (digitToInt, isHexDigit)
import Data.Word (Word32)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArrayLen, withArray)
import Foreign.Ptr
import Foreign.Storable (peek, poke)

import Bgmonad.Core (Fit (..))

foreign import ccall safe "bgmonad_load"
  c_load :: CString -> CInt -> CInt -> CInt -> Word32 -> CDouble -> Ptr CString -> IO (Ptr Word32)
foreign import ccall safe "bgmonad_blur"
  c_blur :: Ptr Word32 -> CInt -> CInt -> CInt -> IO (Ptr Word32)
foreign import ccall safe "bgmonad_warp"
  c_warp :: Ptr Word32 -> CInt -> CInt -> Ptr CFloat -> CInt -> CInt
         -> Ptr (Ptr Word32) -> Ptr Word32 -> Ptr Word32 -> CInt -> CInt -> CInt -> CInt -> IO ()
foreign import ccall unsafe "bgmonad_blit"
  c_blit :: Ptr Word32 -> CInt -> CInt -> Ptr Word32 -> CInt -> CInt -> CInt -> IO ()
foreign import ccall unsafe "bgmonad_fill"
  c_fill :: Ptr Word32 -> CSize -> Word32 -> IO ()
foreign import ccall unsafe "&bgmonad_free"
  p_free :: FunPtr (Ptr Word32 -> IO ())
foreign import ccall unsafe "bgmonad_free"
  c_free :: Ptr a -> IO ()
foreign import ccall unsafe "bgmonad_shm_file" shmFile :: CSize -> IO CInt
foreign import ccall unsafe "bgmonad_mmap" shmMap :: CInt -> CSize -> IO (Ptr Word32)
foreign import ccall unsafe "bgmonad_munmap" shmUnmap :: Ptr Word32 -> CSize -> IO ()

-- | Something the renderer can sample: XRGB pixels (all the layers of an
-- output have the same size) or a plain colour.
data Layer
  = LayerImage !(ForeignPtr Word32)
  | LayerColor !Word32

-- | Load an image scaled to exactly @w@ x @h@ pixels. @bg@ shows through
-- transparent images and around the ones that do not fill the layer;
-- @unit@ is the size of an image pixel for 'Center'.
loadLayer :: FilePath -> Int -> Int -> Fit -> Word32 -> Double -> IO (Either String Layer)
loadLayer path w h fit bg unit =
  withCString path $ \cpath -> alloca $ \perr -> do
    poke perr nullPtr
    p <- c_load cpath (fromIntegral w) (fromIntegral h) mode bg (realToFrac unit) perr
    if p /= nullPtr
      then Right . LayerImage <$> newForeignPtr p_free p
      else do
        e <- peek perr
        msg <- if e == nullPtr then pure "cannot load the image" else peekCString e <* c_free e
        pure (Left msg)
  where
    mode = case fit of
      Cover -> 0
      Contain -> 1
      Stretch -> 2
      Center -> 3

-- | A blurred copy of a layer of @w@ x @h@ pixels.
blurLayer :: Layer -> Int -> Int -> Int -> IO (Maybe Layer)
blurLayer (LayerColor c) _ _ _ = pure (Just (LayerColor c))
blurLayer (LayerImage fp) w h radius = withForeignPtr fp $ \p -> do
  q <- c_blur p (fromIntegral w) (fromIntegral h) (fromIntegral radius)
  if q == nullPtr then pure Nothing else Just . LayerImage <$> newForeignPtr p_free q

-- | Release the pixels now instead of waiting for the garbage collector
-- (which has no idea how big they are). The layer must not be used again.
freeLayer :: Layer -> IO ()
freeLayer (LayerImage fp) = finalizeForeignPtr fp
freeLayer (LayerColor _) = pure ()

-- | Draw a frame: a weighted blend (weights out of 256) of layers of
-- @sw@ x @sh@ pixels, each sampled where the grid says. The grid has @gw@
-- columns of nodes, one node every @cell@ pixels, with the source
-- position of that pixel.
drawLayers :: Ptr Word32 -> Int -> Int -> Ptr CFloat -> Int -> Int -> [(Layer, Int)] -> Int -> Int -> Int -> IO ()
drawLayers dst dw dh grid gw cell layers sw sh threads = go layers []
  where
    go [] acc =
      let (ptrs, colors, weights) = unzip3 (reverse acc)
      in withArrayLen ptrs $ \n pp -> withArray colors $ \pc -> withArray weights $ \pw ->
           c_warp dst (fromIntegral dw) (fromIntegral dh) grid (fromIntegral gw) (fromIntegral cell)
                  pp pc pw (fromIntegral n) (fromIntegral sw) (fromIntegral sh) (fromIntegral threads)
    go ((LayerImage fp, w) : rest) acc = withForeignPtr fp $ \p -> go rest ((p, 0, fromIntegral w) : acc)
    go ((LayerColor c, w) : rest) acc = go rest ((nullPtr, c, fromIntegral w) : acc)

-- | Copy the @dw@ x @dh@ rectangle at @(ox, oy)@ of a layer @sw@ pixels wide.
copyLayer :: Ptr Word32 -> Int -> Int -> ForeignPtr Word32 -> Int -> Int -> Int -> IO ()
copyLayer dst dw dh fp sw ox oy = withForeignPtr fp $ \p ->
  c_blit dst (fromIntegral dw) (fromIntegral dh) p (fromIntegral sw) (fromIntegral ox) (fromIntegral oy)

fillPixels :: Ptr Word32 -> Int -> Word32 -> IO ()
fillPixels dst n = c_fill dst (fromIntegral n)

-- | @#rrggbb@ (or @#rgb@) to 0xffRRGGBB; black if it makes no sense.
parseColor :: String -> Word32
parseColor s = case map digitToInt (dropWhile (== '#') s) of
  _ | not (all isHexDigit (dropWhile (== '#') s)) -> 0xff000000
  [r, g, b] -> rgb (17 * r) (17 * g) (17 * b)
  (r1 : r2 : g1 : g2 : b1 : b2 : _) -> rgb (16 * r1 + r2) (16 * g1 + g2) (16 * b1 + b2)
  _ -> 0xff000000
  where
    rgb r g b = 0xff000000 + fromIntegral (r * 65536 + g * 256 + b)
