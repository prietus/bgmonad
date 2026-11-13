-- | The @bgmonad@ executable: like xmonad, it builds and runs
-- @~/.config/bgmonad/bgmonad.hs@ when there is one, and otherwise shows
-- the default wallpaper. With a command it talks to the running bgmonad.
module Bgmonad.Main
  ( launch
  ) where

import Control.Monad (unless, when)
import System.Directory (doesFileExist, makeAbsolute)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (executeFile)

import Bgmonad.Config (def)
import Bgmonad.Control
import Bgmonad.Engine (runBgmonad)
import Bgmonad.Recompile
import Bgmonad.Wallpapers (expandHome)

launch :: IO ()
launch = do
  args <- getArgs
  case args of
    ["--recompile"] -> do
      ok <- recompile True
      if ok then exitSuccess else exitFailure
    ["--default"] -> runBgmonad def
    ["--help"] -> usage >> exitSuccess
    ["--version"] -> putStrLn "bgmonad 0.1.0" >> exitSuccess
    [] -> do
      d <- getDirs
      have <- doesFileExist (cfgSource d)
      if not have
        then runBgmonad def
        else do
          ok <- recompile False
          bin <- doesFileExist (cfgBinary d)
          when (not ok) $ hPutStrLn stderr "bgmonad: using the last working build of the configuration"
          if bin
            then executeFile (cfgBinary d) False [] Nothing
            else do
              hPutStrLn stderr "bgmonad: no build of the configuration, showing the default wallpaper"
              runBgmonad def
    _ | Just cmd <- parseCommand args -> do
          ws <- case cmd of
            Set f -> (\p -> ["set", p]) <$> (makeAbsolute =<< expandHome f)
            _ -> pure args
          sendCommand ws >>= \case
            Nothing -> hPutStrLn stderr "bgmonad: not running on this display" >> exitFailure
            Just reply -> do
              unless (reply == "ok") (putStrLn reply)
              when (take 6 reply == "error:") exitFailure
      | otherwise -> usage >> exitFailure
  where
    usage = putStr $ unlines $
      [ "Usage: bgmonad [--recompile | --default | --version | COMMAND]"
      , ""
      , "Runs ~/.config/bgmonad/bgmonad.hs (rebuilt when it changes), or the"
      , "default wallpaper when there is none. Running it again replaces the"
      , "bgmonad that is already there, without a flash."
      , ""
      , "  --recompile  build the configuration and exit"
      , "  --default    ignore the configuration"
      , ""
      , "Commands for the running bgmonad:"
      ] ++ commandHelp
