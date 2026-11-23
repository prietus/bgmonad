-- | bgmonad: a wallpaper for Wayland written and configured in Haskell,
-- that moves and follows rivermonad.
--
-- > import Bgmonad
-- >
-- > main :: IO ()
-- > main = bgmonad def
-- >   { wallpaper = byWorkspace [ ("web", image "~/Pictures/sea.jpg") ]
-- >                   (shuffled 30 ["~/Pictures/wallpapers"])
-- >   , effects = [ flow, parallax 0.04, dimWhenBusy 0.25 ]
-- >   }
module Bgmonad
  ( bgmonad
  , def
  , still
  , module Bgmonad.Core
  , module Bgmonad.Wallpapers
  , module Bgmonad.Effects
  , R.Workspace (..)
  ) where

import Bgmonad.Config (def, still)
import Bgmonad.Core
import Bgmonad.Effects
import Bgmonad.Engine (runBgmonad)
import qualified Bgmonad.Rivermonad as R
import Bgmonad.Wallpapers

-- | Show the wallpaper on every output, replacing a bgmonad that is
-- already running.
bgmonad :: Config -> IO ()
bgmonad = runBgmonad
