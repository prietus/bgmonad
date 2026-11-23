-- | The stock configurations.
module Bgmonad.Config
  ( def
  , still
  ) where

import Bgmonad.Core
import Bgmonad.Effects
import Bgmonad.Wallpapers

-- | The image @~/.config/wallpaper@ (make it a link to the one you like),
-- flowing slowly.
def :: Config
def = defaultConfig
  { wallpaper = image "~/.config/wallpaper"
  , effects = [ flow ]
  }

-- | A plain wallpaper, like feh's or swaybg's: the same image as 'def',
-- drawn once at full resolution and left alone. Nothing moves and nothing
-- follows rivermonad, so it uses no CPU; a change of wallpaper (a
-- slideshow, @bgmonad set@...) still fades in.
--
-- > main = bgmonad still { wallpaper = image "~/Pictures/sea.jpg" }
still :: Config
still = def { effects = [] }
