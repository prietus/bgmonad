-- | The default configuration.
module Bgmonad.Config
  ( def
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
