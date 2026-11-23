-- ~/.config/bgmonad/bgmonad.hs: the author's configuration (see README.md)
--
-- bgmonad rebuilds this file when it changes. After editing it, check it
-- with `bgmonad --recompile` and run `bgmonad` again: the new wallpaper
-- replaces the running one.
import Bgmonad

main :: IO ()
main = bgmonad def
  { -- ~/.config/wallpaper is a link to the image; change it with
    --   ln -sfn /path/to/image ~/.config/wallpaper
    -- and the wallpaper follows within a few seconds (with a crossfade).
    wallpaper = image "~/.config/wallpaper"
    -- other ideas:
    --   shuffled 30 ["~/Pictures"]                      a random one every 30 minutes
    --   byWorkspace [ ("web", image "~/Pictures/x.png") ] (image "~/.config/wallpaper")
    --   byHour [ (8, image "~/Pictures/day.png"), (20, image "~/Pictures/night.png") ]

  , effects =
      [ flow             -- the lines of the picture drift slowly, like macOS's
      , parallax 0.09    -- and it slides a little as you change workspace
      , sway 0.04 60     -- and glides sideways by itself: this far, back in this many seconds
      -- , dimWhenBusy 0.25    -- darker behind windows
      -- , blurWhenBusy 0.02   -- out of focus behind windows
      ]
    -- more or less motion:
    --   flowWith defFlow { flowAmount = 0.012, flowSize = 0.8, flowSpeed = 1.5 }

    -- Half the resolution, a quarter of the work (for bgmonad and for
    -- river); on this kind of wallpaper it looks the same. 1 = every pixel.
  , renderScale = 0.5
    -- As little CPU as possible (this is a virtual machine: every frame is
    -- expensive for river too):
  , busyFps = Just 0     -- still while the workspace has windows: costs nothing
  , liveFps = Just 8     -- on an empty workspace the flow needs few frames...
  , fps = 20             -- ...but sliding between workspaces stays smooth
  }
