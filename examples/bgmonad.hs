-- ~/.config/bgmonad/bgmonad.hs
--
-- bgmonad rebuilds this file when it changes (like xmonad). Check it with
-- `bgmonad --recompile`; run `bgmonad` again to replace the running one.
import Bgmonad

main :: IO ()
main = bgmonad def
  { wallpaper =
      -- a wallpaper of its own for the "web" workspace; everywhere else,
      -- a random picture every half hour by day and a fixed one at night
      byWorkspace [ ("web", image "~/Pictures/sea.jpg") ] $
        byHour
          [ (8,  shuffled 30 ["~/Pictures/wallpapers"])
          , (21, image "~/Pictures/night.png")
          ]
  , effects =
      [ flow               -- the picture flows slowly, like macOS's
      , parallax 0.04      -- and slides a little as you change workspace
      , dimWhenBusy 0.25   -- darker behind windows
      ]
  , transition = 1.5       -- seconds of crossfade between pictures
  }
