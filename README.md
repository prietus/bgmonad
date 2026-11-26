# bgmonad

A living wallpaper for Wayland, written and configured in Haskell, in the
family of [rivermonad](https://github.com/prietus/rivermonad) and [waymobar](https://github.com/prietus/waymobar).

- **It moves.** The picture flows very slowly, like macOS's wallpapers:
  its lines bend and drift a few pixels, never repeating. No video and no
  sequence of images on disk: every frame is computed on the fly from the
  still image you already have.
- **It follows rivermonad.** A wallpaper per workspace, a wallpaper that
  slides as you change workspace, that blurs or dims behind windows and
  comes back when the workspace is empty, that slows down or stops while
  you work.
- **It is a Haskell file.** A wallpaper is a rule (`image`, `slideshow`,
  `byWorkspace`, `byHour`...) and an effect is a function; both compose,
  and you can write your own in a few lines.
- **It is frugal.** Frames are only drawn when the compositor wants them
  (nothing while a fullscreen window covers the wallpaper), never more
  often than `fps`, and a wallpaper without effects costs nothing once
  drawn.

It works on any compositor with wlr-layer-shell (river, sway, Hyprland,
niri...); the rivermonad features need rivermonad.

## Build

```sh
sudo pacman -S gdk-pixbuf2          # libgdk-pixbuf-2.0-dev on Debian; plus ghc and cabal-install
cd bgmonad
cabal install --lib lib:bgmonad     # so ghc can compile your config
cabal install exe:bgmonad
```

Start it from your rivermonad config, instead of swaybg:

```haskell
startupHook = spawnOnce "bgmonad"
```

## Configuration

Without a configuration `bgmonad` shows `~/.config/wallpaper` (make it a
link to an image) with the `flow` effect. To change it, write
`~/.config/bgmonad/bgmonad.hs` (see [examples/bgmonad.hs](examples/bgmonad.hs), or
the one I use in [examples/dotfiles](examples/dotfiles/config/bgmonad/bgmonad.hs)):

```haskell
import Bgmonad

main :: IO ()
main = bgmonad def
  { wallpaper = byWorkspace [ ("web", image "~/Pictures/sea.jpg") ]
                  (shuffled 30 ["~/Pictures/wallpapers"])
  , effects = [ flow, parallax 0.04, dimWhenBusy 0.25 ]
  }
```

Like xmonad, bgmonad compiles this file when it changes. `bgmonad
--recompile` checks it; running `bgmonad` again replaces the one that is
running, once the new one has its first frame ready, so there is no flash.

### A plain wallpaper

If all you want is a picture on the desktop, like feh's or swaybg's, start
from `still` instead of `def`:

```haskell
main = bgmonad still { wallpaper = image "~/Pictures/sea.jpg" }
```

It is drawn once, at full resolution, and uses no CPU afterwards. The
wallpaper rules below still work (a slideshow, one per workspace...) and
changes fade in. Without a configuration at all: `bgmonad --still`
(`~/.config/wallpaper`) or `bgmonad --still FILE`.

### Wallpapers

| | |
|---|---|
| `image "~/Pictures/a.png"` | An image (whatever gdk-pixbuf loads). Links are followed and the file is watched: point the link elsewhere or overwrite the file and the wallpaper changes. |
| `solid "#1e1e2e"` | A colour. |
| `slideshow 30 ["~/Pictures/walls", "~/a.png"]` | Images and directories of images, 30 minutes each. |
| `shuffled 30 [...]` | The same in random order, without repeats until all were shown. |
| `byWorkspace [("web", w1), ("code", w2)] rest` | By rivermonad workspace. |
| `byOutput [("HDMI-A-1", w)] rest` | By output. |
| `byHour [(7, day), (20.5, night)]` | By time of day. |

They nest freely. A wallpaper is just `Ctx -> IO Pick` (see
`Bgmonad.Core`), so anything else is a few lines of Haskell: the weather,
the battery, the day of the week.

When the wallpaper changes it is loaded in the background and crossfaded
in `transition` seconds.

### Effects

| | |
|---|---|
| `flow` | The slow flow. `flowWith defFlow { flowAmount = 0.012, flowSize = 0.7, flowSpeed = 1.5 }` to tune how far, how fine and how fast. |
| `drift` | A slow zoom and pan (Ken Burns). `driftWith amount period`. |
| `sway 0.04 60` | The wallpaper glides 4% of the screen height to each side and back, once a minute: the slide of `parallax`, on its own. |
| `parallax 0.04` | The wallpaper slides 4% of the screen height between the first and the last workspace. |
| `blurWhenBusy 0.02` | Blurred behind windows, sharp on an empty workspace. |
| `dimWhenBusy 0.3` | 30% darker behind windows. |
| `dim 0.2`, `blur 0.01` | Always. |
| `warp margin f` | Your own displacement field. |

Effects are a monoid: list them in `effects` or combine them with `<>`.
An effect is a record of plain functions of the frame (time, where the
workspace is, whether it has windows); `Bgmonad.Effects` is short and
meant to be read. For example, ripples from the centre:

```haskell
ripple :: Effect
ripple = warp 0.004 $ \fr x y ->
  let cx = x - frAspect fr / 2
      cy = y - 0.5
      r  = sqrt (cx * cx + cy * cy) + 1e-6
      a  = 0.004 * sin (40 * r - 2 * frTime fr)
  in (a * cx / r, a * cy / r)
```

### What it costs

A still wallpaper (`effects = []`) costs nothing after the first frame.
A moving one redraws the whole screen `fps` times per second, in bgmonad
and in the compositor, which has to take the new frame in. On a 3840x2160
output at 20 fps that was about a fifth of a core for bgmonad and twice as
much for the compositor in a virtual machine. Three knobs:

- `renderScale = 0.5` draws half the resolution and lets the compositor
  scale it up: a quarter of the work on both sides. On soft, abstract
  wallpapers it is very hard to tell apart; photographs lose some detail.
- `busyFps = Just 5` (or `Just 0`) moves the wallpaper in slow motion (or
  not at all) while the workspace has windows and you can barely see it.
- `fps`: the stock effects move a fraction of a pixel per frame, so 20 is
  as smooth as 60.
- `liveFps = Just 8` draws the motion that never stops (`flow`, `drift`)
  at 8 frames per second, while workspace switches and crossfades keep
  the full `fps`.

`bgmonad pause` stops the motion altogether.

Memory: a decoded wallpaper takes 4 bytes per pixel of the output (33 MB
at 4K), twice that with a blur effect; `cacheSize` of them are kept per
output (2 by default: going back to the previous one is instant).

## Wallpapers

[wallpapers/](wallpapers) has ten abstract 4K wallpapers made for bgmonad,
in the colours of [Catppuccin](https://catppuccin.com) Mocha. Five are
full of curves: `ribbons`, `contours`, `silk`, `aurora` and `ember`. Five
are minimal, a line or two on the dark base: `line`, `strands`, `arcs`,
`dunes` and `ring`. They are all long curved lines over soft gradients,
which is what a slow flow shows best: a thin line that bends a few pixels
is seen, a noisy texture is not.

They are generated by [tools/gen-wallpapers.c](tools/gen-wallpapers.c)
(no third-party images) and are under the same license as the code. Give
it another seed for other shapes in the same style.

## Commands

```
bgmonad next | prev          step through slideshows
bgmonad pause | resume | toggle
bgmonad set FILE             show an image, whatever the configuration says
bgmonad unset                back to the configuration
bgmonad status               what every output shows
bgmonad quit
```

Bind them in rivermonad:

```haskell
, ("M-w",   spawn "bgmonad next")
, ("M-S-w", spawn "bgmonad toggle")
```

## How it works

- The Wayland protocol is implemented in pure Haskell (the same code as
  rivermonad and waymobar, generated from `protocol/*.xml` by
  `tools/regen.sh`). The wallpaper is a layer-shell surface in the
  background layer of every output, drawn into shared memory.
- Every frame, the effects are evaluated on a coarse grid (a node every
  ~36 pixels): for each node, where in the picture that pixel comes from.
  A small C kernel (`cbits/warp.c`, NEON on aarch64) interpolates the grid
  and samples the picture bilinearly, blending up to four layers: the
  wallpaper, its blurred copy, and the same two of the wallpaper it is
  replacing. That is all a crossfade, a blur or a dim is: a weight.
- rivermonad's state arrives through its IPC socket (`EventStream`), like
  in waymobar. Positions glide towards it with an exponential ease.
- The animation clock only advances while frames are drawn, so the
  picture carries on from where you last saw it.

## License

BSD-3-Clause.
