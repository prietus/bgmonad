-- | The stock effects. They are ordinary values of 'Effect': read this
-- module to write your own.
module Bgmonad.Effects
  ( -- * Motion
    flow
  , Flow (..)
  , defFlow
  , flowWith
  , drift
  , driftWith
  , sway
    -- * Reacting to rivermonad
  , parallax
  , blurWhenBusy
  , dimWhenBusy
    -- * Building blocks
  , warp
  , dim
  , blur
  , whenBusy
  ) where

import Bgmonad.Core

-- | The wallpaper flows very slowly, like macOS's: its lines bend and
-- drift a few pixels back and forth, never repeating.
flow :: Effect
flow = flowWith defFlow

data Flow = Flow
  { flowAmount :: Double
    -- ^ how far a point of the picture travels, in screen heights (0.01 is
    -- 1% of the screen: lively; 0.004 is barely there)
  , flowSize   :: Double
    -- ^ size of the swirls, in screen heights: smaller bends finer details
  , flowSpeed  :: Double
    -- ^ 1 = a swirl takes 10 to 30 seconds
  } deriving (Eq, Show)

defFlow :: Flow
defFlow = Flow { flowAmount = 0.007, flowSize = 1, flowSpeed = 1 }

-- | The field is a sum of a few shear waves travelling in different
-- directions with periods that share no factor. Every wave moves the
-- picture across its direction of travel, so nothing is squeezed: it
-- looks like a liquid being stirred, not like a flag.
flowWith :: Flow -> Effect
flowWith (Flow amount size speed) = mempty
  { fxWarp = Just field
  , fxMargin = amount
  , fxLive = True
  }
  where
    -- (direction in turns, waves per screen height, period in seconds, weight, phase)
    waves :: [(Double, Double, Double, Double, Double)]
    waves =
      [ (0.07, 0.55, 29, 1.00, 0.0)
      , (0.31, 0.85, 19, 0.75, 1.3)
      , (0.58, 1.30, 13, 0.50, 2.9)
      , (0.83, 1.90, 11, 0.30, 4.1)
      ]
    total = sum [ w | (_, _, _, w, _) <- waves ]
    field fr =
      let t = frTime fr * speed
          ws = [ (cos a, sin a, 2 * pi * f / size, amount * w / total, 2 * pi * t / p + ph)
               | (turns, f, p, w, ph) <- waves, let a = 2 * pi * turns ]
      in \x y ->
           let go (ax, ay) (nx, ny, k, amp, ph) =
                 let s = amp * sin (k * (x * nx + y * ny) + ph)
                 in (ax - ny * s, ay + nx * s)
           in foldl go (0, 0) ws

-- | A slow zoom and pan (the Ken Burns effect), back and forth.
drift :: Effect
drift = driftWith 0.02 90

-- | @driftWith amount period@: move up to @amount@ screen heights, taking
-- @period@ seconds to come back.
driftWith :: Double -> Double -> Effect
driftWith amount period = mempty
  { fxWarp = Just field
  , fxMargin = amount
  , fxLive = True
  }
  where
    field fr =
      let ph = 2 * pi * frTime fr / period
          cx = frAspect fr / 2
          -- zoom between 0 and amount at the nearest edge, pan with the rest
          z = amount * (1 - cos ph) / 2
          px = (amount - z) * sin (ph * 2) * 0.7
          py = (amount - z) * sin (ph + 1) * 0.7
      in \x y -> ((x - cx) * (-z / max cx 0.5) + px, (y - 0.5) * (-z / 0.5) + py)

-- | The wallpaper glides sideways and back, on its own and forever: the
-- slide of 'parallax' without changing workspace. @sway 0.04 60@ moves it
-- 4% of the screen height to each side, taking a minute to come back.
sway :: Double -> Double -> Effect
sway amount period = mempty
  { fxWarp = Just (\fr -> let dx = amount * sin (2 * pi * frTime fr / period) in \_ _ -> (dx, 0))
  , fxMargin = abs amount
  , fxLive = True
  }

-- | The wallpaper slides sideways as you move through rivermonad's
-- workspaces, like the home screen of a phone: @parallax 0.04@ moves it
-- 4% of the screen height from the first workspace to the last.
parallax :: Double -> Effect
parallax amount = mempty
  { fxWarp = Just (\fr -> let dx = amount * (2 * frWorkspace fr - 1) in \_ _ -> (dx, 0))
  , fxMargin = abs amount
  , fxReactive = True
  }

-- | Blur the wallpaper while the workspace has windows and bring it back
-- into focus when it is empty. The radius is in screen heights (0.02 is a
-- soft blur, 0.05 frosted glass).
blurWhenBusy :: Double -> Effect
blurWhenBusy radius = whenBusy (blur radius)

-- | Darken the wallpaper while the workspace has windows:
-- @dimWhenBusy 0.3@ takes away 30% of the light.
dimWhenBusy :: Double -> Effect
dimWhenBusy amount = whenBusy (dim amount)

-- | A custom displacement field that reaches at most @margin@ screen
-- heights. It is drawn all the time (it may use 'frTime') and follows
-- rivermonad (it may use 'frWorkspace' and 'frBusy'). See 'Warp'.
--
-- > ripple = warp 0.004 $ \fr -> \x y ->
-- >   let r = sqrt ((x - frAspect fr / 2) ^ 2 + (y - 0.5) ^ 2) + 1e-6
-- >       a = 0.004 * sin (40 * r - 2 * frTime fr)
-- >   in (a * (x - frAspect fr / 2) / r, a * (y - 0.5) / r)
warp :: Double -> (Frame -> Warp) -> Effect
warp margin f = mempty { fxWarp = Just f, fxMargin = margin, fxLive = True, fxReactive = True }

-- | Always darker.
dim :: Double -> Effect
dim amount = mempty { fxDim = const amount }

-- | Always blurred.
blur :: Double -> Effect
blur radius = mempty { fxBlur = const 1, fxBlurRadius = radius }

-- | Apply the blur and dimming of an effect in proportion to 'frBusy'.
whenBusy :: Effect -> Effect
whenBusy e = e
  { fxBlur = \fr -> frBusy fr * fxBlur e fr
  , fxDim = \fr -> frBusy fr * fxDim e fr
  , fxReactive = True
  }
