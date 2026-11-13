-- | The types a configuration is made of.
module Bgmonad.Core
  ( -- * Configuration
    Config (..)
  , Fit (..)
  , defaultConfig
    -- * Wallpapers
  , Wallpaper (..)
  , Pick (..)
  , Ctx (..)
    -- * Effects
  , Effect (..)
  , Frame (..)
  , Warp
  ) where

import Data.Time.LocalTime (LocalTime)

import Bgmonad.Rivermonad (Workspace)

-- | How an image is fitted to the screen.
data Fit
  = Cover    -- ^ fill the screen, cropping what does not fit (the usual choice)
  | Contain  -- ^ show the whole image, with 'bgColor' bars
  | Stretch  -- ^ fill the screen ignoring the aspect ratio
  | Center   -- ^ no scaling: one image pixel is one screen pixel
  deriving (Eq, Ord, Show)

data Config = Config
  { wallpaper   :: Wallpaper
    -- ^ what to show; see "Bgmonad.Wallpapers"
  , effects     :: [Effect]
    -- ^ how it moves and reacts; see "Bgmonad.Effects". @[]@ is a plain
    -- still wallpaper, which costs nothing once drawn.
  , fit         :: Fit
  , bgColor     :: String
    -- ^ @#rrggbb@: behind transparent or letterboxed images, and what is
    -- shown when an image cannot be loaded
  , transition  :: Double
    -- ^ seconds of crossfade when the wallpaper changes (0 = cut)
  , easing      :: Double
    -- ^ seconds effects take to follow rivermonad (workspace switches,
    -- windows opening): the time to cover about two thirds of the way
  , fps         :: Int
    -- ^ frames per second while something moves. The motion of the
    -- stock effects is slow and smooth: 20 looks the same as 60 and costs
    -- a third.
  , busyFps     :: Maybe Int
    -- ^ frames per second while the workspace shown by rivermonad has
    -- windows, when you would rather not spend much on a wallpaper you
    -- barely see: @Just 5@ moves it in slow motion, @Just 0@ holds it still
    -- until the workspace is empty again. 'Nothing' = the same as 'fps'.
  , renderScale :: Double
    -- ^ 1 draws every device pixel; 0.5 draws half the resolution (a
    -- quarter of the work) and lets the compositor scale it up, which is
    -- hard to tell apart on soft wallpapers. Needs wp_viewporter.
  , threads     :: Int
    -- ^ threads drawing a frame; 0 = one per core, up to 4
  , cacheSize   :: Int
    -- ^ decoded wallpapers kept in memory per output (at least the one on
    -- screen), so going back to a workspace's wallpaper is instant. Each
    -- takes 4 bytes per pixel, twice that with a blur effect.
  , outputs     :: [String]
    -- ^ outputs to manage (by name, e.g. @"DP-1"@); [] = all
  , namespace   :: String
    -- ^ layer-shell namespace, for compositor rules
  }

-- | A black screen; "Bgmonad.Config" has a populated 'Bgmonad.Config.def'.
defaultConfig :: Config
defaultConfig = Config
  { wallpaper = Wallpaper (\_ -> pure (PickColor "#000000"))
  , effects = []
  , fit = Cover
  , bgColor = "#000000"
  , transition = 1.2
  , easing = 0.35
  , fps = 20
  , busyFps = Nothing
  , renderScale = 1
  , threads = 0
  , cacheSize = 2
  , outputs = []
  , namespace = "wallpaper"
  }

-- | What ends up on an output.
data Pick
  = PickImage FilePath  -- ^ an image file (anything gdk-pixbuf loads)
  | PickColor String    -- ^ @#rrggbb@
  deriving (Eq, Ord, Show)

-- | What a wallpaper can depend on.
data Ctx = Ctx
  { ctxOutput    :: String           -- ^ the output's name, e.g. @"eDP-1"@
  , ctxWorkspace :: Maybe Workspace  -- ^ the workspace rivermonad shows there ('Nothing' without rivermonad)
  , ctxTime      :: LocalTime
  , ctxSeconds   :: Integer          -- ^ seconds since 1970
  , ctxStep      :: Int              -- ^ how many times @bgmonad next@ was run (minus @prev@)
  }

-- | A wallpaper is a rule that says what to show in a given situation. It
-- is asked again whenever the situation changes (rivermonad switched
-- workspace, @bgmonad next@...) and every few seconds; when the answer
-- changes, the new picture is loaded in the background and faded in.
newtype Wallpaper = Wallpaper { pickWallpaper :: Ctx -> IO Pick }

-- | The state of an output when a frame is drawn.
data Frame = Frame
  { frTime      :: Double  -- ^ seconds of animation so far (stops while paused)
  , frAspect    :: Double  -- ^ width / height of the output
  , frWorkspace :: Double
    -- ^ where the workspace shown on the output is in rivermonad's list,
    -- from 0 (first) to 1 (last). It glides when you switch (see 'easing').
  , frBusy      :: Double
    -- ^ 0 while the workspace is empty, 1 while it has windows (glides too)
  }

-- | A displacement field. Positions are in screen heights: (0, 0) is the
-- top left corner, (aspect, 1) the bottom right one. The pixel at @(x, y)@
-- shows the wallpaper at @(x + dx, y + dy)@, where @(dx, dy) = warp x y@.
type Warp = Double -> Double -> (Double, Double)

-- | Something that changes how the wallpaper is drawn. Effects are
-- combined with '<>' (or just listed in 'effects'): displacements, blur
-- and dimming add up.
data Effect = Effect
  { fxWarp       :: Maybe (Frame -> Warp)
    -- ^ the function is applied to the frame once and then to a few
    -- thousand points, so precompute what only depends on the frame
  , fxMargin     :: Double
    -- ^ the farthest the warp reaches, in screen heights: the wallpaper is
    -- scaled up this much beyond every edge so there is always picture to show
  , fxBlur       :: Frame -> Double   -- ^ 0 = sharp .. 1 = blurred
  , fxBlurRadius :: Double            -- ^ blur radius in screen heights (0 = this effect does not blur)
  , fxDim        :: Frame -> Double   -- ^ 0 = as is .. 1 = black
  , fxLive       :: Bool
    -- ^ True if it changes with 'frTime' and needs frames all the time;
    -- otherwise frames are only drawn when something else changes
  , fxReactive   :: Bool
    -- ^ True if it looks at 'frWorkspace' or 'frBusy': frames are drawn
    -- while those glide to their new values
  }

instance Semigroup Effect where
  a <> b = Effect
    { fxWarp = case (fxWarp a, fxWarp b) of
        (Just f, Just g) -> Just $ \fr ->
          let f' = f fr
              g' = g fr
          in \x y -> let (ax, ay) = f' x y
                         (bx, by) = g' x y
                     in (ax + bx, ay + by)
        (f, Nothing) -> f
        (Nothing, g) -> g
    , fxMargin = fxMargin a + fxMargin b
    , fxBlur = \fr -> min 1 (fxBlur a fr + fxBlur b fr)
    , fxBlurRadius = max (fxBlurRadius a) (fxBlurRadius b)
    , fxDim = \fr -> 1 - (1 - fxDim a fr) * (1 - fxDim b fr)
    , fxLive = fxLive a || fxLive b
    , fxReactive = fxReactive a || fxReactive b
    }

instance Monoid Effect where
  mempty = Effect Nothing 0 (const 0) 0 (const 0) False False
