{-# LANGUAGE OverloadedStrings #-}
-- | The Wayland side: one layer-shell surface in the background layer of
-- every output, drawn into shared memory buffers.
--
-- Threads: a reader decodes compositor events, loaders decode pictures,
-- the rivermonad client and the control socket listen; everything ends up
-- in one queue consumed by the main loop, which owns all the state and
-- draws the outputs that need a frame once the queue is empty.
--
-- Frames are only drawn when the compositor asks for them (frame
-- callbacks), so a wallpaper hidden behind a fullscreen window costs
-- nothing, and never more often than 'fps'.
module Bgmonad.Engine
  ( runBgmonad
  ) where

import Control.Concurrent (forkIO, getNumCapabilities, threadDelay)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import Data.Binary.Get (runGetOrFail)
import Data.Bits ((.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (find, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.LocalTime (getZonedTime, zonedTimeToLocalTime)
import Data.Word (Word32)
import Foreign.C.Types (CFloat)
import Foreign.ForeignPtr
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (pokeElemOff)
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getNumProcessors, setNumCapabilities)
import System.Directory (getModificationTime)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Posix.Types (Fd (..))

import Bgmonad.Control
import Bgmonad.Core
import Bgmonad.Image
import qualified Bgmonad.Rivermonad as R
import Bgmonad.Wallpapers (expandHome)
import Bgmonad.Wayland.Protocol
import Bgmonad.Wayland.Wire

data Msg
  = MsgEvent Event
  | MsgClosed String
  | MsgRiver                                   -- ^ rivermonad's state changed
  | MsgTick                                    -- ^ time passes: ask the wallpaper again
  | MsgWake ObjectId                           -- ^ time for the next frame of a surface
  | MsgLoaded ObjectId Int Picture             -- ^ surface, request number, the picture
  | MsgControl Command (String -> IO ())
  | MsgTakeOver                                -- ^ replace a previous bgmonad even with nothing on screen

data Globals = Globals
  { gCompositor :: Maybe (ObjectId, Word32)  -- ^ object and bound version
  , gShm        :: Maybe ObjectId
  , gLayerShell :: Maybe ObjectId
  , gFractional :: Maybe ObjectId
  , gViewporter :: Maybe ObjectId
  }

data Out = Out
  { oName   :: Maybe String
  , oScale  :: Int
  , oDone   :: Bool
  , oGlobal :: Word32
  , oBg     :: Maybe ObjectId   -- ^ its wallpaper's wl_surface
  }

data Buffer = Buffer
  { bufObj  :: ObjectId
  , bufPtr  :: Ptr Word32
  , bufSize :: Int
  , bufW    :: Int
  , bufH    :: Int
  , bufBusy :: Bool
  }

-- | What identifies a decoded picture: what was picked and, for files,
-- when the file was last written (so overwriting it shows the new one).
type Key = (Pick, Maybe UTCTime)

data Picture = Picture
  { picKey   :: Key
  , picSharp :: Layer
  , picBlur  :: Maybe Layer
  }

-- | The sizes everything is drawn at, which follow from the surface's
-- size and scale.
data Geom = Geom
  { gPw, gPh :: Int               -- ^ the buffer, in pixels
  , gMargin  :: Int               -- ^ extra pixels of picture beyond every edge
  , gCell    :: Int               -- ^ pixels between nodes of the warp grid
  , gGw, gGh :: Int               -- ^ nodes of the grid
  , gGrid    :: ForeignPtr CFloat
  , gUnit    :: Double            -- ^ buffer pixels per device pixel
  , gInt     :: Maybe Int         -- ^ the integer buffer scale, when not using a viewport
  }

-- | The picture layers are this big.
gSw, gSh :: Geom -> Int
gSw g = gPw g + 2 * gMargin g
gSh g = gPh g + 2 * gMargin g

-- | The wallpaper of an output.
data Bg = Bg
  { bSurface    :: ObjectId
  , bLayer      :: ObjectId
  , bFrac       :: Maybe ObjectId
  , bViewport   :: Maybe ObjectId
  , bOutput     :: ObjectId
  , bName       :: String
  , bW, bH      :: Int            -- ^ logical size
  , bConfigured :: Bool
  , bScale120   :: Int            -- ^ fractional scale (120 = 1), 0 if unknown
  , bBufScale   :: Int            -- ^ preferred integer buffer scale, 0 if unknown
  , bBuffers    :: [Buffer]
  , bGeom       :: Maybe Geom
  , bDirty      :: Bool           -- ^ wants a frame
  , bFrameCb    :: Maybe ObjectId -- ^ waiting for the compositor to want one
  , bNextAt     :: Double         -- ^ not before this time (the 'fps' cap)
  , bWake       :: Bool           -- ^ a MsgWake is on its way
  , bTarget     :: Maybe Key      -- ^ what should be on screen
  , bRequest    :: Int            -- ^ number of the last load request
  , bCurrent    :: Maybe Picture  -- ^ what is on screen
  , bFade       :: Maybe (Picture, Double)  -- ^ what it is replacing, since when
  , bCache      :: [Picture]      -- ^ most recently shown first; has bCurrent and bFade
  , bPos        :: Double         -- ^ 'frWorkspace'
  , bPosTo      :: Double
  , bBusy       :: Double         -- ^ 'frBusy'
  , bBusyTo     :: Double
  , bEaseAt     :: Double         -- ^ when bPos and bBusy were last advanced
  , bSeen       :: Bool           -- ^ has heard from rivermonad
  , bClock      :: Double         -- ^ 'frTime': seconds of animation drawn so far
  , bClockAt    :: Double         -- ^ when it was last advanced
  , bWorkspace  :: Maybe R.Workspace
  }

data S = S
  { sGlobals  :: Globals
  , sOutputs  :: M.Map ObjectId Out
  , sBgs      :: M.Map ObjectId Bg      -- ^ by wl_surface
  , sReady    :: Bool                   -- ^ the initial roundtrip is done
  , sStep     :: Int                    -- ^ @bgmonad next@ minus @bgmonad prev@
  , sOverride :: Maybe FilePath         -- ^ @bgmonad set@
  , sPaused   :: Bool                   -- ^ @bgmonad pause@
  , sShown    :: Bool                   -- ^ a first frame is on screen
  }

data Env = Env
  { eConn    :: Conn
  , eReg     :: ObjectId
  , eCfg     :: Config
  , eEffect  :: Effect          -- ^ all of 'effects' together
  , eThreads :: Int
  , eState   :: IORef S
  , eSync    :: ObjectId
  , ePush    :: Msg -> IO ()
  , eRiver   :: R.RivermonadClient
  }

-- | Show the wallpaper until the compositor goes away (or @bgmonad quit@).
-- A bgmonad already running on the display is replaced once this one has
-- its first frame on screen.
runBgmonad :: Config -> IO ()
runBgmonad cfg = do
  q <- newTQueueIO
  let push = atomically . writeTQueue q
  cores <- getNumProcessors
  caps <- getNumCapabilities
  when (caps < 2) $ setNumCapabilities (min 4 (max 2 cores))
  client <- R.startClient (push MsgRiver)

  conn <- connect `E.catch` \(e :: E.SomeException) -> do
    hPutStrLn stderr ("bgmonad: cannot connect to the Wayland compositor: " ++ show e)
    exitFailure
  reg <- wlDisplayGetRegistry conn displayObject
  sync <- wlDisplaySync conn displayObject
  flush conn
  _ <- forkIO (reader conn push)
  _ <- forkIO $ forever (threadDelay 5000000 >> push MsgTick)
  -- take over from a previous instance even if nothing can be drawn
  _ <- forkIO (threadDelay 10000000 >> push MsgTakeOver)

  st <- newIORef S
    { sGlobals = Globals Nothing Nothing Nothing Nothing Nothing
    , sOutputs = M.empty, sBgs = M.empty, sReady = False
    , sStep = 0, sOverride = Nothing, sPaused = False, sShown = False }
  let env = Env
        { eConn = conn, eReg = reg, eCfg = cfg, eEffect = mconcat (effects cfg)
        , eThreads = if threads cfg > 0 then threads cfg else max 1 (min 4 cores)
        , eState = st, eSync = sync, ePush = push, eRiver = client }
      loop = do
        first <- atomically (readTQueue q)
        rest <- atomically (flushTQueue q)
        quit <- or <$> mapM (handle env) (first : rest)
        unless quit $ do
          drawDue env
          flush conn
          loop
  loop

reader :: Conn -> (Msg -> IO ()) -> IO ()
reader conn push = do
  r <- E.try $ forever $ do
    msgs <- readMessages conn
    forM_ msgs $ \m -> do
      iface <- lookupInterface conn (rawSender m)
      case iface >>= \i -> decodeEvent i (rawSender m) (rawOpcode m) of
        Nothing -> pure ()
        Just g -> case runGetOrFail g (BL.fromStrict (rawBody m)) of
          Left _ -> hPutStrLn stderr ("bgmonad: cannot decode event from " ++ show (rawSender m))
          Right (_, _, (ev, news)) -> do
            mapM_ (uncurry (registerObject conn)) news
            push (MsgEvent ev)
  case r of
    Left e -> push (MsgClosed (show (e :: E.SomeException)))
    Right () -> pure ()

------------------------------------------------------------------------------
-- Messages

-- | Handle one message; True means quit.
handle :: Env -> Msg -> IO Bool
handle env = \case
  MsgClosed why -> do
    hPutStrLn stderr ("bgmonad: " ++ why)
    pure True
  MsgEvent ev -> event env ev >> pure False
  MsgRiver -> follow env >> retarget env >> pure False
  MsgTick -> retarget env >> pure False
  MsgWake surf -> updBg env surf (\b -> b { bWake = False }) >> pure False
  MsgLoaded surf req pic -> do
    bg <- M.lookup surf . sBgs <$> readIORef st
    case bg of
      Just b | bRequest b == req -> install env b pic
      _ -> freePicture pic
    pure False
  MsgControl cmd reply -> control cmd reply
  MsgTakeOver -> do
    shown <- sShown <$> readIORef st
    unless shown $ do
      modifyIORef' st (\s -> s { sShown = True })
      takeOver env
    pure False
  where
    st = eState env
    ok reply = reply "ok" >> pure False
    control :: Command -> (String -> IO ()) -> IO Bool
    control cmd reply = case cmd of
      Next -> modifyIORef' st (\s -> s { sStep = sStep s + 1 }) >> retarget env >> ok reply
      Prev -> modifyIORef' st (\s -> s { sStep = sStep s - 1 }) >> retarget env >> ok reply
      Pause -> setPaused True >> ok reply
      Resume -> setPaused False >> ok reply
      Toggle -> do
        p <- sPaused <$> readIORef st
        setPaused (not p)
        reply (if p then "resumed" else "paused")
        pure False
      Set f -> modifyIORef' st (\s -> s { sOverride = Just f }) >> retarget env >> ok reply
      Unset -> modifyIORef' st (\s -> s { sOverride = Nothing }) >> retarget env >> ok reply
      Status -> do
        s <- readIORef st
        let line b = bName b ++ ": " ++ maybe "nothing yet" (showPick . fst . picKey) (bCurrent b)
              ++ (if fmap picKey (bCurrent b) /= bTarget b then " (loading " ++ maybe "?" (showPick . fst) (bTarget b) ++ ")" else "")
            showPick (PickImage f) = f
            showPick (PickColor c) = c
        reply (unlines (map line (M.elems (sBgs s)) ++ [ "paused" | sPaused s ]))
        pure False
      Quit -> reply "ok" >> pure True
    setPaused p = modifyIORef' st $ \s -> s
      { sPaused = p, sBgs = M.map (\b -> b { bDirty = True }) (sBgs s) }

event :: Env -> Event -> IO ()
event env ev = case ev of
  EvWlDisplay _ (WlDisplayError obj code msg) -> do
    hPutStrLn stderr ("bgmonad: protocol error on " ++ show obj ++ " (" ++ show code ++ "): " ++ BC.unpack msg)
    exitFailure
  EvWlRegistry _ (WlRegistryGlobal name iface ver) -> global env name iface ver
  EvWlRegistry _ (WlRegistryGlobalRemove name) -> do
    outs <- sOutputs <$> readIORef st
    forM_ [ o | (o, out) <- M.toList outs, oGlobal out == name ] $ \o -> do
      bgs <- sBgs <$> readIORef st
      mapM_ (destroyBg env) [ b | b <- M.elems bgs, bOutput b == o ]
      modifyIORef' st $ \s -> s { sOutputs = M.delete o (sOutputs s) }
      wlOutputRelease c o
  EvWlCallback cb (WlCallbackDone _)
    | cb == eSync env -> do
        s <- readIORef st
        let g = sGlobals s
        when (isNothing (gLayerShell g)) $ do
          hPutStrLn stderr "bgmonad: the compositor does not support wlr-layer-shell"
          exitFailure
        when (isNothing (gCompositor g) || isNothing (gShm g)) $ do
          hPutStrLn stderr "bgmonad: the compositor lacks wl_compositor or wl_shm"
          exitFailure
        writeIORef st s { sReady = True }
        createBgs env
    | otherwise -> do
        forgetObject c cb
        modifyIORef' st $ \s -> s { sBgs = M.map (\b -> if bFrameCb b == Just cb then b { bFrameCb = Nothing } else b) (sBgs s) }
  EvWlOutput o oe -> do
    let upd f = modifyIORef' st $ \s -> s { sOutputs = M.adjust f o (sOutputs s) }
    case oe of
      WlOutputName n -> upd (\out -> out { oName = Just (utf8 n) })
      WlOutputScale sc -> do
        upd (\out -> out { oScale = fromIntegral sc })
        bgs <- sBgs <$> readIORef st
        forM_ [ b | b <- M.elems bgs, bOutput b == o ] (resize env . bSurface)
      WlOutputDone -> upd (\out -> out { oDone = True }) >> createBgs env
      _ -> pure ()
  EvZwlrLayerSurfaceV1 ls lse -> do
    bgs <- sBgs <$> readIORef st
    forM_ (find ((== ls) . bLayer) (M.elems bgs)) $ \b -> case lse of
      ZwlrLayerSurfaceV1Configure serial w h -> do
        zwlrLayerSurfaceV1AckConfigure c ls serial
        updBg env (bSurface b) $ \x -> x
          { bW = max 1 (fromIntegral w), bH = max 1 (fromIntegral h), bConfigured = True }
        resize env (bSurface b)
      ZwlrLayerSurfaceV1Closed -> destroyBg env b
  EvWpFractionalScaleV1 fs (WpFractionalScaleV1PreferredScale sc) -> do
    bgs <- sBgs <$> readIORef st
    forM_ [ b | b <- M.elems bgs, bFrac b == Just fs ] $ \b -> do
      updBg env (bSurface b) (\x -> x { bScale120 = fromIntegral sc })
      resize env (bSurface b)
  EvWlSurface surf (WlSurfacePreferredBufferScale sc) -> do
    updBg env surf (\x -> x { bBufScale = fromIntegral sc })
    resize env surf
  EvWlBuffer buf WlBufferRelease -> do
    bgs <- sBgs <$> readIORef st
    forM_ (M.elems bgs) $ \b -> forM_ (find ((== buf) . bufObj) (bBuffers b)) $ \x ->
      if bDirty b
        then updBg env (bSurface b) $ \y -> y { bBuffers = map (\z -> if bufObj z == buf then z { bufBusy = False } else z) (bBuffers y) }
        else do
          -- nothing moves: a still wallpaper only needs the buffer on screen
          freeBuffer c x
          updBg env (bSurface b) $ \y -> y { bBuffers = filter ((/= buf) . bufObj) (bBuffers y) }
  _ -> pure ()
  where
    c = eConn env
    st = eState env

updBg :: Env -> ObjectId -> (Bg -> Bg) -> IO ()
updBg env surf f = modifyIORef' (eState env) $ \s -> s { sBgs = M.adjust f surf (sBgs s) }

global :: Env -> Word32 -> B.ByteString -> Word32 -> IO ()
global env name iface ver = do
  let bind v = wlRegistryBind c (eReg env) name iface (min ver v)
      setG f = modifyIORef' st $ \s -> s { sGlobals = f (sGlobals s) }
  case iface of
    "wl_compositor" -> bind 6 >>= \o -> setG (\g -> g { gCompositor = Just (o, min ver 6) })
    "wl_shm" -> bind 1 >>= \o -> setG (\g -> g { gShm = Just o })
    "zwlr_layer_shell_v1" -> bind 4 >>= \o -> setG (\g -> g { gLayerShell = Just o })
    "wp_fractional_scale_manager_v1" -> bind 1 >>= \o -> setG (\g -> g { gFractional = Just o })
    "wp_viewporter" -> bind 1 >>= \o -> setG (\g -> g { gViewporter = Just o })
    "wl_output" -> do
      o <- bind 4
      modifyIORef' st $ \s -> s { sOutputs = M.insert o (Out Nothing 1 False name Nothing) (sOutputs s) }
    _ -> pure ()
  where
    c = eConn env
    st = eState env

------------------------------------------------------------------------------
-- Surfaces

-- | Give a wallpaper to every announced output that should have one.
createBgs :: Env -> IO ()
createBgs env = do
  s <- readIORef (eState env)
  when (sReady s) $
    forM_ (M.toList (sOutputs s)) $ \(o, out) ->
      when (oDone out && isNothing (oBg out) && wanted (oName out)) $ createBg env o out
  where
    wanted n = null (outputs (eCfg env)) || maybe False (`elem` outputs (eCfg env)) n

createBg :: Env -> ObjectId -> Out -> IO ()
createBg env o out = do
  g <- sGlobals <$> readIORef st
  forM_ ((,) <$> gCompositor g <*> gLayerShell g) $ \((comp, _), shell) -> do
    surf <- wlCompositorCreateSurface c comp
    frac <- forM (gFractional g) $ \fm -> wpFractionalScaleManagerV1GetFractionalScale c fm surf
    vp <- forM (gViewporter g) $ \vpm -> wpViewporterGetViewport c vpm surf
    ls <- zwlrLayerShellV1GetLayerSurface c shell surf o zwlrLayerShellV1LayerBackground
            (TE.encodeUtf8 (T.pack (namespace (eCfg env))))
    zwlrLayerSurfaceV1SetSize c ls 0 0
    zwlrLayerSurfaceV1SetAnchor c ls
      (zwlrLayerSurfaceV1AnchorTop .|. zwlrLayerSurfaceV1AnchorBottom .|. zwlrLayerSurfaceV1AnchorLeft .|. zwlrLayerSurfaceV1AnchorRight)
    zwlrLayerSurfaceV1SetExclusiveZone c ls (-1)   -- under bars, not beside them
    -- clicks on the desktop are nobody's
    empty <- wlCompositorCreateRegion c comp
    wlSurfaceSetInputRegion c surf empty
    wlRegionDestroy c empty
    wlSurfaceCommit c surf
    now <- getMonotonicTime
    modifyIORef' st $ \s -> s
      { sBgs = M.insert surf Bg
          { bSurface = surf, bLayer = ls, bFrac = frac, bViewport = vp, bOutput = o
          , bName = fromMaybe "" (oName out), bW = 1, bH = 1, bConfigured = False
          , bScale120 = 0, bBufScale = 0, bBuffers = [], bGeom = Nothing
          , bDirty = False, bFrameCb = Nothing, bNextAt = 0, bWake = False
          , bTarget = Nothing, bRequest = 0, bCurrent = Nothing, bFade = Nothing, bCache = []
          , bPos = 0, bPosTo = 0, bBusy = 0, bBusyTo = 0, bEaseAt = now, bSeen = False
          , bClock = 0, bClockAt = now
          , bWorkspace = Nothing
          } (sBgs s)
      , sOutputs = M.adjust (\x -> x { oBg = Just surf }) o (sOutputs s) }
    follow env
  where
    c = eConn env
    st = eState env

destroyBg :: Env -> Bg -> IO ()
destroyBg env b = do
  let c = eConn env
  mapM_ (wpFractionalScaleV1Destroy c) (bFrac b)
  mapM_ (wpViewportDestroy c) (bViewport b)
  zwlrLayerSurfaceV1Destroy c (bLayer b)
  wlSurfaceDestroy c (bSurface b)
  mapM_ (freeBuffer c) (bBuffers b)
  mapM_ freePicture (bCache b)
  modifyIORef' (eState env) $ \s -> s
    { sBgs = M.delete (bSurface b) (sBgs s)
    , sOutputs = M.adjust (\x -> x { oBg = Nothing }) (bOutput b) (sOutputs s) }

freeBuffer :: Conn -> Buffer -> IO ()
freeBuffer c b = do
  wlBufferDestroy c (bufObj b)
  shmUnmap (bufPtr b) (fromIntegral (bufSize b))

newBuffer :: Conn -> ObjectId -> Int -> Int -> IO (Maybe Buffer)
newBuffer c shm w h = do
  let stride = 4 * w
      size = stride * h
  fd <- shmFile (fromIntegral size)
  if fd < 0 then pure Nothing else do
    p <- shmMap fd (fromIntegral size)
    if p == nullPtr then pure Nothing else do
      pool <- wlShmCreatePool c shm (Fd fd) (fromIntegral size)  -- closes fd once sent
      buf <- wlShmPoolCreateBuffer c pool 0 (fromIntegral w) (fromIntegral h) (fromIntegral stride) wlShmFormatXrgb8888
      wlShmPoolDestroy c pool
      pure (Just (Buffer buf p size w h False))

-- | The surface's size or scale may have changed: work out the pixel
-- sizes again and, if they are different, load the pictures at the new
-- size (the old frame stays on screen meanwhile).
resize :: Env -> ObjectId -> IO ()
resize env surf = do
  s <- readIORef (eState env)
  forM_ (M.lookup surf (sBgs s)) $ \b -> when (bConfigured b) $ do
    let cfg = eCfg env
        outScale = maybe 1 oScale (M.lookup (bOutput b) (sOutputs s))
        (scale, unit, int) = case bViewport b of
          Just _ ->
            let dev = if bScale120 b > 0 then fromIntegral (bScale120 b) / 120
                      else fromIntegral (max 1 (if bBufScale b > 0 then bBufScale b else outScale))
                rs = min 1 (max 0.1 (renderScale cfg))
            in (dev * rs, rs, Nothing)
          Nothing ->
            let i = max 1 (if bBufScale b > 0 then bBufScale b else outScale)
            in (fromIntegral i :: Double, 1, Just i)
        pw = max 2 (round (fromIntegral (bW b) * scale))
        ph = max 2 (round (fromIntegral (bH b) * scale))
        margin = ceiling (fxMargin (eEffect env) * fromIntegral ph)
        cell = max 16 (ph `div` 60)
        gw = (pw + cell - 1) `div` cell + 1
        gh = (ph + cell - 1) `div` cell + 2
        same = case bGeom b of
          Just g -> gPw g == pw && gPh g == ph && gInt g == int
          Nothing -> False
    unless same $ do
      grid <- mallocForeignPtrArray (gw * gh * 2)
      mapM_ freePicture (bCache b)
      updBg env surf $ \x -> x
        { bGeom = Just (Geom pw ph margin cell gw gh grid unit int)
        , bTarget = Nothing, bRequest = bRequest x + 1
        , bCurrent = Nothing, bFade = Nothing, bCache = [] }
      retarget env

------------------------------------------------------------------------------
-- What to show

-- | Take note of what rivermonad shows on every output.
follow :: Env -> IO ()
follow env = do
  rs <- R.currentState (eRiver env)
  now <- getMonotonicTime
  forM_ rs $ \r -> modifyIORef' (eState env) $ \s -> s { sBgs = M.map (upd now r) (sBgs s) }
  where
    upd now r b =
      let wss = R.rsWorkspaces r
          mine = find (\w -> R.wsVisible w && R.wsOutput w == Just (bName b)) wss
          n = length (filter (not . R.wsExtra) wss)
          pos = case mine of
            Just w | n > 1 -> min 1 (max 0 (fromIntegral (R.wsIndex w - 1) / fromIntegral (n - 1)))
            _ -> bPosTo b
          busy = case mine of
            Just w | R.wsWindows w > 0 -> 1
            _ -> 0
          settled = bPos b == bPosTo b && bBusy b == bBusyTo b
          moved = pos /= bPosTo b || busy /= bBusyTo b
      in if not (bSeen b)
           then b { bPos = pos, bPosTo = pos, bBusy = busy, bBusyTo = busy, bSeen = True
                  , bWorkspace = mine, bDirty = True }
           else b { bPosTo = pos, bBusyTo = busy, bWorkspace = mine
                  , bDirty = bDirty b || (moved && (fxReactive (eEffect env) || isJust (busyFps (eCfg env))))
                  , bEaseAt = if settled then now else bEaseAt b }

-- | Ask the configuration what every output should show and start
-- loading what is not there yet.
retarget :: Env -> IO ()
retarget env = do
  s <- readIORef (eState env)
  time <- zonedTimeToLocalTime <$> getZonedTime
  secs <- floor <$> getPOSIXTime
  forM_ (M.elems (sBgs s)) $ \b -> forM_ (bGeom b) $ \geom -> do
    let ctx = Ctx { ctxOutput = bName b, ctxWorkspace = bWorkspace b, ctxTime = time
                  , ctxSeconds = secs, ctxStep = sStep s }
        fallback = PickColor (bgColor (eCfg env))
    pick <- case sOverride s of
      Just f -> pure (PickImage f)
      Nothing -> (E.evaluate =<< pickWallpaper (wallpaper (eCfg env)) ctx) `E.catch` \(e :: E.SomeException) -> do
        hPutStrLn stderr ("bgmonad: the wallpaper rule failed: " ++ show e)
        pure fallback
    key <- case pick of
      PickImage f -> do
        f' <- expandHome f
        t <- E.try (getModificationTime f') :: IO (Either E.IOException UTCTime)
        pure (PickImage f', either (const Nothing) Just t)
      PickColor _ -> pure (pick, Nothing)
    when (bTarget b /= Just key) $ do
      let req = bRequest b + 1
      updBg env (bSurface b) (\x -> x { bTarget = Just key, bRequest = req })
      case find ((== key) . picKey) (bCache b) of
        Just pic -> install env b { bRequest = req } pic
        Nothing -> void $ forkIO $ do
          pic <- loadPicture env geom key
          ePush env (MsgLoaded (bSurface b) req pic)

-- | Decode a picture at the size of an output. Never fails: what cannot
-- be loaded becomes the background colour.
loadPicture :: Env -> Geom -> Key -> IO Picture
loadPicture env g key = do
  let cfg = eCfg env
      bg = parseColor (bgColor cfg)
      radius = round (fxBlurRadius (eEffect env) * fromIntegral (gPh g)) :: Int
  sharp <- case fst key of
    PickColor c -> pure (LayerColor (parseColor c))
    PickImage f -> do
      r <- E.try (loadLayer f (gSw g) (gSh g) (fit cfg) bg (gUnit g))
      case r of
        Right (Right l) -> pure l
        Right (Left msg) -> failed f msg
        Left e -> failed f (show (e :: E.SomeException))
  blurred <- if radius > 0 then blurLayer sharp (gSw g) (gSh g) radius else pure Nothing
  pure (Picture key sharp blurred)
  where
    failed f msg = do
      hPutStrLn stderr ("bgmonad: " ++ f ++ ": " ++ msg)
      pure (LayerColor (parseColor (bgColor (eCfg env))))

freePicture :: Picture -> IO ()
freePicture p = freeLayer (picSharp p) >> mapM_ freeLayer (picBlur p)

-- | Make a picture the one on screen, fading from the previous one.
install :: Env -> Bg -> Picture -> IO ()
install env b pic = do
  now <- getMonotonicTime
  let fade = case bCurrent b of
        Just old | transition (eCfg env) > 0, picKey old /= picKey pic -> Just (old, now)
        _ -> Nothing
      keep = max 1 (cacheSize (eCfg env)) `max` (if isJust fade then 2 else 1)
      others = filter ((/= picKey pic) . picKey) (bCache b)
      -- the one fading out goes right behind the new one
      ordered = sortOn (\p -> if Just (picKey p) == fmap (picKey . fst) fade then 0 else 1 :: Int) others
      (kept, dropped) = splitAt (keep - 1) ordered
  mapM_ freePicture dropped
  updBg env (bSurface b) $ \x -> x
    { bCurrent = Just pic, bFade = fade, bCache = pic : kept, bDirty = True }

------------------------------------------------------------------------------
-- Drawing

-- | Draw the outputs that want a frame and may have one now; the ones
-- that are early get a wake-up call.
drawDue :: Env -> IO ()
drawDue env = do
  s <- readIORef (eState env)
  now <- getMonotonicTime
  forM_ (M.elems (sBgs s)) $ \b ->
    when (bDirty b && bConfigured b && isNothing (bFrameCb b) && isJust (bCurrent b) && isJust (bGeom b)) $
      if now >= bNextAt b - 0.002
        then draw env b now
        else unless (bWake b) $ do
          updBg env (bSurface b) (\x -> x { bWake = True })
          void $ forkIO $ do
            threadDelay (ceiling ((bNextAt b - now) * 1e6))
            ePush env (MsgWake (bSurface b))

draw :: Env -> Bg -> Double -> IO ()
draw env b now = do
  s <- readIORef (eState env)
  let c = eConn env
      cfg = eCfg env
      surf = bSurface b
  forM_ ((,,,) <$> gShm (sGlobals s) <*> bGeom b <*> bCurrent b <*> gCompositor (sGlobals s)) $ \(shm, g, cur, (_, compVer)) -> do
    let pw = gPw g
        ph = gPh g
        sized x = bufW x == pw && bufH x == ph
        (stale, keep) = foldr (\x (a, k) -> if not (bufBusy x) && not (sized x) then (x : a, k) else (a, x : k)) ([], []) (bBuffers b)
    mapM_ (freeBuffer c) stale
    mbuf <- case find (\x -> not (bufBusy x) && sized x) keep of
      Just x -> pure (Just (x, keep))
      Nothing
        | length keep >= 3 -> pure Nothing  -- wait for a release
        | otherwise -> fmap (\x -> (x, x : keep)) <$> newBuffer c shm pw ph
    case mbuf of
      Nothing -> updBg env surf (\x -> x { bBuffers = keep })
      Just (buf, bufs) -> do
        -- where everything is at this instant
        let tau = easing cfg
            dt = max 0 (now - bEaseAt b)
            ease v to
              | tau <= 0 || abs (to - v) < 0.002 = to
              | otherwise = to + (v - to) * exp (negate dt / tau)
            pos = ease (bPos b) (bPosTo b)
            busy = ease (bBusy b) (bBusyTo b)
            easingOn = pos /= bPosTo b || busy /= bBusyTo b
            -- behind windows the motion may be slower, or stop
            behind = bBusyTo b > 0
            rate = fromIntegral (max 1 (fps cfg)) :: Double
            calm = case busyFps cfg of
              Just n | behind -> min rate (fromIntegral (max 0 n))
              _ -> rate
            frozen = sPaused s || calm <= 0
            -- the animation goes on from where it was last seen: time
            -- spent hidden, paused or waiting does not count
            clock
              | frozen = bClock b
              | otherwise = bClock b + min (max 0.25 (2 / calm)) (max 0 (now - bClockAt b))
            frame = Frame
              { frTime = clock
              , frAspect = fromIntegral pw / fromIntegral ph
              , frWorkspace = pos
              , frBusy = busy }
            fx = eEffect env
            fadeAt = case bFade b of
              Just (old, t0) | now - t0 < transition cfg -> Just (old, smooth ((now - t0) / transition cfg))
              _ -> Nothing
            blurAmt = min 1 (max 0 (fxBlur fx frame))
            light = 1 - min 1 (max 0 (fxDim fx frame))
            parts pic w = case picBlur pic of
              Just bl | blurAmt > 0 -> [(picSharp pic, w * (1 - blurAmt)), (bl, w * blurAmt)]
              _ -> [(picSharp pic, w)]
            layers = weigh light $ case fadeAt of
              Just (old, m) -> parts old (1 - m) ++ parts cur m
              Nothing -> parts cur 1

        case (fxWarp fx, layers) of
          (Nothing, [(LayerImage fp, 256)]) -> copyLayer (bufPtr buf) pw ph fp (gSw g) (gMargin g) (gMargin g)
          (Nothing, [(LayerColor col, 256)]) -> fillPixels (bufPtr buf) (pw * ph) col
          (mwarp, _) -> do
            fillGrid g (fmap ($ frame) mwarp)
            withForeignPtr (gGrid g) $ \grid ->
              drawLayers (bufPtr buf) pw ph grid (gGw g) (gCell g) layers (gSw g) (gSh g) (eThreads env)

        wlSurfaceAttach c surf (bufObj buf) 0 0
        case (gInt g, bViewport b) of
          (Nothing, Just vp) -> do
            wlSurfaceSetBufferScale c surf 1
            wpViewportSetDestination c vp (fromIntegral (bW b)) (fromIntegral (bH b))
          (Just i, _) -> when (compVer >= 3) $ wlSurfaceSetBufferScale c surf (fromIntegral i)
          _ -> pure ()
        if compVer >= 4
          then wlSurfaceDamageBuffer c surf 0 0 (fromIntegral pw) (fromIntegral ph)
          else wlSurfaceDamage c surf 0 0 (fromIntegral (bW b)) (fromIntegral (bH b))
        cb <- wlSurfaceFrame c surf
        wlSurfaceCommit c surf

        -- with nothing else to draw for now, give the spare buffers back
        let fading = isJust fadeAt
            more = fading || easingOn || (fxLive fx && not frozen)
            interval = 1 / (if fading || easingOn || frozen then rate else calm)
            (spare, used) = if more then ([], bufs) else foldr (\y (a, k) -> if bufBusy y || bufObj y == bufObj buf then (a, y : k) else (y : a, k)) ([], []) bufs
        mapM_ (freeBuffer c) spare
        updBg env surf $ \x -> x
          { bBuffers = map (\y -> if bufObj y == bufObj buf then y { bufBusy = True } else y) used
          , bFrameCb = Just cb
          , bNextAt = now + interval
          , bClock = clock, bClockAt = now
          , bDirty = more
          , bPos = pos, bBusy = busy, bEaseAt = now
          , bFade = if fading then bFade x else Nothing }
        unless (sShown s) $ do
          modifyIORef' (eState env) (\x -> x { sShown = True })
          takeOver env
  where
    smooth t = let u = min 1 (max 0 t) in u * u * (3 - 2 * u)

-- | Turn fractions of the light into weights out of 256 that add up to
-- exactly what they should (a pixel must not flicker by a rounding error).
weigh :: Double -> [(Layer, Double)] -> [(Layer, Int)]
weigh light parts =
  let total = round (256 * light) :: Int
      rounded = [ (l, round (256 * light * w) :: Int) | (l, w) <- parts ]
      err = total - sum (map snd rounded)
      biggest = maximum (map snd rounded)
      fixup done ((l, w) : rest)
        | not done && w == biggest = (l, w + err) : fixup True rest
        | otherwise = (l, w) : fixup done rest
      fixup _ [] = []
  in filter ((> 0) . snd) (fixup False rounded)

-- | Fill the grid with the source position of every node: where the
-- node is, plus the margin, plus what the effects say.
fillGrid :: Geom -> Maybe Warp -> IO ()
fillGrid g mwarp = withForeignPtr (gGrid g) $ \p -> do
  let h = fromIntegral (gPh g) :: Double
      m = fromIntegral (gMargin g) :: Double
      cell = fromIntegral (gCell g) :: Double
      gw = gGw g
  forM_ [0 .. gGh g - 1] $ \j -> forM_ [0 .. gw - 1] $ \i -> do
    let x = fromIntegral i * cell
        y = fromIntegral j * cell
        (dx, dy) = case mwarp of
          Just f -> f (x / h) (y / h)
          Nothing -> (0, 0)
        k = 2 * (j * gw + i)
    pokeElemOff p k (realToFrac (x + m + dx * h))
    pokeElemOff p (k + 1) (realToFrac (y + m + dy * h))

-- | The first frame is on screen: a previous bgmonad can go now, and its
-- control socket is ours.
takeOver :: Env -> IO ()
takeOver env = void $ forkIO $ do
  replaceRunning
  startControlServer (\cmd reply -> ePush env (MsgControl cmd reply))

utf8 :: B.ByteString -> String
utf8 = T.unpack . TE.decodeUtf8Lenient
