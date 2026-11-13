-- | The control socket: @bgmonad next@, @bgmonad pause@... talk to the
-- running bgmonad through @$XDG_RUNTIME_DIR/bgmonad-$WAYLAND_DISPLAY.sock@
-- with one line per request and one line of reply. Bind them to keys in
-- rivermonad with @spawn "bgmonad next"@.
module Bgmonad.Control
  ( Command (..)
  , parseCommand
  , commandHelp
  , controlSocketPath
  , startControlServer
  , sendCommand
  , replaceRunning
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import qualified Control.Exception as E
import Control.Monad (forever, void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Directory (removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

data Command
  = Next            -- ^ step slideshows forward
  | Prev
  | Pause           -- ^ freeze the animation
  | Resume
  | Toggle          -- ^ pause or resume
  | Set FilePath    -- ^ show this image on every output, whatever the configuration says
  | Unset           -- ^ back to the configuration
  | Status          -- ^ what every output shows
  | Quit
  deriving (Eq, Show)

parseCommand :: [String] -> Maybe Command
parseCommand = \case
  ["next"] -> Just Next
  ["prev"] -> Just Prev
  ["pause"] -> Just Pause
  ["resume"] -> Just Resume
  ["toggle"] -> Just Toggle
  ["set", f] -> Just (Set f)
  ["unset"] -> Just Unset
  ["status"] -> Just Status
  ["quit"] -> Just Quit
  _ -> Nothing

commandHelp :: [String]
commandHelp =
  [ "  next, prev   step through slideshows"
  , "  pause, resume, toggle"
  , "               freeze or continue the animation"
  , "  set FILE     show an image on every output, whatever the configuration says"
  , "  unset        back to the configuration"
  , "  status       what every output shows"
  , "  quit         stop bgmonad"
  ]

controlSocketPath :: IO FilePath
controlSocketPath = do
  dir <- fromMaybe "/tmp" <$> lookupEnv "XDG_RUNTIME_DIR"
  disp <- fromMaybe "wayland-0" <$> lookupEnv "WAYLAND_DISPLAY"
  pure (dir </> ("bgmonad-" ++ map (\c -> if c == '/' then '_' else c) disp ++ ".sock"))

-- | Listen for commands. The handler gets the command and a way to reply
-- (once, from any thread); it runs in the server's threads.
startControlServer :: (Command -> (String -> IO ()) -> IO ()) -> IO ()
startControlServer handler = do
  r <- E.try $ do
    path <- controlSocketPath
    _ <- E.try (removeFile path) :: IO (Either E.IOException ())
    sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
    NS.withFdSocket sock NS.setCloseOnExecIfNeeded
    NS.bind sock (NS.SockAddrUnix path)
    NS.listen sock 8
    void $ forkIO $ forever $ do
      (client, _) <- NS.accept sock
      NS.withFdSocket client NS.setCloseOnExecIfNeeded
      void $ forkIO $ serve client `E.finally` NS.close client
  case r of
    Right () -> pure ()
    Left e -> hPutStrLn stderr ("bgmonad: control socket disabled: " ++ show (e :: E.SomeException))
  where
    serve client = do
      line <- readLine client B.empty
      let ws = map T.unpack (T.splitOn (T.pack "\t") (TE.decodeUtf8Lenient line))
      answer <- newEmptyMVar
      case parseCommand ws of
        Just cmd -> handler cmd (void . tryPutMVar answer)
        Nothing -> putMVar answer "error: unknown command"
      -- the handler answers from another thread, when it gets to it
      r <- timeout 5000000 (takeMVar answer)
      void (E.try (NSB.sendAll client (TE.encodeUtf8 (T.pack (fromMaybe "error: no answer" r ++ "\n")))) :: IO (Either E.IOException ()))

readLine :: NS.Socket -> B.ByteString -> IO B.ByteString
readLine s acc = case BC.elemIndex '\n' acc of
  Just i -> pure (B.take i acc)
  Nothing
    | B.length acc > 65536 -> pure acc
    | otherwise -> do
        r <- E.try (NSB.recv s 4096) :: IO (Either E.IOException B.ByteString)
        case r of
          Right chunk | not (B.null chunk) -> readLine s (acc <> chunk)
          _ -> pure acc

-- | Send a command to the running bgmonad and return its reply; 'Nothing'
-- if there is none running.
sendCommand :: [String] -> IO (Maybe String)
sendCommand ws = do
  path <- controlSocketPath
  r <- E.try $ E.bracket (NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol) NS.close $ \s -> do
    NS.connect s (NS.SockAddrUnix path)
    NSB.sendAll s (TE.encodeUtf8 (T.intercalate (T.pack "\t") (map T.pack ws)) <> BC.pack "\n")
    let loop acc = do
          chunk <- NSB.recv s 4096
          if B.null chunk then pure acc else loop (acc <> chunk)
    T.unpack . T.stripEnd . TE.decodeUtf8Lenient <$> loop B.empty
  pure $ case r of
    Right reply -> Just reply
    Left e -> const Nothing (e :: E.IOException)

-- | Ask a bgmonad that is already running on this display to leave and
-- wait until it has, so that starting bgmonad again replaces it (with the
-- new configuration).
replaceRunning :: IO ()
replaceRunning = sendCommand ["quit"] >>= \case
  Nothing -> pure ()
  Just _ -> wait (40 :: Int)
  where
    wait 0 = pure ()
    wait n = sendCommand ["status"] >>= \case
      Nothing -> pure ()
      Just _ -> threadDelay 50000 >> wait (n - 1)
