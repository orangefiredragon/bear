{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Serv.Server.Core.Logger
    ( setupLogger
    , cleanupLogger
    , LogEnv(..)
    , HandlerLogger
    ) where

import           Data.Monoid                          ((<>))
import           System.Log.FastLogger                hiding (newTimeCache)

import           Control.AutoUpdate                   (defaultUpdateSettings,
                                                       mkAutoUpdate,
                                                       updateAction)
import           Control.Concurrent                   (myThreadId)
import           Control.Exception                    (SomeException (..),
                                                       bracket, handle)
import           Control.Monad                        (replicateM, when)
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as BS
import           Data.Time                            (UTCTime,
                                                       defaultTimeLocale,
                                                       formatTime,
                                                       getCurrentTime,
                                                       utcToLocalZonedTime)

import           Data.IORef

import           Serv.Server.Core.ServerConfig        (LogAppender (..),
                                                       LogConfig (..))

import           Control.Monad.IO.Class
import           Data.Default.Class
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger
import           Servant                              (Handler)

type HandlerLogger = LogStr -> Handler ()
data LogEnv = LogEnv
                { logMsg        :: FastLogger
                , logMsgH       :: HandlerLogger
                , cleanup       :: IO()
                , logMiddleware :: Maybe Middleware
                }


-- bootstrap Logger
setupLogger :: LogConfig -> IO LogEnv
setupLogger LogConfig{..} = do
  -- build log types
  let os = case appenders of
            [] -> [None]
            x  -> x

  let ts = map (\x -> case x of
                  None    -> LogNone
                  Console -> LogStdout defaultBufSize
                  File    -> LogFileNoRotate "server.log" defaultBufSize
                  ) appenders

  -- build loggers
  ls <- mapM mkLogger ts
  let loggers = map fst ls
  let cleaners = map snd ls

  -- build log and cleanup functions
  let logMsg msg = mapM_ (\x -> x msg) loggers
  let cleanup = sequence_ cleaners

  let logMsgH msg = liftIO(logMsg msg)

  -- WAI logging middleware
  reqLogSet <- case request of
    None    -> return Nothing
    Console -> Just <$> newStdoutLoggerSet defaultBufSize
    File    -> Just <$> newFileLoggerSet defaultBufSize "req.log"

  logMiddleware <- case reqLogSet of
                      Nothing   -> return Nothing
                      (Just ls) -> Just <$> mkRequestLogger def { outputFormat = Apache FromSocket
                                                                , destination  = Logger ls
                                                                }
  return (LogEnv logMsg logMsgH cleanup logMiddleware)

-- Clean up Logger
cleanupLogger :: LogEnv -> IO ()
cleanupLogger LogEnv{..} = cleanup

-- Logger Factory
mkLogger :: LogType -> IO (FastLogger, IO())
mkLogger logType = do
  tgetter <- newTimeCache "%Y-%m-%dT%H:%M:%S%Q%z"
  (logger, cleanup) <- newTimedFastLogger  tgetter logType

  return (
          \(msg :: LogStr) -> do
                    trd <- myThreadId
                    logger (\x->toLogStr x <> " " <>  toLogStr (show trd) <> " " <> msg <> "\n")
         , cleanup)


getTime :: IO UTCTime
getTime = getCurrentTime
-- | Format UTC date.
formatDate :: TimeFormat -> UTCTime -> IO FormattedTime
formatDate fmt ut = do
 zt <- utcToLocalZonedTime ut
 return $ BS.pack $ formatTime defaultTimeLocale (BS.unpack fmt) zt


 -- |  Make 'IO' action which get cached formatted local time.
 -- Use this to avoid the cost of frequently time formatting by caching an
 -- auto updating formatted time, this cache update every 1 second.
 -- more detail in "Control.AutoUpdate"
newTimeCache :: TimeFormat -> IO (IO FormattedTime)
newTimeCache fmt = mkAutoUpdate defaultUpdateSettings{
       updateAction = getTime >>= formatDate fmt
   }
