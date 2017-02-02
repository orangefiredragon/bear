{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Serv.Server.ServerEnv
    ( ServerEnv(..)
    , setupServerEnv
    ) where

import           Control.Exception
import           Data.Default.Class
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import           Serv.Server.Core.Logger
import           Serv.Server.Core.Metrics
import           Serv.Server.Core.ServerConfig
import           System.Log.FastLogger

data ServerEnv = ServerEnv
  { serverConfig   :: ServerConfig
  ,  serverMetrics :: ServerMetrics
  , log            :: LogStr -> IO ()
  , logH           :: HandlerLogger
  , logEnv         :: LogEnv
  }


setupServerEnv :: IO ServerEnv
setupServerEnv = do

  -- bootstrap configuration
  conf <- loadConfig
  config <- case conf of
    (Left errs) -> fail (concat ("Configuration errors.\n" : errs))
    (Right c)   -> return c

  -- bootstrap metrics
  metrics <- setupMetrics

  -- bootstrap logging
  logEnv <- setupLogger (serverLog config)
  -- WAI logging middleware

  return (ServerEnv config metrics (logMsg logEnv) (logMsgH logEnv) logEnv)
