{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Serv.Server.ServerEnv
    ( ServerEnv(..)
    , setupServerEnv
    ) where

import           Control.Exception
import           Serv.Server.Core.Metrics
import           Serv.Server.Core.ServerConfig

data ServerEnv = ServerEnv
  { serverConfig   :: ServerConfig
  ,  serverMetrics :: ServerMetrics
  }


setupServerEnv :: IO ServerEnv
setupServerEnv = do
  -- config <- setupServerConfig
  conf <- loadConfig
  config <- case conf of
    -- (Left errs) -> throw  ( (concat ("Configuration errors.\n" : errs)))
    (Right c)   -> return c
  metrics <- setupMetrics
  return (ServerEnv config metrics)
