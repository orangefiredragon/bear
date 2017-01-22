{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Serv.Server.ServerEnv
    ( ServerEnv(..)
    , setupServerEnv
    ) where

import           Serv.Server.Core.Metrics
import           Serv.Server.Core.ServerConfig

data ServerEnv = ServerEnv
  { serverConfig   :: ServerConfig
  ,  serverMetrics :: ServerMetrics
  }


setupServerEnv :: IO ServerEnv
setupServerEnv = do
  config <- setupServerConfig
  metrics <- setupMetrics
  return (ServerEnv config metrics)
