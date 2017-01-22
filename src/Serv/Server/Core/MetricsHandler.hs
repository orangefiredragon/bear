{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Serv.Server.Core.MetricsHandler
    ( handleMetrics
    ) where

import           Control.Monad.Except
import           Data.Aeson
import           Serv.Api.Auth
import           Serv.Server.Core.Metrics
import           Serv.Server.ServerEnv
import           Servant
import           System.Metrics.Json

handleMetrics :: ServerEnv -> Principal -> Handler Data.Aeson.Value
handleMetrics ServerEnv {..} p = liftIO $ sampleToJson <$> sampleMetrics serverMetrics
