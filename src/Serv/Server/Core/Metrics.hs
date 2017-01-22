{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Serv.Server.Core.Metrics
    ( ServerMetrics(..)
    , setupMetrics
    , sampleMetrics
    ) where

import           Network.Wai.Metrics
import           System.Metrics

data ServerMetrics = ServerMetrics
  { metricsStore :: Store
  , waiMetrics   :: WaiMetrics
  }

setupMetrics :: IO ServerMetrics
setupMetrics = do
  store <- newStore
  registerGcMetrics store
  waiMetrics <- registerWaiMetrics store
  return ServerMetrics { metricsStore = store, waiMetrics = waiMetrics }

sampleMetrics :: ServerMetrics -> IO Sample
sampleMetrics ServerMetrics { metricsStore } = sampleAll metricsStore
