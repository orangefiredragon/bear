{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Serv.Server.Core.Metrics
    ( ServerMetrics(..)
    , setupMetrics
    , sampleMetrics
    ) where

import           System.Metrics

data ServerMetrics = ServerMetrics
  { metricsStore :: Store
  }

setupMetrics :: IO ServerMetrics
setupMetrics = do
  store <- newStore
  registerGcMetrics store
  return ServerMetrics { metricsStore = store}

sampleMetrics :: ServerMetrics -> IO Sample
sampleMetrics ServerMetrics { metricsStore } = sampleAll metricsStore
