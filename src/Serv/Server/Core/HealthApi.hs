{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Serv.Server.Core.HealthApi
    ( HealthApi
    ) where

import           Data.Proxy
import           Serv.Server.Core.HealthTypes
import           Servant.API

-- |  Health API V1
type HealthApi = "health" :> Get '[JSON] HealthResponse             -- Get the health.
