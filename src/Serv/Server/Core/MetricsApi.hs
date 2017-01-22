{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Serv.Server.Core.MetricsApi
    ( MetricsApi
    ) where

import           Data.Aeson
import           Data.Proxy
import           Serv.Api.Auth
import           Servant.API
import           Servant.API.BasicAuth (BasicAuthData (BasicAuthData))


-- |  Health API V1
type MetricsApi = BasicAuth "serv-realm" Principal :> "metrics" :> Get '[JSON] Data.Aeson.Value       -- Get metrics.
