{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}

module Serv.Server.Core.HealthTypes
  ( HealthResponse(..)
  , healthUp
  , healthDown
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics
import           Servant.API

-- | HealthStatus
data HealthStatus = UP
                  | DOWN
  deriving (Generic, Show)
instance FromJSON HealthStatus
instance ToJSON   HealthStatus

-- | HealthResponse
data HealthResponse = HealthResponse
  { status::HealthStatus
  } deriving (Generic, Show)

instance FromJSON HealthResponse
instance ToJSON   HealthResponse

healthUp::HealthResponse
healthUp = HealthResponse UP

healthDown::HealthResponse
healthDown = HealthResponse DOWN
