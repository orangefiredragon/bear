{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Serv.Server.Core.InfoApi
    ( InfoApi
    ) where

import           Data.Aeson
import           Data.Proxy
import           Data.Text             (Text)
import           GHC.Generics
import           Git.Embed
import           Serv.Server.ServerEnv
import           Servant
import           Servant.API

-- | HealthResponse
data InfoResponse = InfoResponse
  { version   :: Text
  , changeset :: Text
  , branch    :: Text
  } deriving (Generic, Show)

defaultInfo = InfoResponse
              { version   = "version"
              , changeset = $(embedGitShortRevision)
              , branch    = $(embedGitBranch)
              }

instance FromJSON InfoResponse
instance ToJSON   InfoResponse

-- |  Info API
type InfoApi = "info" :> Get '[JSON] InfoResponse             -- Get the info.


handleInfo :: ServerEnv -> Handler InfoResponse
handleInfo _ = return defaultInfo
