{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Serv.Api.EntityApi
    ( EntityV1Api
    , entityV1ApiRoot
    ) where

import           Data.Proxy
import           Data.Text               (Text)
import qualified Data.Text.Internal.Lazy as LT
import           Serv.Api.EntityTypes
import           Serv.Api.Types
import           Servant.API

-- | Type for Resource Id path
type IdPath = Capture ":entityId" EntityId
type Location = LT.Text

entityV1ApiRoot :: Text
entityV1ApiRoot = "/v1/entities/"
-- |  Entity API V1
type EntityV1Api ="v1" :> "entities" :>
  (                ReqBody '[JSON] PostEntitiesRequest :>  PostCreated '[JSON]  (Headers '[Header "Location" Location] (Response Entity))            -- Create the entity.
    :<|> IdPath :>                                         Get '[JSON] (Response Entity)             -- Get the entity.
    :<|> IdPath :> ReqBody '[JSON] PutEntitiesRequest :>   Put '[JSON] (Response Entity)             -- Update the entity.
    :<|> IdPath :>                                         Delete '[JSON] (Response LT.Text)           -- Delete the entity.
  )
