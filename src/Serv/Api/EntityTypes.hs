{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}

module Serv.Api.EntityTypes
  ( Entity(..)
  , PostEntitiesRequest(..)
  , PutEntitiesRequest(..)
  ) where

import           Control.Lens
import           Data.Aeson
import qualified Data.Swagger    as Swagger
import           Data.Text       (Text)
import           GHC.Generics
import           Serv.Api.Types
import           Servant.API
import           Servant.Swagger

-- | Entity
data Entity = Entity
  { entityId   :: EntityId
  , entityName :: Text
  } deriving (Generic, Show)

instance FromJSON         Entity
instance ToJSON           Entity
instance Swagger.ToSchema Entity where
  declareNamedSchema proxy = Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions proxy
    & mapped.Swagger.schema.Swagger.description ?~ "This is some real Entity right here"
    & mapped.Swagger.schema.Swagger.example ?~ toJSON (Entity (EntityId 1) "My name")


-- | POST /entities request body
data PostEntitiesRequest = PostEntitiesRequest
  { entityName :: Text                      -- Entity Name. Required.
  } deriving (Show, Generic)

instance FromJSON         PostEntitiesRequest
instance ToJSON           PostEntitiesRequest
instance Swagger.ToSchema PostEntitiesRequest where
  declareNamedSchema proxy = Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions proxy
    & mapped.Swagger.schema.Swagger.description ?~ "This is some real Status right here"
    & mapped.Swagger.schema.Swagger.example ?~ toJSON (PostEntitiesRequest "My name")

-- | POST /entities request body
data PutEntitiesRequest = PutEntitiesRequest
  { entityName :: Maybe Text                      -- Entity Name. Optional.
  } deriving (Show, Generic)

instance FromJSON         PutEntitiesRequest
instance ToJSON           PutEntitiesRequest
instance Swagger.ToSchema PutEntitiesRequest where
  declareNamedSchema proxy = Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions proxy
    & mapped.Swagger.schema.Swagger.description ?~ "This is some real Status right here"
    & mapped.Swagger.schema.Swagger.example ?~ toJSON (PutEntitiesRequest (Just "My name"))
