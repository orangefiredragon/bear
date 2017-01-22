{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module Serv.Api.Types
  ( EntityId(..)
  , failure
  , failureNoBody
  , failureReqBody
  , ResponseStatus(..)
  , Response(..)
  , success
  , successNoBody
  ) where

import           Control.Lens    (mapped, (&), (?~))
-- import           Data.Aeson
import           Data.Aeson      (FromJSON (..), ToJSON (..), Value (..),
                                  object, withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.Swagger    as Swagger
import           Data.Text       (Text)
import qualified Data.Text.Lazy  as LT
import           GHC.Generics
import           Servant.API
import           Servant.Swagger


-- | Entity Identifier
newtype EntityId = EntityId Int
  deriving (Show, Eq, Ord, ToHttpApiData, FromHttpApiData, Generic)

instance FromJSON               EntityId
instance ToJSON                 EntityId
instance Swagger.ToParamSchema  EntityId
instance Swagger.ToSchema       EntityId

type StatusCode = Int
type StatusDetails = Maybe LT.Text

-- | Response status
data ResponseStatus = ResponseStatus
  { code    :: StatusCode
  , details :: StatusDetails
  } deriving (Show, Generic)

instance FromJSON         ResponseStatus
instance ToJSON           ResponseStatus where
  toJSON (ResponseStatus code Nothing) = object [ "code" .= code
                                                ]
  toJSON (ResponseStatus code details) = object [ "code"   .= code
                                                , "details" .= details
                                                ]


instance Swagger.ToSchema ResponseStatus where
  declareNamedSchema proxy = Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions proxy
    & mapped.Swagger.schema.Swagger.description ?~ "Response status example"
    & mapped.Swagger.schema.Swagger.example ?~ toJSON (ResponseStatus 0 (Just "Success"))


-- | Response body
data Response  a = Response
  { body   :: Maybe a
  , status :: ResponseStatus
  } deriving (Show, Generic)

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON =
    withObject "Response" (\o ->
    Response  <$> ( o .:? "data")
              <*> (o .:   "status")
            )

instance (ToJSON a) => ToJSON (Response a) where
  toJSON (Response Nothing status) = object [ "status" .= status
                                            ]
  toJSON (Response body status)    = object [ "data"   .= body
                                            , "status" .= status
                                            ]


instance (Swagger.ToSchema a) => Swagger.ToSchema (Response a) where
  declareNamedSchema proxy = Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions proxy
    & mapped.Swagger.schema.Swagger.description ?~ "Response example"
    & mapped.Swagger.schema.Swagger.example ?~ toJSON successNoBody

mkResp :: (ToJSON a) => StatusCode -> StatusDetails -> Maybe a -> Response a
mkResp c d b = Response { body = b, status = ResponseStatus c d}

success b = mkResp 0 Nothing (Just b)

successNoBody :: Response LT.Text
successNoBody = mkResp 0 Nothing Nothing

failure b = mkResp 700 Nothing (Just b)

failureNoBody :: Response LT.Text
failureNoBody = mkResp 700 Nothing Nothing

--failureReqBody :: Response LT.Text
failureReqBody d = mkResp 701 (Just d) (Nothing :: Maybe LT.Text)
