{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Serv.Server.Core.Config.ServerData
    ( ServerData(..)
    , validateServer
    ) where

import           Control.Exception
import           Data.Aeson                    (FromJSON (..), ToJSON (..),
                                                Value (..), object, (.:), (.:?),
                                                (.=))
import qualified Data.Aeson                    as JS
import qualified Data.Aeson.Types              as JS
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.Monoid                   hiding ((<>))
import           Data.Semigroup                (Semigroup (..))
import           Data.Text                     (Text)
import qualified Data.Yaml                     as Y
import qualified Data.Yaml                     as Yaml
import           GHC.Generics
import           Serv.Server.Core.Config.Types
import           Serv.Util.GenericMonoid
import           Serv.Util.Validate



data ServerData = ServerData
                (Last Port)                   -- API port
                (Last ServerName)             -- server name
  deriving (Eq, Show, Generic)


instance Semigroup ServerData where
  (<>) = gmappend

instance Monoid ServerData where
  mempty  = gmempty
  mappend = (<>)

instance ToJSON ServerData where
  toJSON (ServerData port serverName) =
    object [ "port"    .= port
           , "name" .= serverName
           ]

instance FromJSON ServerData where
  parseJSON =
    JS.withObject "ServerData" $ \o ->
      ServerData <$> (Last <$> (o .:? "port"))
                <*> (Last <$> (o .:? "name"))


validateServer :: Last ServerData -> Validate ServerConfig
validateServer x = do
  (ServerData prt nm) <- requiredField "Server configuration is required" x
  port <- requiredField "Server port is required" prt
  serverName <- requiredField "Server name is required" nm
  return (ServerConfig port serverName )
