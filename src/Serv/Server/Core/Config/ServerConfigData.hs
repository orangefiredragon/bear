{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Serv.Server.Core.Config.ServerData
    ( ServerConfig(..)
    , defaultConfig
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
import           Serv.Server.Core.Config.Types
import           Serv.Util.GenericMonoid
import           Serv.Util.Validate


validateConfig :: RawConfig -> Either [String] ServerConfig
validateConfig (RawConfig apiPort sysPort serverName logConfig) =
   runValidate $
     ServerConfig <$> validatePort apiPort
                  <*> validatePort sysPort
                  <*> validateServerName serverName
                  <*> validateLog (LogConfig [None] None) logConfig

validatePort :: Last Int -> Validate Int
validatePort = requiredField "a port number is required"

validateServerName :: Last ServerName -> Validate ServerName
validateServerName = requiredField "server name is required"



data RawConfig = RawConfig
                (Last Port)                   -- API port
                (Last Port)                   -- Management port
                (Last ServerName)             -- server name
                (Last RawLogConfig)
  deriving (Eq, Show, Generic)


instance Semigroup RawConfig where
  (<>) = gmappend

instance Monoid RawConfig where
  mempty  = gmempty
  mappend = (<>)

instance ToJSON RawConfig where
  toJSON (RawConfig apiPort sysPort serverName serverLog) =
    object [ "apiPort"    .= apiPort
           , "sysPort"    .= sysPort
           , "serverName" .= serverName
           , "log"        .= serverLog
           ]

instance FromJSON RawConfig where
  parseJSON =
    JS.withObject "RawConfig" $ \o ->
      RawConfig <$> (Last <$> (o .:? "apiPort"))
                <*> (Last <$> (o .:? "sysPort"))
                <*> (Last <$> (o .:? "serverName"))
                <*> (Last <$> (o .:? "log"))

readConfigFile :: FilePath -> IO RawConfig
readConfigFile file = either throwIO return =<< Yaml.decodeFileEither file

showConfig :: RawConfig -> BS.ByteString
showConfig = Yaml.encode
