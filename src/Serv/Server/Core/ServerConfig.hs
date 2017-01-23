{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Serv.Server.Core.ServerConfig
    ( ServerConfig(..)
    , setupServerConfig
    , loadConfig
    ) where

import           Control.Exception
import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             Value (..), object, (.:), (.:?),
                                             (.=))
import qualified Data.Aeson                 as JS
import qualified Data.Aeson.Types           as JS
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Monoid                hiding ((<>))
import           Data.Semigroup             (Semigroup (..))
import           Data.Text                  (Text)
import qualified Data.Yaml                  as Y
import qualified Data.Yaml                  as Yaml
import           GHC.Generics
import           Serv.Util.GenericMonoid
import           Serv.Util.Validate
import           System.Environment         (lookupEnv)

type Port = Int
type Secret = Text
type ServerName = Text


data ServerConfig = ServerConfig
  { serverApiPort :: Port
  , serverSysPort :: Port
  , serverName    :: ServerName
  } deriving (Generic, Show)

defaultServerConfig = ServerConfig { serverApiPort = 8080, serverSysPort = 8090, serverName = "Bear" }

setupServerConfig :: IO ServerConfig
setupServerConfig = return defaultServerConfig

loadConfig :: IO (Either [String] ServerConfig)
loadConfig = do
  fileConfig <- readConfigFile "serv.yaml"
  let c = defaultConfig <> fileConfig
  putStrLn "Loading configuaration"
  BS.putStrLn (showConfig c)
  return (validateConfig c)

validateConfig :: RawConfig -> Either [String] ServerConfig
validateConfig (RawConfig apiPort sysPort serverName) =
   runValidate $
     ServerConfig <$> validatePort apiPort
                  <*> validatePort sysPort
                  <*> validateServerName serverName

validatePort :: Last Int -> Validate Int
validatePort = requiredField "a port number is required"

validateServerName :: Last ServerName -> Validate ServerName
validateServerName = requiredField "server name is required"


-- Raw Config file handling

data RawConfig = RawConfig
                (Last Port)                   -- API port
                (Last Port)                   -- Management port
                (Last ServerName)             -- server name
  deriving (Eq, Show, Generic)

-- Default Configuration

defaultConfig :: RawConfig
defaultConfig = RawConfig (Last (Just 8080)) (Last (Just 8080)) (Last (Just "Bear"))


instance Semigroup RawConfig where
  (<>) = gmappend

instance Monoid RawConfig where
  mempty  = gmempty
  mappend = (<>)

instance ToJSON RawConfig where
  toJSON (RawConfig apiPort sysPort serverName) =
    object [ "apiPort"    .= apiPort
           , "sysPort"    .= sysPort
           , "serverName" .= serverName
           ]

instance FromJSON RawConfig where
  parseJSON =
    JS.withObject "RawConfig" $ \o ->
      RawConfig <$> (Last <$> (o .:? "apiPort"))
             <*> (Last <$> (o .:? "sysPort"))
             <*> (Last <$> (o .:? "serverName"))

readConfigFile :: FilePath -> IO RawConfig
readConfigFile file = either throwIO return =<< Yaml.decodeFileEither file

showConfig :: RawConfig -> BS.ByteString
showConfig = Yaml.encode


optionalField :: a -> Last a -> Validate a
optionalField def (Last Nothing)  = return def
optionalField _   (Last (Just p)) = return p

requiredField :: String -> Last a -> Validate a
requiredField errmsg (Last Nothing)  = fail errmsg
requiredField _      (Last (Just p)) = return p



{-
loadConfig :: IO (Maybe RuntimeConig)
loadConfig = loadConfigFromFile "serv.yaml"

loadConfigFromFile :: String -> IO (Maybe RuntimeConig)
loadConfigFromFile = Y.decodeFile


getEnvVar :: Read a => String -> a -> IO a
getEnvVar name def = do
    maybeValue <- lookupEnv name
    return $ maybe def read maybeValue

  -}
