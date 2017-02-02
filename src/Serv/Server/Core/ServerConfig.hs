{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Serv.Server.Core.ServerConfig
    ( ServerConfig(..)
    , loadConfig
    , LogConfig(..)
    , LogAppender(..)
    , defaultConfig
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


data LogAppender = None | Console | File
   deriving (Eq, Generic, Show, ToJSON, FromJSON)

data LogConfig = LogConfig
  { appenders :: [LogAppender]
  , request   :: LogAppender
  } deriving (Generic, Show)


data ServerConfig = ServerConfig
  { serverApiPort :: Port
  , serverSysPort :: Port
  , serverName    :: ServerName
  , serverLog     :: LogConfig
  } deriving (Generic, Show)


loadConfig :: IO (Either [String] ServerConfig)
loadConfig = do
  fileConfig <- readConfigFile "serv.yaml"
  let c = defaultConfig <> fileConfig
  putStrLn "Loading configuaration"
  BS.putStrLn (showConfig c)
  return (validateConfig c)

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

validateLog :: LogConfig -> Last RawLogConfig -> Validate LogConfig
validateLog def (Last Nothing)                    = return  def
validateLog _   (Last (Just (RawLogConfig as (Last Nothing)))) = return (LogConfig as None)
validateLog _   (Last (Just (RawLogConfig as (Last (Just r))))) = return (LogConfig as r)

-- Raw Config file handling

data RawLogConfig = RawLogConfig [LogAppender] (Last LogAppender)
  deriving (Eq, Show, Generic)

instance Semigroup RawLogConfig where
  (<>) = gmappend

instance Monoid RawLogConfig where
  mempty  = gmempty
  mappend = (<>)

instance ToJSON RawLogConfig where
  toJSON (RawLogConfig appenders request) =
    object [ "appenders"    .= appenders
           , "request"      .= request
           ]

instance FromJSON RawLogConfig where
  parseJSON =
    JS.withObject "RawLogConfig" $ \o ->
      RawLogConfig <$> (o .: "appenders")
                   <*> (Last <$> (o .:? "request"))



data RawConfig = RawConfig
                (Last Port)                   -- API port
                (Last Port)                   -- Management port
                (Last ServerName)             -- server name
                (Last RawLogConfig)
  deriving (Eq, Show, Generic)

-- Default Configuration

defaultConfig :: RawConfig
defaultConfig = RawConfig (Last Nothing) (Last Nothing) (Last (Just "Bear")) (Last (Just(RawLogConfig [Console] (Last (Just Console)))))


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


optionalField :: a -> Last a -> Validate a
optionalField def (Last Nothing)  = return def
optionalField _   (Last (Just p)) = return p

requiredField :: String -> Last a -> Validate a
requiredField errmsg (Last Nothing)  = fail errmsg
requiredField _      (Last (Just p)) = return p



{-

getEnvVar :: Read a => String -> a -> IO a
getEnvVar name def = do
    maybeValue <- lookupEnv name
    return $ maybe def read maybeValue

  -}
