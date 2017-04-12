{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Serv.Server.Core.Config.ConfigLoader
    ( loadConfig
    ) where

import           Control.Exception
import qualified Data.Aeson                             as JS
import qualified Data.ByteString.Char8                  as BS
import qualified Data.ByteString.Lazy.Char8             as LBS
import           Data.Monoid                            hiding ((<>))
import           Data.Semigroup                         (Semigroup (..))
import           Data.Text                              (Text)
import qualified Data.Yaml                              as Yaml
import qualified Data.Yaml                              as Y
import           GHC.Generics
import           Serv.Server.Core.Config.ConfigData
import           Serv.Server.Core.Config.LogData
import           Serv.Server.Core.Config.ManagementData
import           Serv.Server.Core.Config.ServerData
import           Serv.Server.Core.Config.Types
import           Serv.Util.GenericMonoid
import           Serv.Util.Validate
import           System.Environment                     (lookupEnv)


loadConfig :: IO (Either [String] Config)
loadConfig = do
  fileConfig <- readConfigFile "serv.yaml"
  let c = defaultConfig <> fileConfig
  putStrLn "Loaded configuaration:"
  BS.putStrLn (showConfig c)
  return (validateConfig c)

-- Default Configuration
defaultConfig :: ConfigData
defaultConfig = ConfigData (Last (Just (ServerData (Last (Just 9000)) (Last (Just "Bear"))))) (Last (Just (ManagementData (Last (Just 9060)) (Last (Just "admin")) (Last (Just "secret"))))) (Last (Just (LogData [Console] (Last (Just None)))))

readConfigFile :: FilePath -> IO ConfigData
readConfigFile file = either throwIO return =<< Yaml.decodeFileEither file

showConfig :: ConfigData -> BS.ByteString
showConfig = Yaml.encode

{-
getEnvVar :: Read a => String -> a -> IO a
getEnvVar name def = do
    maybeValue <- lookupEnv name
    return $ maybe def read maybeValue
-}
