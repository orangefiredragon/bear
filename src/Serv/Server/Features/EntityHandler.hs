{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Serv.Server.Features.EntityHandler
    ( handleDeleteEntity
    , handleGetEntity
    , handlePostEntity
    , handlePutEntity
    ) where

import           Control.Monad.IO.Class
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text.Lazy             as LT
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int (decimal)
import           Serv.Api.EntityApi
import           Serv.Api.EntityTypes
import           Serv.Api.Types
import           Serv.Server.ServerEnv
import           Servant
import           System.Log.FastLogger      (toLogStr)

--handlePostEntity :: ServerEnv -> PostEntitiesRequest -> Handler (Response Entity)
handlePostEntity serverEnv@ServerEnv{..} req = do
  let eId@(EntityId idValue) = EntityId 12345
  let location = toLazyText $ fromText entityV1ApiRoot <> decimal idValue

  return $ addHeader location $ success (Entity eId "Lala")

handleGetEntity :: ServerEnv -> EntityId -> Handler (Response Entity)
handleGetEntity serverEnv@ServerEnv{..} eId = do
  liftIO (log (toLogStr ("aaa"::Text)))
  return $ success (Entity eId "Lala")

handlePutEntity :: ServerEnv-> EntityId -> PutEntitiesRequest -> Handler (Response Entity)
handlePutEntity serverEnv@ServerEnv{..} eId req = return $ success (Entity eId "Lala")

handleDeleteEntity :: ServerEnv -> EntityId -> Handler (Response LT.Text)
handleDeleteEntity serverEnv@ServerEnv{..} eId = return successNoBody
