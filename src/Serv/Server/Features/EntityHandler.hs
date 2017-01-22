{-# LANGUAGE OverloadedStrings #-}

module Serv.Server.Features.EntityHandler
    ( handleDeleteEntity
    , handleGetEntity
    , handlePostEntity
    , handlePutEntity
    ) where

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

-- handlePostEntity :: ServerEnv -> PostEntitiesRequest -> Handler (Response Entity)
handlePostEntity serverEnv req = do
  let eId@(EntityId idValue) = EntityId 12345
  let location = toLazyText $ fromText entityV1ApiRoot <> decimal idValue
  return $ addHeader location $ success (Entity eId "Lala")

handleGetEntity :: ServerEnv -> EntityId -> Handler (Response Entity)
handleGetEntity serverEnv eId = return $ success (Entity eId "Lala")

handlePutEntity :: ServerEnv-> EntityId -> PutEntitiesRequest -> Handler (Response Entity)
handlePutEntity serverEnv eId req = return $ success (Entity eId "Lala")

handleDeleteEntity :: ServerEnv -> EntityId -> Handler (Response LT.Text)
handleDeleteEntity serverEnv eId = return successNoBody
