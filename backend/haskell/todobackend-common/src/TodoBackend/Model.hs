{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module TodoBackend.Model where

import Control.Monad (mzero)
import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Data.Aeson
import Data.Aeson.TH
import Data.Maybe (fromMaybe)
import qualified Database.Persist.Sqlite as Sqlite
import qualified Data.Text as Text
import Database.Persist.TH
import Web.PathPieces

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Todo
    title String
    completed Bool
    order Int
    deriving Show
|]

data TodoResponse = TodoResponse
  { trid        :: TodoId
  , trurl       :: String
  , trtitle     :: String
  , trcompleted :: Bool
  , trorder     :: Int
  } deriving (Show)

$(deriveToJSON defaultOptions { fieldLabelModifier = drop 2}
  ''TodoResponse)

mkTodoResponse :: String -> Sqlite.Entity Todo -> TodoResponse
mkTodoResponse rootUrl (Sqlite.Entity key Todo{..}) =
    TodoResponse key todoUrl todoTitle todoCompleted todoOrder
  where
    todoUrl = rootUrl ++ "/todos/" ++ Text.unpack (toPathPiece key)

data TodoAction = TodoAction
  { actTitle :: Maybe String
  , actCompleted :: Maybe Bool
  , actOrder :: Maybe Int
  } deriving Show

instance FromJSON TodoAction where
  parseJSON (Object o) = TodoAction
    <$> o .:? "title"
    <*> o .:? "completed"
    <*> o .:? "order"
  parseJSON _ = mzero

instance ToJSON TodoAction where
  toJSON (TodoAction mTitle mCompl mOrder) = noNullsObject
      [ "title"     .= mTitle
      , "completed" .= mCompl
      , "order"     .= mOrder
      ]
    where
      noNullsObject = object . filter notNull
      notNull (_, Null) = False
      notNull _         = True

actionToTodo :: TodoAction -> Todo
actionToTodo (TodoAction mTitle mCompleted mOrder) = Todo title completed order
  where
    title     = fromMaybe "" mTitle
    completed = fromMaybe False mCompleted
    order     = fromMaybe 0 mOrder

actionToUpdates :: TodoAction -> [Sqlite.Update Todo]
actionToUpdates act =  updateTitle
                    ++ updateCompl
                    ++ updateOrd
  where
    updateTitle = maybe [] (\title -> [TodoTitle Sqlite.=. title])
                  (actTitle act)
    updateCompl = maybe [] (\compl -> [TodoCompleted Sqlite.=. compl])
                  (actCompleted act)
    updateOrd = maybe [] (\ord -> [TodoOrder Sqlite.=. ord])
                  (actOrder act)

runDb :: Sqlite.SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb = runNoLoggingT . runResourceT . Sqlite.withSqliteConn "dev.sqlite3" . Sqlite.runSqlConn
