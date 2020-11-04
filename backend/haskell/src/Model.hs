module Model where

import Control.Monad

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data TodoResponse = TodoResponse
  { trid        :: Int
  -- , trurl       :: String
  , trtitle     :: String
  , trcompleted :: Bool
  , trorder     :: Int
  } deriving (Show)

instance ToJSON TodoResponse where
  toJSON (TodoResponse trid trtitle trcompleted trorder) = noNullsObject
      [ "id"        .= trid
      , "title"     .= trtitle
      , "completed" .= trcompleted
      , "orderx"     .= trorder
      ]
    where
      noNullsObject = object . filter notNull
      notNull (_, Null) = False
      notNull _         = True

mkTodoResponse :: Todo -> TodoResponse
mkTodoResponse Todo { .. } = TodoResponse trId trTitle trCompleted trOrder
  where
    trId = fromMaybe 0 tId
    trTitle = fromMaybe "" tTitle
    trCompleted = fromMaybe False tCompleted
    trOrder = fromMaybe 0 tOrder

data TodoAction = TodoAction
  { actTitle :: Maybe String
  , actCompleted :: Maybe Bool
  , actOrder :: Maybe Int
  } deriving (Show)

instance FromJSON TodoAction where
  parseJSON (Object o) = TodoAction
    <$> o .:? "title"
    <*> o .:? "completed"
    <*> o .:? "orderx"
  parseJSON _ = mzero

data Todo = Todo
  { tId :: Maybe Int
  , tTitle :: Maybe String
  , tCompleted :: Maybe Bool
  , tOrder :: Maybe Int
  } deriving (Show, Eq, Read)

instance FromRow Todo where
    fromRow = Todo <$> field <*> field <*> field <*> field

