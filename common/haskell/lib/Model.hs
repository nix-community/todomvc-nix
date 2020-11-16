module Model where

import Control.Monad

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as Text

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
      , "orderx"    .= trorder
      ]
    where
      noNullsObject = object . filter notNull
      notNull (_, Null) = False
      notNull _         = True

instance FromJSON TodoResponse where
  parseJSON (Object o) = TodoResponse
    <$> o .: "id"
    <*> o .: "title"
    <*> o .: "completed"
    <*> o .: "orderx"
  parseJSON _ = mzero

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

