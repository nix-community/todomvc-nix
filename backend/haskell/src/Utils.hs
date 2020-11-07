module Utils where

import Data.Maybe (fromMaybe)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Middleware.AddHeaders

import Db
import Model

allowCors :: Middleware
allowCors = addHeaders [
    ("Access-Control-Allow-Origin", "*"),
    ("Access-Control-Allow-Headers", "Accept, Content-Type"),
    ("Access-Control-Allow-Methods", "GET, HEAD, POST, DELETE, OPTIONS, PUT, PATCH")
  ]

allowOptions :: Middleware
allowOptions app req resp = case requestMethod req of
  "OPTIONS" -> resp $ responseLBS status200 [] "Ok"
  _         -> app req resp

mkTodoResponse :: Todo -> TodoResponse
mkTodoResponse Todo { .. } = TodoResponse trId trTitle trCompleted trOrder
  where
    trId = fromMaybe 0 tId
    trTitle = fromMaybe "" tTitle
    trCompleted = fromMaybe False tCompleted
    trOrder = fromMaybe 0 tOrder