{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
import qualified Database.Persist.Sqlite as Sqlite
import           Network.Wai.Handler.Warp     (run)
import           System.Environment
import           Yesod

import           TodoBackend.Model
import           TodoBackend.Utils


data App = App
  { appRoot :: String
  }

mkYesod "App" [parseRoutes|
/todos         TodosR GET POST  DELETE
/todos/#TodoId TodoR  GET PATCH DELETE
|]

instance Yesod App

json :: Sqlite.Entity Todo -> Handler Value
json ent = do
  App url <- getYesod
  returnJson $ mkTodoResponse url ent

jsonList :: [Sqlite.Entity Todo] -> Handler Value
jsonList ents = do
  App url <- getYesod
  returnJson $ map (mkTodoResponse url) ents

getTodosR :: Handler Value
getTodosR = do
  todos <- liftIO $ runDb $ Sqlite.selectList [] ([] :: [Sqlite.SelectOpt Todo])
  jsonList todos

postTodosR :: Handler Value
postTodosR = do
  todoAct <- requireJsonBody
  let todo = actionToTodo todoAct
  tid <- liftIO $ runDb $ Sqlite.insert todo
  json $ Sqlite.Entity tid todo

deleteTodosR :: Handler ()
deleteTodosR = do
  liftIO $ runDb $ Sqlite.deleteWhere ( [] :: [Sqlite.Filter Todo])
  return ()

getTodoR :: TodoId -> Handler Value
getTodoR tid = do
    todo <- liftIO $ runDb $ get404 tid
    json $ Sqlite.Entity tid todo

patchTodoR :: TodoId -> Handler Value
patchTodoR tid = do
  todoAct <- requireJsonBody
  let todoUp = actionToUpdates todoAct
  todo <- liftIO $ runDb $ Sqlite.updateGet tid todoUp
  json $ Sqlite.Entity tid todo

deleteTodoR :: TodoId -> Handler ()
deleteTodoR tid = do
  liftIO $ runDb $ Sqlite.delete tid
  return ()

mkApp :: Application -> Application
mkApp a = allowCors $ allowOptions a

main :: IO ()
main = do
  runDb $ Sqlite.runMigration migrateAll
  port <- read <$> getEnv "PORT"
  url <- getEnv "URL"
  waiApp <- toWaiApp $ App url
  run port $ mkApp waiApp
