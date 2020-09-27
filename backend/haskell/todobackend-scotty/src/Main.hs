{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class (liftIO)
import qualified Database.Persist.Sqlite as Sqlite
import Network.HTTP.Types.Status (status404)
import System.Environment
import Web.Scotty
import Web.PathPieces

import TodoBackend.Model
import TodoBackend.Utils

json' :: String -> Sqlite.Entity Todo -> ActionM ()
json' url = json . mkTodoResponse url

jsonList :: String -> [Sqlite.Entity Todo] -> ActionM ()
jsonList url = json . map (mkTodoResponse url)

main :: IO ()
main = do
  runDb $ Sqlite.runMigration migrateAll
  port <- read <$> getEnv "PORT"
  url <- getEnv "URL"
  scotty port $ do
    middleware allowCors
    middleware allowOptions
    get "/todos" $ do
      todos <- liftIO readTodos
      jsonList url todos
    get "/todos/:id" $ do
      pid <- param "id"
      actionOr404 pid (\tid -> do
                Just todo <- liftIO $ readTodo tid
                json' url (Sqlite.Entity tid todo))
    patch "/todos/:id" $ do
      pid <- param "id"
      actionOr404 pid (\tid -> do
                          todoAct <- jsonData
                          let todoUp = actionToUpdates todoAct
                          todo <- liftIO $ runDb $ Sqlite.updateGet tid todoUp
                          json' url (Sqlite.Entity tid todo))
    delete "/todos/:id" $ do
      pid <- param "id"
      actionOr404 pid (liftIO . deleteTodo)
    post "/todos" $ do
      todoAct <- jsonData
      let todo = actionToTodo todoAct
      tid <- liftIO $ insertTodo todo
      json' url (Sqlite.Entity tid todo)
    delete "/todos" $ liftIO $ runDb $ Sqlite.deleteWhere ([] :: [Sqlite.Filter Todo])
  where
    readTodos :: IO [Sqlite.Entity Todo]
    readTodos =  runDb $ Sqlite.selectList [] []

    readTodo :: Sqlite.Key Todo -> IO (Maybe Todo)
    readTodo tid = runDb $ Sqlite.get tid

    deleteTodo :: Sqlite.Key Todo -> IO ()
    deleteTodo tid = runDb $ Sqlite.delete tid

    insertTodo :: Todo -> IO (Sqlite.Key Todo)
    insertTodo todo = runDb $ Sqlite.insert todo

    actionOr404 pid action = case fromPathPiece pid of
            Nothing -> status status404
            Just tid -> action tid
