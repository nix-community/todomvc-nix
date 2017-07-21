{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad (msum)
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Database.Persist.Sqlite as Sqlite
import Happstack.Server
import System.Environment
import Web.PathPieces

import TodoBackend.Model

data Config = Config String

main :: IO ()
main = do
  runDb $ Sqlite.runMigration migrateAll
  p <- read <$> getEnv "PORT"
  url <- getEnv "URL"
  let conf = Config url
  simpleHTTP (nullConf { port = p}) $ flip runReaderT conf $ do
    mapM_ (uncurry setHeaderM) [ ("Access-Control-Allow-Origin", "*")
                               ,  ("Access-Control-Allow-Headers", "Accept, Content-Type")
                               ,  ("Access-Control-Allow-Methods", "GET, HEAD, POST, DELETE, OPTIONS, PUT, PATCH")
                               ]
    msum [ method OPTIONS >>= \_ -> ok $ toResponse ("ok" :: String)
         , todosByIdApi
         , todosListApi
         ]

type App a = ReaderT Config (ServerPartT IO) a

getBody :: App L.ByteString
getBody = do
  req <- askRq
  mbody <- liftIO $ takeRequestBody req
  case mbody of
    Nothing -> return ""
    Just rb -> return $ unBody rb

getTodoActionBody :: App (Maybe TodoAction)
getTodoActionBody = decode <$> getBody

jsonResponse :: ToJSON a => a -> App Response
jsonResponse = ok . toResponseBS "application/json" . encode

todoResponse :: Sqlite.Entity Todo -> App Response
todoResponse e = do
  Config url <- ask
  jsonResponse . mkTodoResponse url $ e

todosByIdApi :: App Response
todosByIdApi = dir "todos" $ path $ \tid ->
    case fromPathPiece tid of
        Nothing -> badRequest $ toResponse ("Invalid id" :: String)
        Just tid' -> msum  [ getTodo tid'
                           , patchTodo tid'
                           , deleteTodo tid'
                           ]
  where
   getTodo :: TodoId -> App Response
   getTodo tid = do
     method GET
     todoM <- liftIO $ readTodo tid
     case todoM of
       Nothing -> notFound $ toResponse ("Id not found" :: String)
       Just todo -> todoResponse $ Sqlite.Entity tid todo
   patchTodo :: TodoId -> App Response
   patchTodo tid = do
     method PATCH
     mtact <- getTodoActionBody
     case mtact of
       Nothing -> badRequest $ toResponse ("Invalid request body" :: String)
       Just tact -> do
         let todoUp = actionToUpdates tact
         todo <- liftIO $ runDb $ Sqlite.updateGet tid todoUp
         todoResponse (Sqlite.Entity tid todo)

   deleteTodo :: TodoId -> App Response
   deleteTodo tid = do
     method DELETE
     liftIO $ runDb $ Sqlite.delete tid
     ok $ toResponse ("deleted" :: String)


readTodo :: TodoId -> IO (Maybe Todo)
readTodo tid = runDb $ Sqlite.get tid

todosListApi :: App Response
todosListApi = dir "todos" $
    msum [ getAll
         , create
         , delete
         ]
  where
    getAll = do
      method GET
      todos <- liftIO $ runDb $ Sqlite.selectList [] ([] :: [Sqlite.SelectOpt Todo])
      Config url <- ask
      let todosResp = map (mkTodoResponse url) todos
      jsonResponse todosResp
    create = do
      method POST
      mtact <- getTodoActionBody
      case mtact of
        Nothing -> badRequest $ toResponse ("bad request" :: String)
        Just tact -> do
          let todo = actionToTodo tact
          tid <- liftIO $ runDb $ Sqlite.insert todo
          todoResponse $ Sqlite.Entity tid todo
    delete :: App Response
    delete = do
      method DELETE
      liftIO $ runDb $ Sqlite.deleteWhere ([] :: [Sqlite.Filter Todo])
      ok $ toResponse ("deleted" :: String)
