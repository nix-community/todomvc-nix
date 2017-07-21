{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Database.Persist.Sqlite as Sqlite
import Snap
import System.Environment

import TodoBackend.Model

data App = App
  { appRoot :: String
  }

writeJSON :: (ToJSON a, MonadSnap m) => a -> m ()
writeJSON j = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ j

getJSON :: FromJSON a => Handler App App (Either String a)
getJSON = do
    bodyVal <- decode `fmap` readRequestBody 50000
    return $ case bodyVal of
      Nothing -> Left "Invalid JSON data in POST body"
      Just v -> case fromJSON v of
                  Error e -> Left e
                  Success a -> Right a

returnJson :: Sqlite.Entity Todo -> Handler App App ()
returnJson todo = do
  url <- gets appRoot
  writeJSON $ mkTodoResponse url todo

jsonList :: [Sqlite.Entity Todo] -> Handler App App ()
jsonList todos = do
  url <- gets appRoot
  writeJSON $ map (mkTodoResponse url) todos

appInit :: SnapletInit App App
appInit = makeSnaplet "todoapp" "Todobackend example" Nothing $ do
    addRoutes [ ("todos", optionsResp)
              , ("todos", todosHandler)
              , ("todos/:todoid", optionsResp)
              , ("todos/:todoid", todoHandler)
              ]
    url <- liftIO $ getEnv "URL"
    return $ App url

allowCors :: Handler App App ()
allowCors = mapM_ (modifyResponse . uncurry setHeader) [
    ("Access-Control-Allow-Origin", "*"),
    ("Access-Control-Allow-Headers", "Accept, Content-Type"),
    ("Access-Control-Allow-Methods", "GET, HEAD, POST, DELETE, OPTIONS, PUT, PATCH")
  ]

optionsResp :: Handler App App ()
optionsResp = method OPTIONS allowCors


todosHandler :: Handler App App ()
todosHandler = do
  allowCors
  req <- getRequest
  case rqMethod req of
   GET -> do
      todos <- liftIO $ runDb $
        Sqlite.selectList [] ([] :: [Sqlite.SelectOpt Todo])
      jsonList todos
   POST -> do
      todoActE <- getJSON
      case todoActE of
        Right todoAct -> do
            let todo = actionToTodo todoAct
            tid <- liftIO $ runDb $ Sqlite.insert todo
            returnJson $ Sqlite.Entity tid todo
        Left _ -> writeBS "error"
   DELETE -> liftIO $ runDb $ Sqlite.deleteWhere ([] :: [Sqlite.Filter Todo])
   _ -> writeBS "error"

todoHandler :: Handler App App ()
todoHandler = do
  allowCors
  Just tidBS <- getParam "todoid"
  req <- getRequest
  case C8.readInteger tidBS of
    Nothing       -> writeBS "error"
    Just (n, _) -> do
        let tid = Sqlite.toSqlKey $ fromIntegral n
        case rqMethod req of
            GET -> do
                Just todo <- liftIO $ runDb $ Sqlite.get tid
                returnJson $ Sqlite.Entity tid todo
            PATCH -> do
                todoActE <- getJSON
                case todoActE of
                  Left _        -> writeBS "error"
                  Right todoAct -> do
                    let todoUp = actionToUpdates todoAct
                    todo <- liftIO $ runDb $ Sqlite.updateGet tid todoUp
                    returnJson $ Sqlite.Entity tid todo
            DELETE -> liftIO $ runDb $ Sqlite.delete tid
            _ -> undefined

main :: IO ()
main = do
  runDb $ Sqlite.runMigration migrateAll
  port <- read <$> getEnv "PORT"
  let config = setPort port defaultConfig
  serveSnaplet config appInit
