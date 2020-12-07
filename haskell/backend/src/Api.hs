module Api where

import Control.Monad.IO.Class ( liftIO )
import qualified Data.Text as T
import Data.Proxy
import Database.PostgreSQL.Simple
import Polysemy
import Polysemy.Reader
import Polysemy.Fail
import Polysemy.Error
import Servant

import Db
import Model
import Utils

type TodoApi = "hstodos"  :>
  (    Summary "Insert Todo" :> ReqBody '[JSON] TodoAction :> Post '[JSON] Int
  :<|> Summary "Get Todo" :> Capture "todoId" Int :> Get '[JSON] TodoResponse
  :<|> Summary "Get All Todo" :> Get '[JSON] [TodoResponse]
  :<|> Summary "Update Todo" :> Capture "todoId" Int :> ReqBody '[JSON] TodoAction :> Patch '[JSON] ()
  :<|> Summary "Delete Todo"     :> Capture "todoId" Int :> Delete '[JSON] ()
  :<|> Summary "Delete All Todo" :>  Delete '[JSON] ()
  )

insertTodo :: Member DbCrud r => TodoAction -> Sem r Int
insertTodo ta = insert ta

getTodo' :: Members '[ Error T.Text, DbCrud ] r => Int -> Sem r TodoResponse
getTodo' tId = do
  getTodo tId  >>= \case
    Nothing -> throw @T.Text ""
    Just h  -> return (mkTodoResponse h)

getTodos :: Member DbCrud r => Sem r [TodoResponse]
getTodos = do
  todos <- getAll
  return (fmap mkTodoResponse todos)

updateTodo :: Members '[ Error T.Text, DbCrud ] r => Int -> TodoAction -> Sem r ()
updateTodo tid ta = update tid ta

deleteTodo :: Member DbCrud r => Int -> Sem r ()
deleteTodo tid = delete tid

deleteTodos :: Member DbCrud r => Sem r ()
deleteTodos = do
  _ <- deleteAll
  return ()

todoApi :: Members '[DbCrud, Error T.Text] r
        => ServerT TodoApi (Sem r)
todoApi =
  insertTodo :<|> getTodo' :<|> getTodos :<|> updateTodo :<|> deleteTodo :<|> deleteTodos

type AppEffects
  = DbCrud : Error T.Text : Fail : Reader Connection : Embed IO : '[]

toHandler :: Connection -> Sem AppEffects a -> Handler a
toHandler conn app = do
  res <- liftIO $ runM
    ( runReader conn
    . runFail
    . runError
    . runPostgres
    $ app
    )
  case res of
    Left  _err -> throwError err500
    Right r    -> case r of
      Left  _err -> throwError err404
      Right s    -> return s

createApp :: Connection -> Application
createApp conn = allowCors $ allowOptions $ serve (Proxy :: Proxy TodoApi)
                       (hoistServer (Proxy :: Proxy TodoApi) (toHandler conn) todoApi)
