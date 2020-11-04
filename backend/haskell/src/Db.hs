{-# LANGUAGE TemplateHaskell #-}
module Db where

import Data.Maybe

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Model
import Polysemy
import           Polysemy.Fail
import           Polysemy.Reader

-- TODO: should look into this https://funprog.srid.ca/polysemy/interpreter-for-action-that-adds-on-the-effect-stack.html#212801533
data DbCrud m a where
  Insert :: TodoAction -> DbCrud m Int
  GetTodo :: Int -> DbCrud m (Maybe Todo)
  GetAll :: DbCrud m [ Todo ]
  Update :: Int -> TodoAction -> DbCrud m ()
  Delete :: Int -> DbCrud m ()
  DeleteAll :: DbCrud m ()

makeSem ''DbCrud

runPostgres
  :: Members '[Fail, Reader Connection, Embed IO] r
  => Sem (DbCrud : r) a
  -> Sem r a
runPostgres = interpret $ \x -> do
  conn :: Connection <- ask
  case x of
    Insert TodoAction {..} -> do
      [Only id] <- embed $ returning
        conn
        [sql|INSERT INTO todos (title, completed, orderx)
              VALUES (?, ?, ?)
              RETURNING id
        |]
        [(actTitle, actCompleted, actOrder)]
      return id
    GetTodo tId -> do
      res <- embed $ query
        conn
        "SELECT * FROM todos WHERE id = ?"
        (Only tId)
      return (listToMaybe res)
    GetAll -> embed $ query_
      conn
      "SELECT * FROM todos"
    Update tId (TodoAction {..}) -> do
      _ <- embed $ execute
        conn
        "UPDATE todos SET (title, completed, orderx) = (?,?,?) WHERE id = ? RETURNING id"
        (actTitle, actCompleted, actOrder, tId)
      return ()
    Delete tId -> do
      _ <- embed
        $ execute conn "DELETE FROM todos WHERE id = ?" (Only tId)
      return ()
    DeleteAll -> do
      _ <- embed
        $ execute_ conn "DELETE FROM todos where completed = true"
      return ()
