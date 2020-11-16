-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import           Miso
import           Miso.String as MS hiding (map)
import           Control.Lens
import           Data.Proxy
import qualified Data.Map as M
import           Data.Bool
-- | JSAddle import
#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
import Network.HTTP.Proxy hiding (Proxy)
import qualified Network.HTTP.Proxy as Proxy
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp         as Warp
import Network.WebSockets (defaultConnectionOptions)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
#endif
import           Control.Monad.IO.Class
import           Servant.API
import           Servant.Client.JSaddle  as S hiding (Client)

import           Model

serverRootPath :: String
serverRootPath = "http://localhost:" ++ testPort ++ "/"

testPort :: String
testPort = "8186"

type TodoApi = "hstodos"  :>
  (    Summary "Insert Todo" :> ReqBody '[JSON] TodoAction :> Post '[JSON] Int
  :<|> Summary "Get Todo" :> Capture "todoId" Int :> Get '[JSON] TodoResponse
  :<|> Summary "Get All Todo" :> Get '[JSON] [TodoResponse]
  :<|> Summary "Update Todo" :> Capture "todoId" Int :> ReqBody '[JSON] TodoAction :> Patch '[JSON] ()
  :<|> Summary "Delete Todo"     :> Capture "todoId" Int :> Delete '[JSON] ()
  :<|> Summary "Delete All Todo" :>  Delete '[JSON] ()
  )

api :: Proxy TodoApi
api = Proxy

insertTodo :: TodoAction -> ClientM Int
getTodo' :: Int -> ClientM  TodoResponse
getTodos :: ClientM [TodoResponse]
updateTodo :: Int -> TodoAction -> ClientM ()
deleteTodo :: Int -> ClientM ()
deleteTodos :: ClientM ()

insertTodo
  :<|> getTodo'
  :<|> getTodos
  :<|> updateTodo
  :<|> deleteTodo
  :<|> deleteTodos = client api

runClientIO ::
  (Either String b -> msg) ->
  ClientM b ->
  Transition msg model ()
runClientIO constructor c = scheduleIO $ do
  constructor <$>  (servantErrorToS c)

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f =
    Warp.runSettings (Warp.setPort 8080 (Warp.setTimeout 3600 Warp.defaultSettings)) =<<
      JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) JSaddle.jsaddleApp
--   proxyApp <- httpProxyApp proxySettings <$> newManager defaultManagerSettings
--   jsaddleApp <-
--     JSaddle.jsaddleOr defaultConnectionOptions (f >> Miso.syncPoint) proxyApp
--   Warp.runSettings settings jsaddleApp
--   where
--     settings = Warp.setPort 8080
--              . Warp.setTimeout 3600
--              $ Warp.defaultSettings
--     proxySettings = defaultProxySettings
--        { proxyPort = 8189
--        , proxyRequestModifier = rewrite
--        }
--     path req = "http://localhost:8186" <> Proxy.requestPath req
--     rewrite req = return $ Right req {Proxy.requestPath = path req}
    -- f = do
    --   uri <- Miso.getCurrentURI
    --   a <- app
    --   Miso.startApp $ a uri

--   Warp.runSettings (Warp.setPort 8080 (Warp.setTimeout 3600 Warp.defaultSettings)) =<<
--     JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) JSaddle.jsaddleApp

servantErrorToS ::
  forall a b.
  ClientM b ->
  JSM (Either String b)
servantErrorToS c = do
  bUrl <- serverBaseUrl
  S.runClientM c (mkClientEnv bUrl) >>= \case
    Right x -> return $ Right x
    Left x -> return . Left $ show x

serverBaseUrl :: JSM BaseUrl
serverBaseUrl = parseBaseUrl serverRootPath
#else
runApp :: IO () -> IO ()
runApp app = miso =<< app

servantErrorToS ::
  forall a b.
  ClientM b ->
  IO (Either MisoString b)
servantErrorToS c = do
  bUrl <- serverBaseUrl
  S.runClientM c (mkClientEnv bUrl) >>= \case
    Right x -> return $ Right x
    Left x -> return . Left $ x

serverBaseUrl :: IO BaseUrl
serverBaseUrl = parseBaseUrl serverRootPath
#endif

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = ListTodos -- initial action to be executed on application load
    model  = emptyModel                   -- initial model
    update = fromTransition . updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    --logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

-- | Type synonym for an application model
data Model = Model
  { listTodo :: [TodoResponse] }
  deriving (Eq, Show)

updateTodos :: Lens' Model [TodoResponse]
updateTodos = lens listTodo $ \model newTodo -> model { listTodo = newTodo }

emptyModel :: Model
emptyModel = Model
  { listTodo = [] }

-- | Sum type for application events
data Msg
  = NoOp
  | ListTodos
  | UpdateField (Either String [TodoResponse])
--   | EditingEntry Int Bool
--   | UpdateEntry Int TodoResponse
--   | Add
--   | Delete Int
--   | DeleteComplete
--   | Check Int Bool
--   | CheckAll Bool
--   | ChangeVisibility TodoResponse
  deriving (Show)

-- | Updates model, optionally introduces side effects
updateModel :: Msg -> Transition Msg Model ()
updateModel = \case
    ListTodos -> runClientIO UpdateField getTodos
    (UpdateField (Left _)) -> scheduleIO_ $ consoleLog "UpdateField: Cannot Get List"
    (UpdateField (Right trs)) -> updateTodos .= trs
--   EditingEntry id editing ->
--   UpdateEntry id tr ->
--   Add ->
--   Delete id ->
--   DeleteComplete ->
--   Check id check ->
--   CheckAll checkAll ->
--   ChangeVisibility tr ->
    NoOp -> pure ()

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Msg
viewModel m@Model{..} =
 div_
    [ class_ "todomvc-wrapper"
    -- , style_  $ M.singleton "visibility" "hidden"
    ]
    [ section_
        [ class_ "todoapp" ]
        [ viewInput m ""
        , viewTodos listTodo
        -- , viewControls m visibility entries
        ]
    , infoFooter
    , link_
        [ rel_ "stylesheet"
        , href_ "https://numtide.github.io/numstyle/numstyle.css?v=369851d72145ab73c473c6f31adae046ec155d14"
        ]
    ]

viewInput :: Model -> String -> View Msg
viewInput _ task =
  header_ [ class_ "header" ]
    [ h1_ [] [ text "todos" ]
    , input_
        [ class_ "new-todo"
        , placeholder_ "What needs to be done?"
        , autofocus_ True
        , value_ (MS.ms task)
        , name_ "newTodo"
        -- , onInput UpdateField
        -- , onEnter Add
        ]
    ]

onEnter :: Msg -> Attribute Msg
onEnter action =
  onKeyDown $ bool NoOp action . (== KeyCode 13)

viewTodos :: [ TodoResponse ] -> View Msg
viewTodos todos =
  section_
    [ class_ "main"
    -- , style_ $ M.singleton "visibility" cssVisibility
    ]
    [ ul_ [ class_ "todo-list" ] $
      map viewKeyedTodoResponse todos
        -- flip (MS.map todos) $ \t ->
        --   viewKeyedTodoResponse t
    ]
--   where
--     cssVisibility = bool "visible" "hidden" (MS.null entries)

viewKeyedTodoResponse :: TodoResponse -> View Msg
viewKeyedTodoResponse = viewTodoResponse

viewTodoResponse :: TodoResponse -> View Msg
viewTodoResponse TodoResponse {..} = liKeyed_ (toKey trid)
    [ class_ $ MS.intercalate " " $
       [ "completed" | trcompleted ]
    ]
    [ div_
        [ class_ "view" ]
        [ input_
            [ class_ "toggle"
            , type_ "checkbox"
            , checked_ trcompleted
            -- , onClick $ Check eid (not completed)
            ]
        , text $ pack (show trtitle)
        -- label
            -- [ onDoubleClick $ EditingEntry eid True ]
            -- [ text (MS.ms trtitle) ]
        , button_
            [ class_ "destroy"
            -- , onClick $ Delete eid
            ] []
        ]
    , input_
        [ class_ "edit"
        , value_ (MS.ms trtitle)
        , name_ "title"
        , id_ $ "todo-" <> MS.ms trid
        -- , onInput $ UpdateEntry eid
        -- , onBlur $ EditingEntry eid False
        -- , onEnter $ EditingEntry eid False
        ]
    ]

infoFooter :: View Msg
infoFooter =
    footer_ [ class_ "info" ]
    [ p_ [] [ text "Double-click to edit a todo" ]
    , p_ []
        [ text "Written by "
        , a_ [ href_ "https://github.com/numtide" ] [ text "Numtide" ]
        ]
    ]
