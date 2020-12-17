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
import qualified Data.List as L
import           Data.Bool
import qualified Data.Text as Text
import qualified Network.URI as Network
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
import           Control.Monad.State (get, gets)
import           Servant.API
import           Servant.Client.JSaddle  as S hiding (Client)

import           Model

serverRootPath :: String
serverRootPath = "http://localhost:" ++ testPort ++ "/"

testPort :: String
testPort = "8182"

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

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp app =
    Warp.runSettings (Warp.setPort 8080 (Warp.setTimeout 3600 Warp.defaultSettings)) =<<
      JSaddle.jsaddleOr defaultConnectionOptions (app >> syncPoint) JSaddle.jsaddleApp

servantErrorToS ::
  forall a b.
  ClientM b ->
  JSM (Either String b)
servantErrorToS c = do
  bUrl <- serverBaseUrl
  S.runClientM c (mkClientEnv bUrl) >>= \case
    Right x -> return $ Right x
    Left e -> return . Left $ show e

runClientIO ::
  (Either String b -> msg) ->
  ClientM b ->
  Transition msg model ()
runClientIO constructor c = scheduleIO $ do
  constructor <$>  (servantErrorToS c)

serverBaseUrl :: JSM BaseUrl
serverBaseUrl = parseBaseUrl serverRootPath
#else

servantErrorToS ::
  forall a b.
  ClientM b ->
  IO (Either MisoString b)
servantErrorToS c = do
  bUrl <- serverBaseUrl
  S.runClientM c (mkClientEnv bUrl) >>= \case
    Right x -> return $ Right x
    Left e -> return . Left $ MS.toMisoString (show e)

runClientIO ::
  (Either MisoString b -> msg) ->
  ClientM b ->
  Transition msg model ()
runClientIO constructor c = scheduleIO $ do
  constructor <$>  (servantErrorToS c)

serverBaseUrl :: IO BaseUrl
serverBaseUrl = parseBaseUrl serverRootPath
#endif

-- | Entry point for a miso application
#ifndef __GHCJS__
main :: IO ()
main = runApp $ startApp App {..}
#else
main :: JSM ()
main = miso $ \networkURI -> App {..}
#endif
  where
    initialAction = ListTodos     -- initial action to be executed on application load
    model  = emptyModel           -- initial model
    update = fromTransition . updateModel -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

-- | Type synonym for an application model
data Model = Model
  { listTodo :: [TodoResponse]
  , field :: MisoString
  }
  deriving (Eq, Show)

lensTodos :: Lens' Model [TodoResponse]
lensTodos = lens listTodo $ \model newTodo -> model { listTodo = newTodo }

lensInput :: Lens' Model MisoString
lensInput = lens field $ \model newField -> model { field = newField }

emptyModel :: Model
emptyModel = Model
  { listTodo = []
  , field = mempty
  }

-- | Sum type for application events
data Msg
  = NoOp
  | ListTodos
#ifndef __GHCJS__
  | UpdateTodos (Either String [TodoResponse])
#else
  | UpdateTodos (Either MisoString [TodoResponse])
#endif
  | UpdateField MisoString
--   | EditingEntry Int Bool
--   | UpdateEntry Int TodoResponse
  | Add
  | Delete Int
--   | DeleteComplete
--   | Check Int Bool
--   | CheckAll Bool
--   | ChangeVisibility TodoResponse
  deriving (Show)

-- | Updates model, optionally introduces side effects
updateModel :: Msg -> Transition Msg Model ()
updateModel = \case
  ListTodos -> runClientIO UpdateTodos getTodos
  (UpdateTodos (Left _)) -> scheduleIO_ $ consoleLog "UpdateField: Cannot Get List"
  (UpdateTodos (Right trs)) -> lensTodos .= trs

  UpdateField str -> lensInput .= str
  Add -> do
    mdl <- get
    let
      newInput = field mdl
      newList  = listTodo mdl
    _ <- scheduleIO_ $ do
        _ <- servantErrorToS (insertTodo $ TodoAction { actTitle = Just (MS.fromMisoString  newInput), actCompleted = Just False, actOrder = Just ((L.length newList) + 1) } )
        pure ()
    lensInput .= mempty
    scheduleIO $ pure ListTodos

  Delete tid -> do
    mdl <- get
    let
      findTodo = L.find (\x -> (trid x) == tid) (listTodo mdl)
    case findTodo of
      Nothing -> scheduleIO_ $ consoleLog "Delete: Cannot Delete Todo"
      Just todoR -> do
        _ <- scheduleIO_ $ do
          _ <- servantErrorToS (deleteTodo $ trid todoR)
          pure ()
        scheduleIO $ pure ListTodos
--   DeleteComplete ->
--   Check id check ->
--   CheckAll checkAll ->
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
        [ viewInput m field
        , viewTodos listTodo
        -- , viewControls m visibility entries
        ]
    , infoFooter
    , link_
        [ rel_ "stylesheet"
        , href_ "https://numtide.github.io/numstyle/numstyle.css?v=369851d72145ab73c473c6f31adae046ec155d14"
        ]
    ]

viewInput :: Model -> MisoString -> View Msg
viewInput _ task =
  header_ [ class_ "header m-6 d-inline-flex flex-row width-auto flex-justify-between flex-wrap" ]
    [ h1_
      [ class_ "h1 m-6 p-6 text-bold text-center my-lg-2 width-full" ]
      [ text "TodoMVC - Haskell" ]
    , input_
        [ class_ "new-todo form-control width-full"
        , placeholder_ "What needs to be done?"
        , autofocus_ True
        , value_ (MS.ms task)
        , name_ "newTodo"
        , onInput UpdateField
        , onEnter Add
        ]
    ]

onEnter :: Msg -> Attribute Msg
onEnter action =
  onKeyDown $ bool NoOp action . (== KeyCode 13)

viewTodos :: [ TodoResponse ] -> View Msg
viewTodos todos =
  section_
    [ class_ "main my-2 mx-6 d-inline-flex flex-column width-full"
    -- , style_ $ M.singleton "visibility" cssVisibility
    ]
    [ ul_ [ class_ "todo-list list-style-none" ] $
        map viewKeyedTodoResponse todos
        -- flip (MS.map todos) $ \t ->
        --   viewKeyedTodoResponse t
    ]
--   where
--     cssVisibility = bool "visible" "hidden" (MS.null todos)

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
            [ class_ "toggle form-checkbox m-2"
            , type_ "checkbox"
            , checked_ trcompleted
            -- , onClick $ Check eid (not trcompleted)
            ]
        , label_
            [ class_ "h4 m-1"]
            [ text (pack $ show trtitle) ]
        , button_
            [ class_ "destroy"
            , onClick $ Delete trid
            ] []
        ]
    -- , input_
    --     [ class_ "edit"
    --     , value_ (MS.ms trtitle)
    --     , name_ "title"
    --     , id_ $ "todo-" <> MS.ms trid
    --     , onInput $ UpdateEntry eid
    --     , onBlur $ EditingEntry eid False
    --     , onEnter $ EditingEntry eid False
    --     ]
    ]

infoFooter :: View Msg
infoFooter =
    footer_ [ class_ "info" ]
    [ p_ []
        [ text "Written by "
        , a_ [ href_ "https://github.com/numtide" ] [ text "Numtide" ]
        ]
    ]
