module TodoMain where

import System.Environment
import Network.Wai.Handler.Warp
import Network.Wai.Logger             ( withStdoutLogger )
import Api (createApp)
import Data.Function
import Database.PostgreSQL.Simple
import UnliftIO.Exception

main :: IO ()
main = do
  pgUser <- getEnv "PGUSER"
  pgPass <- getEnv "PGPASSWORD"
  pgDb <- getEnv "PGDATABASE"
  bracket (connect defaultConnectInfo { connectUser = pgUser, connectPassword = pgPass, connectDatabase = pgDb }) close
    $ \conn -> withStdoutLogger $ \logger -> do
      let settings = defaultSettings & setPort 8181 & setLogger logger
      putStrLn "Server starting on port 8181..."
      runSettings settings (createApp conn)
