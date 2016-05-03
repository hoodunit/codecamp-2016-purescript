module SpaceBlitz.Server where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Exception (Error, message, stack)
import Data.Argonaut.Encode (encodeJson)
import Data.Maybe (fromMaybe)
import Node.Express.App (App, get, listenHttp, use, useOnError)
import Node.Express.Handler (Handler, next)
import Node.Express.Request (getPath)
import Node.Express.Response (sendJson, setStatus)
import Node.Express.Types (EXPRESS)

logger :: forall e. Handler (console :: CONSOLE | e)
logger = do
  url <- getPath
  liftEff $ log url
  next

errorHandler :: forall e. Error -> Handler (console :: CONSOLE | e)
errorHandler err = do
  setStatus 400
  liftEff $ log $ showError err
  sendJson $ encodeJson $ message err
  
showError :: Error -> String
showError e = (message e) ++ "\n" ++ (fromMaybe "" $ stack e)

app :: forall e. App (console :: CONSOLE | e)
app = do
  liftEff $ log "Starting server..."
  use logger
  get "/" $ sendJson "hello"
  useOnError errorHandler

main :: forall e. Eff (console :: CONSOLE, express :: EXPRESS| e) Unit
main = do 
  listenHttp app port \_ -> log $ "Started server on localhost:" ++ show port
  return unit
  where port = 3001
