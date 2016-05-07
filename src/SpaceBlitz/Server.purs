module SpaceBlitz.Server where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Printer (printJson)
import Data.Either (either)

import SpaceBlitz.Dgram (UDP, createSocket, send)
import SpaceBlitz.Message (ClientMessage(..), ServerMessage(..), parseEvent)

type Config = {
  port :: Int,
  host :: String,
  hostPort :: Int
}

config :: Config
config = {
  port: 3001,
  host: "127.0.0.1",
  hostPort: 3001
}

handleMessage :: forall eff. ServerMessage -> Eff (console :: CONSOLE, udp :: UDP | eff) Unit
handleMessage msg = log $ show msg
  
main :: forall eff. Eff (console :: CONSOLE, udp :: UDP| eff) Unit
main = do 
  socket <- createSocket config.port (parseEvent >>> either log handleMessage)
  send socket config.host config.hostPort (printJson $ encodeJson Ping)
  return unit
