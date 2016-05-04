module SpaceBlitz.Server where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Printer (printJson)

import SpaceBlitz.Dgram (UDP, createSocket, send)
import SpaceBlitz.Message (Message(FooMessage), parseEvent)

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
  
main :: forall e. Eff (console :: CONSOLE, udp :: UDP| e) Unit
main = do 
  socket <- createSocket config.port (parseEvent >>> show >>> log)
  send socket config.host config.hostPort (printJson $ encodeJson FooMessage)
  return unit
