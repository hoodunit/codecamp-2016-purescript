module SpaceBlitz.Server where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE(), log)

import SpaceBlitz.Dgram (UdpEvent, UDP, createSocket)

type Config = {
  port :: Int
}

config :: Config
config = {
  port: 3001
}

logEvent :: forall e. UdpEvent -> Eff (console :: CONSOLE | e) Unit
logEvent msg = log $ show msg

main :: forall e. Eff (console :: CONSOLE, udp :: UDP| e) Unit
main = do 
  socket <- createSocket config.port logEvent
  return unit
