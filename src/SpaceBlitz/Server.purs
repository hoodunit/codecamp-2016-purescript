module SpaceBlitz.Server where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Argonaut.Core (fromObject, fromString, toObject, toString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Printer (printJson)
import Data.Either (Either(Left, Right))
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.StrMap as M
import Data.Tuple (Tuple(Tuple))

import SpaceBlitz.Dgram (UdpEvent(MessageEvent), UDP, createSocket, send)

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
  
data Message = FooMessage | BarMessage
derive instance genericMessage :: Generic Message
instance showMessage :: Show Message where
  show = gShow
  
instance decodeJsonMessage :: DecodeJson Message where
  decodeJson json = maybe (Left $ "Failed to parse message: " ++ show json) Right $ do
    obj <- toObject json
    msgType <- M.lookup "msgType" obj >>= toString
    case msgType of
      "foo" -> Just FooMessage
      "bar" -> Just BarMessage
      _ -> Nothing
      
instance encodeJsonMessage :: EncodeJson Message where
  encodeJson FooMessage = fromObject $ M.fromFoldable [Tuple "msgType" $ fromString "foo"]
  encodeJson BarMessage = fromObject $ M.fromFoldable [Tuple "msgType" $ fromString "bar"]

logMessage :: forall e. Message -> Eff (console :: CONSOLE | e) Unit
logMessage msg = log $ show msg

parseEvent :: UdpEvent -> Either String Message
parseEvent (MessageEvent msg) = parseMessage msg
parseEvent event = Left $ "Could not parse event: '" ++ show event ++ "'"

parseMessage :: String -> Either String Message
parseMessage raw = jsonParser raw >>= decodeJson

main :: forall e. Eff (console :: CONSOLE, udp :: UDP| e) Unit
main = do 
  socket <- createSocket config.port (parseEvent >>> show >>> log)
  send socket config.host config.hostPort (printJson $ encodeJson FooMessage)
  return unit
