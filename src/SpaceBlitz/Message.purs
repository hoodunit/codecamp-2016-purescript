module SpaceBlitz.Message 
  ( Message(FooMessage, BarMessage)
  , parseEvent
  , parseMessage
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Argonaut.Core (fromObject, fromString, toObject, toString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (bimap)
import Data.Either (Either(Left, Right))
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.StrMap as M
import Data.Tuple (Tuple(Tuple))

import SpaceBlitz.Dgram (UdpEvent(MessageEvent))
  
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
parseMessage raw = (bimap errorMsg id $ jsonParser raw) >>= decodeJson
  where errorMsg msg = "Error parsing Json from message '" ++ raw ++ "': " ++ msg
