module SpaceBlitz.Message 
  ( ClientMessage(..)
  , ServerMessage(..)
  , GameInitData
  , MissileData
  , FleetDirections
  , FleetStatus
  , parseEvent
  , parseMessage
  ) where

import Prelude
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Argonaut.Core (Json, JArray, JObject, fromObject, fromString, toArray, toObject, toString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, gDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (bimap)
import Data.Either (Either(Left, Right), either)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.StrMap as M
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(Tuple))

import SpaceBlitz.Dgram (UdpEvent(MessageEvent))

data ServerMessage = 
     GameInit GameInitData
   | FleetStatuses (Array FleetStatus)
   | Pong
   
type GameInitData = String
newtype FleetStatus = FleetStatus
  { name :: String
  , status :: Status }
  
data Status = Alive | Destroyed
   
data ClientMessage = 
     Join
   | Ping
   | LaunchMissiles MissileData
   | MoveFleet FleetDirections
   | SelfDestruct
   
type MissileData = String
type FleetDirections = String

derive instance genericStatus :: Generic Status
derive instance genericFleetStatus :: Generic FleetStatus
derive instance genericServerMessage :: Generic ServerMessage
instance showServerMessage :: Show ServerMessage where
  show = gShow
  
instance decodeJsonServerMessage :: DecodeJson ServerMessage where
  decodeJson json = either (Left <<< errorMsg) Right $ do
    obj <- asObject json
    msgType <- obj .? "type"
    case msgType of
      "fleetStatuses" -> decodeFleetStatuses obj
      _ -> Left "Fail"
   where
     errorMsg :: String -> String
     errorMsg failMsg = "Failed to parse server message '" ++ show json ++ "'\n    " ++ failMsg

decodeFleetStatuses :: JObject -> Either String ServerMessage
decodeFleetStatuses obj = do
  statusesArr <- obj .? "statuses"
  statuses <- parseArray decodeFleetStatus statusesArr
  return $ FleetStatuses statuses

decodeFleetStatus :: Json -> Either String FleetStatus
decodeFleetStatus json = do
  obj <- asObject json
  name <- obj .? "name"
  statusStr <- obj .? "status"
  status <- case statusStr of
    "alive" -> Right Alive 
    "destroyed" -> Right Destroyed
    _ -> Left $ "Unrecognized status '" ++ statusStr ++ "'"
  pure $ FleetStatus { name: name, status: status }
  
parseArray :: forall a. (Json -> Either String a) -> Json -> Either String (Array a)
parseArray parseItem json = do
  arr <- asArray json
  sequence $ map parseItem arr
     
asObject :: Json -> Either String JObject
asObject json = toObject json >>=? "Could not parse '" ++ show json ++ "' as an object"
      
asArray :: Json -> Either String JArray
asArray json = toArray json >>=? "Could not parse '" ++ show json ++ "' as an array"

infixl 1 >>=?
(>>=?) :: forall a. Maybe a -> String -> Either String a
(>>=?) m msg = maybe (Left msg) Right m

infix 7 .?
(.?) :: forall a. (DecodeJson a) => JObject -> String -> Either String a
(.?) o s = maybe (Left $ "Expected field '" ++ show s ++ "' in '" ++ show o ++ "'") decodeJson (M.lookup s o)
  
derive instance genericClientMessage :: Generic ClientMessage
instance showClientMessage :: Show ClientMessage where
  show = gShow
      
instance encodeJsonClientMessage :: EncodeJson ClientMessage where
  encodeJson Join = fromObject $ M.fromFoldable [msgType "join"]
  encodeJson Ping = fromObject $ M.fromFoldable [msgType "ping"]
  encodeJson (LaunchMissiles _data) = fromObject $ M.fromFoldable [msgType "launchMissiles"]
  encodeJson (MoveFleet dirs) = fromObject $ M.fromFoldable [msgType "moveFleet"]
  encodeJson SelfDestruct = fromObject $ M.fromFoldable [msgType "selfDestruct"]
  
msgType :: String -> Tuple String Json
msgType t = Tuple "msgType" $ fromString t

parseEvent :: UdpEvent -> Either String ServerMessage
parseEvent (MessageEvent msg) = parseMessage msg
parseEvent event = Left $ "Could not parse event: '" ++ show event ++ "'"

parseMessage :: String -> Either String ServerMessage
parseMessage raw = (bimap errorMsg id $ jsonParser raw) >>= decodeJson
  where errorMsg msg = "Error parsing Json from message '" ++ raw ++ "': " ++ msg
