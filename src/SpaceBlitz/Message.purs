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
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Argonaut.Core (Json, fromObject, fromString, toArray, toObject, toString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (bimap)
import Data.Either (Either(Left, Right))
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.StrMap as M
import Data.Traversable (sequence)
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
  decodeJson json = maybe (Left $ "Failed to parse message: " ++ show json) Right $ do
    obj <- toObject json
    msgType <- M.lookup "type" obj >>= toString
    case msgType of
      "gameInit" -> Just $ GameInit "nothing"
      "fleetStatuses" -> do
        statusesArr <- (M.lookup "statuses" obj) >>= toArray
        statuses <- sequence $ map parseFleetStatus statusesArr
        Just $ FleetStatuses statuses
      "pong" -> Just Pong
      _ -> Nothing
      
parseFleetStatus :: Json -> Maybe FleetStatus
parseFleetStatus json = do
  obj <- toObject json
  name <- M.lookup "name" obj >>= toString
  statusStr <- M.lookup "status" obj >>= toString
  status <- case statusStr of
    "alive" -> Just Alive 
    "destroyed" -> Just Destroyed
    _ -> Nothing
  pure $ FleetStatus { name: name, status: status }
  
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
