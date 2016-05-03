module SpaceBlitz.Dgram 
  ( UdpEvent(ListeningEvent, MessageEvent, CloseEvent, ErrorEvent)
  , UdpEventListener
  , UDP
  , Socket
  , createSocket
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, error)
import Data.Foreign (Foreign, unsafeFromForeign)

foreign import data UDP :: !
foreign import data Socket :: *
type RawEventListener eff = RawEvent -> Eff eff Unit
type RawEvent = { event :: String, rawData :: Foreign }
type UdpEventListener eff = UdpEvent -> Eff eff Unit
data UdpEvent = ListeningEvent | MessageEvent String | CloseEvent | ErrorEvent Error

instance showUdpEvent :: Show UdpEvent where
  show ListeningEvent = "Listening"
  show (MessageEvent msg) = "Message '" ++ msg ++ "'"
  show CloseEvent = "Close"
  show (ErrorEvent error) = "Error '" ++ show error ++ "'"

foreign import createRawSocket :: forall hEff eff. Int -> RawEventListener hEff -> Eff (udp :: UDP | eff) Socket

createSocket :: forall hEff eff. Int -> UdpEventListener hEff -> Eff (udp :: UDP | eff) Socket
createSocket port listener = createRawSocket port (parseRawEvent >>> listener)

parseRawEvent :: RawEvent -> UdpEvent
parseRawEvent { event, rawData } = case event of
  "error" -> ErrorEvent $ unsafeFromForeign rawData
  "message" -> MessageEvent $ unsafeFromForeign rawData
  "listening" -> ListeningEvent
  "close" -> CloseEvent
  _ -> ErrorEvent $ error $ "Unknown UDP event: { event: " ++ event ++ ", rawData: " ++ unsafeFromForeign rawData ++ " }"
