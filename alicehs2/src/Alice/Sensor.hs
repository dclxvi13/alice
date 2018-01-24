module Alice.Sensor where

import           Alice.Network
import           Data.ByteString
import           Network.Socket  (Socket)

data SensorType = Text
  | Image
  | Sound

data Sensor = Sensor {
  sensorType :: SensorType,
  sensorLink :: Socket
}

type SensorData = ByteString

initSensor :: String -> Int -> SensorType -> IO Sensor
initSensor host port stype = do
  sock <- start host port
  return Sensor { sensorType = stype, sensorLink = sock }

getSensorData :: Sensor -> IO (SensorType, SensorData)
getSensorData Sensor{ sensorType = sType, sensorLink = sLink } = do
  sData <- getNetworkData sLink
  return (sType, sData)
