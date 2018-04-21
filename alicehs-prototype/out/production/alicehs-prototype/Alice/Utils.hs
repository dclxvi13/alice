module Alice.Utils where

import           Data.ByteString

type Form = Int

data State = State
  { _sensors :: [Sensor]
  }

data Sensor =
  Sensor

data SensorType
  = SensorText
  | SensorImage

getSensorsData :: [Sensor] -> IO [(ByteString, SensorType)]
getSensorsData state = undefined

parseData :: ByteString -> SensorType -> IO [Form]
parseData bytes SensorText = undefined
parseData bytes sensorType = undefined

updateState :: State -> [[Form]] -> IO (State, [Form])
updateState state forms = undefined

doActions :: [Form] -> IO ()
doActions actions = undefined
