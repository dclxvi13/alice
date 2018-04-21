module Alice.Utils where

import           Data.ByteString

import           Alice.Database

type Form = Int
type Action = Int

data State = State
  { _sensors :: [Sensor]
  , _aliceDB :: AliceDB
  }

data Sensor =
  Sensor

data SensorType
  = SensorText
  | SensorImage

prepare :: IO State
prepare = do
  db <- connectAliceDB
  return $ State [] db

getSensorsData :: [Sensor] -> IO [(ByteString, SensorType)]
getSensorsData state = undefined

parseData :: ByteString -> SensorType -> IO [Form]
parseData bytes SensorText = undefined
parseData bytes sensorType = undefined

updateState :: State -> [[Form]] -> IO (State, [Action])
updateState state forms = undefined

doActions :: [Action] -> IO ()
doActions actions = undefined
