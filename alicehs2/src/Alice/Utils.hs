module Alice.Utils where

import           Alice.Data
import           Alice.Form
import           Alice.Sensor


parseData :: AliceDB -> (SensorType, SensorData) -> IO [Form]
parseData conn (sType, sData) =
  case sType of
            Text  -> parseText conn sData
            Image -> undefined
            Sound -> undefined

parseText :: AliceDB -> SensorData -> IO [Form]
parseText conn sData = undefined

associate :: AliceDB -> Form -> IO [Form]
associate conn form = undefined
