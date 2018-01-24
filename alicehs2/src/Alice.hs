module Alice (
    state,
    startAgent
    ) where

import           Alice.Data
import           Alice.Form
import           Alice.Sensor
import qualified Alice.Ticker  as Ticker
import           Alice.Utils
import           Control.Monad (forM, void)

data AliceState = AliceState {
  _sensors  :: [Sensor],
  _database :: AliceDB
}

type NowData = (AliceState, Frame)

data Frame = Frame {
--  _state           :: AliceState,
  _sensorForms     :: [Form],
  _associatedForms :: [(Form, [Form])]
--  _emotedForms ::
--  _conclused
--  _act
}

state :: IO AliceState
state = do
  let txtSensorHost = ""
      txtSensorPort = 3000
  txtSensor <- initSensor txtSensorHost txtSensorPort Text
  db <- connectDB
  return AliceState { _sensors = [txtSensor], _database = db }

startAgent :: AliceState -> IO ()
startAgent state = do
  t <- Ticker.startTicker
  loop state $ \s -> void (Ticker.getTick t) >> iteration s

iteration :: AliceState -> IO AliceState
iteration state =
  frame state >>=
  sensors >>=
  associations >>=
  emotions >>=
  conclusions >>=
  actions >>=
  memoize

loop :: a -> (a -> IO a) -> IO ()
loop s fun = do
  new <- fun s
  loop new fun

frame :: AliceState -> IO NowData
frame state = return (state, Frame {
  _sensorForms = [],
  _associatedForms = []
})

sensors :: NowData -> IO NowData
sensors (state, frame) = do
  let sensors = _sensors state
      db = _database state
  forms <- concat $ forM sensors $ \sensor -> do
                                        datas <- getSensorData sensor
                                        parseData db datas
  return (state, frame {_sensorForms = forms})

associations :: NowData -> IO NowData
associations (state, frame) = do
  let sensorForms = _sensorForms frame
      db = _database state
  forms <- forM sensorForms $ \sensorForm -> do
                                              fs <- associate db sensorForm
                                              return (sensorForm, fs)
  return (state, frame {_associatedForms = forms})

emotions :: NowData -> IO NowData
emotions (state, frame) = undefined

conclusions :: NowData -> IO NowData
conclusions (state, frame) = undefined

actions :: NowData -> IO NowData
actions (state, frame) = undefined

memoize :: NowData -> IO AliceState
memoize (state, frame) = undefined
