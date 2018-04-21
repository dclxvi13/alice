module Alice where

import           Alice.Utils
import           Control.Monad

start :: IO ()
start = do
  state <- prepare
  loop state

prepare :: IO State
prepare = do
  return $ State []

loop :: State -> IO ()
loop state = do
  dat <- getSensorsData $ _sensors state
  forms <- forM dat $ \(bytes, sensor) -> parseData bytes sensor
  (state', actions) <- updateState state forms
  doActions actions
  loop state'
