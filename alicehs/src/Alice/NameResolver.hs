module Alice.NameResolver where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Data.Dynamic
import qualified Data.Map.Strict as Map

data NRMsg = ProxyV String Dynamic | SetN String (TQueue Dynamic)

data NRState = NRState {
  nrState_self :: NR,
  nrState_storage :: Map.Map String (TQueue Dynamic)
}

type NR = TQueue NRMsg

start :: NR -> IO ThreadId
start q = forkIO $ loop $ NRState q Map.empty

setName :: NR -> String -> TQueue Dynamic -> IO ()
setName nr name queue = atomically $ writeTQueue nr $ SetN name queue

loop :: NRState -> IO ()
loop state = do
  msg <- atomically $ readTQueue $ nrState_self state
  state' <- case msg of
    ProxyV name value -> do
      case Map.lookup name (nrState_storage state) of
        Nothing -> undefined
        Just queue ->
          atomically $ writeTQueue queue value
      return state
    SetN name queue ->
      let m = Map.insert name queue $ nrState_storage state in
      return state {nrState_storage = m}
  loop state'
