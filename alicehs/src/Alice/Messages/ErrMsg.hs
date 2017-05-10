module Alice.Messages.ErrMsg where

import Control.Concurrent

data ErrMsg = ErrMsg {
  errMsg_source :: ThreadId,
  errMsg_reason :: String
}
