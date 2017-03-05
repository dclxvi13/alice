-----------------------------------------------------------------------------
--
-- Module      :  Alice.Messages.CommMsg
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Alice.Messages.CommMsg where

import Data.ByteString

data CommMsg = Tick
    | TextSensorClosed
    | TextData ByteString

