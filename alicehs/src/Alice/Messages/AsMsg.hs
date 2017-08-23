-----------------------------------------------------------------------------
--
-- Module      :  Alice.Messages.AsMsg
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

module Alice.Messages.AsMsg where

import Alice.Data.Form
import Alice.Data.Emo

data AsMsg = FromComm [[Form]] |
    UpdateCurrent Emo
