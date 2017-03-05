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

import Alice.Data.SForm
import Alice.Data.Emo

data AsMsg = FromComm [SForm] |
    UpdateCurrent Emo
