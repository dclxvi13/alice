-----------------------------------------------------------------------------
--
-- Module      :  Alice.Data.GetAssocsResult
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

module Alice.Data.GetAssocsResult where

import Alice.Data.Assoc

data GetAssocsResult = GetAssocsResult {
    getAssocsResult_Main :: Assoc,
    getAssocsResult_FirstResult :: [Assoc],
    getAssocsResult_SecondResult :: [Assoc]}
