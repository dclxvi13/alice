-----------------------------------------------------------------------------
--
-- Module      :  Alice.Data.ContextUnit
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

module Alice.Data.ContextUnit where

import Alice.Data.GetAssocsResult
import Alice.Data.Emo
import Alice.Data.Form

data ContextUnit = ContextUnit {
    contextUnit_pair :: (Form, Form),
    contextUnit_emo :: Emo,
    contextUnit_assocs :: GetAssocsResult
}

