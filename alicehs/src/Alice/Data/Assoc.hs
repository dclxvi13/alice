-----------------------------------------------------------------------------
--
-- Module      :  Alice.Data.Assoc
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

module Alice.Data.Assoc where

import Alice.Data.Form

data Assoc = Assoc {
    assoc_Key :: Form,
    assoc_First :: Form,
    assoc_Second :: Form,
    assoc_Index :: Int
}

