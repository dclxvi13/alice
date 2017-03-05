-----------------------------------------------------------------------------
--
-- Module      :  Alice.Data.SForm
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

module Alice.Data.SForm where

import Alice.Data.Form

data SForm = SForm {
    sform_Object :: Form,
    sform_ObjectProperties :: [(Form, Form)],
    sform_Action :: Form,
    sform_ActionProperties :: [(Form, Form)]
} deriving Show

