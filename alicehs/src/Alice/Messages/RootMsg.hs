-----------------------------------------------------------------------------
--
-- Module      :  Alice.Messages.RootMsg
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

module Alice.Messages.RootMsg where

import Control.Concurrent.STM.TQueue

import Alice.Data.Form
import Alice.Data.GetAssocsResult
import Alice.Data.Emo

data RootMsg = WordToForm String (TQueue Form) |
    GetAssoc (TQueue GetAssocsResult) (Form, Form) |
    GetEmo (TQueue Emo) (Form, Form) Emo

