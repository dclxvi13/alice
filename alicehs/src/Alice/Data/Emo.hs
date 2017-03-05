-----------------------------------------------------------------------------
--
-- Module      :  Alice.Data.Emo
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

module Alice.Data.Emo where

data Emo = Emo {
    emo_Ad :: Int, -- адреналин - стресс, страх
    emo_Do :: Int, -- дофамин - удовлетворение
    emo_Ox :: Int, -- окситоцин - привязанность
    emo_Ph :: Int, -- фенилэтиламин - заинтересованность
    emo_Co :: Int, -- кортизол - накопленный стресс
    emo_En :: Int  -- эндорфин - счастье, восторг
}
