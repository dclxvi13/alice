module Main where

import           Alice

main :: IO ()
main = do
  s <- state
  startAgent s
