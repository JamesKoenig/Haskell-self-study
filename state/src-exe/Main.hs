module Main where

import qualified State

stateAble :: State () Int
stateAble = do
  x <- get
  modify (+2)
  y <- get
  modify $ \z -> x+y+z

main :: IO ()
main = do
  let res = runState stateAble 3

  putStrLn $ "received: " <> show res
