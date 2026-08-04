module Main where

import qualified State as ST

stateAble :: ST.State Int ()
stateAble = do
  x <- ST.get
  ST.modify (+2)
  y <- ST.get
  ST.modify $ \z -> x+y+z

main :: IO ()
main = do
  let res = ST.runState stateAble 3

  putStrLn $ "received: " <> show res
