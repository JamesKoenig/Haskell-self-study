module Main (main) where

import Test.QuickCheck

type AState = (Bool,Ordering)
type AValue = Ordering

main :: IO ()
main = do
  putStrLn "hello! from State test function"
  (Fun (uh,_,_) _f) <- generate arbitrary :: IO (Fun AState (AValue,AState))
  print uh
  putStrLn "done!"

--main' :: IO ()
--main' = do
--  putStrLn "hello world"
--  (Fun (uh,_,_) f) <- generate arbitrary :: IO (Fun Semaphore TwoEnum)
--  putStrLn "recieved the function:"
--  print uh
--  putStrLn "generating arguments"
--  args <- sample' arbitrary :: IO [Semaphore]
--  print args
--  let printElem :: Semaphore -> IO ()
--      printElem s = do
--        putStrLn $ show s <> "->" <> show (f s)
--  traverse_ printElem args
--  putStrLn "done!"

