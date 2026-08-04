module Main (main) where

import State
import Test.QuickCheck
import Data.Foldable (traverse_)

data TwoEnum = First | Second deriving (Show,Eq,Ord,Enum)

instance Arbitrary TwoEnum where
  arbitrary = elements [First,Second]

data Semaphore = Green | Yellow | Red deriving (Show,Eq,Ord,Enum,Bounded)

instance Arbitrary Semaphore where
  arbitrary = chooseEnum (Green,Red)

instance CoArbitrary Semaphore where
  coarbitrary = coarbitraryEnum

instance Function Semaphore where
  function = functionBoundedEnum

main :: IO ()
main = do
  putStrLn "hello world"
  fun@(Fun (uh,_,_) f) <- generate arbitrary :: IO (Fun Semaphore TwoEnum)
  putStrLn "recieved the function:"
  print uh
  putStrLn "generating arguments"
  args <- sample' arbitrary :: IO [Semaphore]
  print args
  let printElem :: Semaphore -> IO ()
      printElem s = do
        putStrLn $ show s <> "->" <> show (f s)
  traverse_ printElem args
  putStrLn "done!"

