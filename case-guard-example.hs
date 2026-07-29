#!/usr/bin/env stack
{- stack
   ghci
   -v
   --install-ghc
   --resolver lts-24.43
 -}

{-
 - case uses pattern matching with guards! the result is that you can use them
 - similarly to how they appear when using guards in function pattern matching
 -
 - `foo'`  showcses this behavior
 - `foo''` includes the well-documented pattern match + guards for functions.
 -
 - `main`  showcases the code by generating a random 7-length list and playing
 -         `foo` against it while debug-printing the interim steps.
 -}
module CaseGuardExample where

import Test.QuickCheck

data AdditivePrimary = Red | Green | Blue deriving (Show,Eq,Ord,Enum,Bounded)

foo :: [AdditivePrimary] -> Int
foo xs = foo' 0 False xs

foo' :: Int -> Bool -> [AdditivePrimary] -> Int
foo' acc _  []     = acc
foo' acc on (x:xs) =
  case x of
    Green | on        -> foo' (acc+1) on xs
    Blue  | on        -> foo' (acc-1) on xs
          | otherwise -> foo' (acc+1) on xs
    Red               -> foo' acc (not on) xs
    _                 -> foo' acc on xs

-- equivalent with pure pattern matching and guards.  NB. Not otherwise used
foo'' :: Int -> Bool -> [AdditivePrimary] -> Int
foo'' acc _  []       = acc
foo'' acc on (Green:xs)
  | on                = foo'' (acc+1) on xs
foo'' acc on (Blue:xs)
  | on                = foo'' (acc-1) on xs
  | otherwise         = foo'' (acc+1) on xs
foo'' acc on (Red:xs) = foo'' acc (not on) xs
foo'' acc on (_:xs)   = foo'' acc on xs

bar :: [AdditivePrimary] -> Int -> IO Int
bar xs l = do
  putStrLn $ "studying: " <> (show $ take l xs)
  let res = bar' xs l
  putStrLn $ "result was: " <> show res
  pure res

bar' :: [AdditivePrimary] -> Int -> Int
bar' xs l = foo (take l xs)

baz :: IO [Int]
baz = do
  let lstGen :: Gen [AdditivePrimary]
      lstGen = vectorOf 7 $ chooseEnum (minBound,maxBound)
  lst <- generate lstGen :: IO [AdditivePrimary]
  putStrLn $ "received: " <> show lst

  let maxLength = length lst
      lengths   = [1..maxLength]

  traverse (bar lst) lengths

main :: IO ()
main = baz >>= print
