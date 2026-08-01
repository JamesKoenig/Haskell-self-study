#!/usr/bin/env stack
{- stack
   ghci
   -v
   --install-ghc
   --resolver lts-24.43
 -}

import Test.QuickCheck
import Data.Foldable (traverse_)

-- haskell.mooc.fi Part 2, Lecture 13, Section 8 discusses a simplified
--   State monad:
data State s a = State (s -> (a,s))

-- The mooc reommends adding this helper function
state :: (s -> (a, s)) -> State s a
state f = State f

runState (State f) s = f s

put  :: s -> State s ()
put state = State (\oldState -> ((),state))

get :: State s s
get = State (\state -> (state,state))

modify :: (s -> s) -> State s ()
modify f = State (\state -> ((), f state))

--  because Monad depends on Functor and Applicative I'm breaking out its
--    two functions here so that I can interact with it now.
sPure x    = State (\s -> (x,s))  --encloses the value
sBind op f = State h
  where h state0 = let (val,state1) = runState op state0
                       op2 = f val
                   in runState op2 state1

--use this when checking whether the Functor instance is correct
oldFmap f sx = sBind sx (sPure . f)

--TODO: FIXME: make Functor and applicative instances from scratch
instance Functor (State s) where
--  fmap f sx = sBind sx (sPure . f)
  fmap f (State oldStateFn) = State g
    where g newState = let (val,state') = oldStateFn newState
                       in (f val, state')

instance Applicative (State s) where
  pure = sPure
  sf <*> sx = sf >>= \f -> sx >>= \x -> pure $ f x

instance Monad (State s) where
  --return x = State (\s -> (x,s)) --original version
  return = pure

  -- same as sBind, but left since we're quoting the mooc:
  op >>= f = State h
    where h state0 = let (val,state1) = runState op state0
                         op2 = f val
                     in runState op2 state1

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

