#!/usr/bin/env stack
{- stack
   ghci
   -v
   --install-ghc
   --resolver lts-24.43
 -}

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

--TODO: FIXME: make Functor and applicative instances from scratch
instance Functor (State s) where
  fmap f sx = sBind sx (sPure . f)

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
