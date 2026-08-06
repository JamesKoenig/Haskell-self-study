module State where

-- haskell.mooc.fi Part 2, Lecture 13, Section 8 discusses a simplified
--   State monad:
data State s a = State (s -> (a,s))

runState :: State s a -> s -> (a,s)
runState (State f) s = f s

put  :: s -> State s ()
put state = State (\_oldState -> ((),state))

get :: State s s
get = State (\state -> (state,state))

modify :: (s -> s) -> State s ()
modify f = State (\state -> ((), f state))

--  because Monad depends on Functor and Applicative I'm breaking out its
--    two functions here so that I can interact with it now.
sPure :: a -> State s a
sPure x    = State (\s -> (x,s))  --encloses the value
sBind :: State s a -> (a -> State s b) -> State s b
sBind op f = State h
  where h state0 = let (val,state1) = runState op state0
                       op2 = f val
                   in runState op2 state1

oldFmap :: (a -> b) -> State s a -> State s b
oldFmap f sx = sBind sx (sPure . f)

-- sf :: State s (a -> b)
-- sx :: State s a
-- (<*>) :: State s (a -> b) -> State s a -> State s b
oldApply :: State s (a -> b) -> State s a -> State s b
oldApply sf sx = sf >>= \f -> sx >>= \x -> pure $ f x
-- the above is eequivalent to
-- oldApply sf sx = do
--   f <- sf
--   x <- sx
--   pure $ f x
-- or:
-- oldApply sf sx = do
--   f <- sf
--   pure (f <$> sx)

altApply :: State s (a -> b) -> State s a -> State s b
altApply sf sx = State g
  where g state0 = let (f, state1) = runState sf state0
                   in runState (f <$> sx) state1

-- stereotypical functional programming approach of using supershort variables
overfunctionalApply :: State s (a -> b) -> State s a -> State s b
overfunctionalApply (State f0) (State f1) = State f2
  where f2 s0 = let ( g, s1) = f0 s0
                    ( x, s2) = f1 s1
                    y = g x
                in (y,s2)

--TODO: logically prove this is equivalent for `oldFmap` above
instance Functor (State s) where
  fmap f (State oldStateFn) = State g
    where g newState = let (val,state') = oldStateFn newState
                       in (f val, state')

--TODO: FIXME: make Applicative instances from scratch
instance Applicative (State s) where
  pure = sPure
  sf <*> sx = State g
    where g state0 = let (f,state1) = runState sf state0
                         (x,state2) = runState sx state1
                     in (f x,state2)

instance Monad (State s) where
  --return x = State (\s -> (x,s)) --original version
  return = pure

  -- same as sBind, but left since we're quoting the mooc:
  op >>= f = State h
    where h state0 = let (val,state1) = runState op state0
                         op2 = f val
                     in runState op2 state1

