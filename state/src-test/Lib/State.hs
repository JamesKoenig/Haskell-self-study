module Lib.State where

import State
import Test.QuickCheck
import Test.QuickCheck.Function ((:->))

--TODO why am I doing this instead of just genState = funToState . foo
data FunState s a =
  FunState (Fun s (a,s))

instance Functor (FunState s) where
  -- p :: s :-> (a,s)
  -- g :: s  -> (a,s)
  fmap f (FunState (Fun (p, d, s) g)) =
    FunState $ Fun (fp <$> p, fp d, s) (fp . g)
    where fp (x,y) = (f x, y)

instance (Arbitrary s, CoArbitrary s, Function s, Arbitrary a) =>
    Arbitrary (FunState s a) where
  arbitrary = FunState <$> (arbitrary :: Gen (Fun s (a,s)))

-- helper function to pop the concrete function out of a Fun
conFunc :: (Fun a b) -> a :-> b
conFunc (Fun (cf,_,_) _) = cf

--TODO: Rename
conFuncFunState :: FunState s a -> s :-> (a,s)
conFuncFunState (FunState fun) = conFunc fun

funToState :: FunState s a -> State s a
funToState (FunState (Fun _ f)) = State f

-- instance Arbitrary a, Arbitrary b => Arbitrary (a,b)
-- instance (Function a, CoArbitrary a, Arbitrary b) => Arbitrary (Fun a b)

--instance (Arbitrary a, Arbitrary b

-- this gives all sorts of warnings and is only here for reference
--instance Arbitrary (Fun a (b,a)) => Arbitrary (State a b) where
--    arbitrary = arbitrary >>= pure . State
--      fn <- arbitrary :: Gen (Fun a (a,b))
--      pure . State $ fn
