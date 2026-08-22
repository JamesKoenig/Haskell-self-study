{-# Language CPP #-}
#ifndef NO_NEWTYPE_DERIVING
{-# Language GeneralizedNewtypeDeriving #-}
#endif
module KindaSmall where

-- written up for https://github.com/nick8325/quickcheck/issues/485

import Test.QuickCheck

newtype Small' a = Small' { getSmall' :: a }
  deriving ( Eq, Ord, Show, Read
#ifndef NO_NEWTYPE_DERIVING
  , Num, Integral, Real, Enum
#endif
  )

instance Functor Small' where
  fmap f (Small' x) = Small' (f x)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Small' a) where
  arbitrary = fmap Small' goArbitrary
   where goArbitrary = sized $ \n ->
            arbitrary `suchThat` (\x -> (abs x) < (coerceLimit n))
         coerceLimit x = fromInteger . toEnum $ x
