#!/usr/bin/env stack
{- stack
   ghci
   -v
   --install-ghc
   --resolver lts-24.43
 -}

module UnRandom where

import Test.QuickCheck.Gen    (Gen(..))
import Test.QuickCheck.Random (mkQCGen)

{- Based upon a random person's question in the Haskell developers Discord
 - where dyniec responded to someone asking
 - > is there a way to run QuickCheck's `arbitrary` in a predicatable fashion?
 - (meaning they wanted the same values every time), with:
 -   `` gen (MkGen g) s = g (mkQCGen s) 30 ``
 -
 - so, this file is me messing around with replacing the seed for given `Gen`s
 -}

unRandom :: Int -> Gen a -> Gen a
unRandom seed (MkGen g) =
  MkGen $ \_ _ -> g (mkQCGen seed) 30

unRandom_ :: Gen a -> Gen a
unRandom_ (MkGen g) =
  MkGen $ \_ _ -> g (mkQCGen 0) 30
