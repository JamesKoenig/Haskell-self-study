{-# LANGUAGE UndecidableInstances #-}
module Elimonoid where

class Elimonoid w where
  elimonoid :: Monoid a => w a -> a

instance Elimonoid Maybe where
  elimonoid Nothing  = mempty
  elimonoid (Just x) = x

instance Foldable t => Elimonoid t where
  --elimonoid :: Monoid a => t a -> a
  elimonoid = foldMap (<>mempty)
