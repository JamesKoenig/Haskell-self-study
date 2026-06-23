{-# LANGUAGE UndecidableInstances #-}
import Data.Monoid (Product(..)) --for now just to do things

class Elimonoid w where
  elimonoid :: Monoid a => w a -> a

instance {-# OVERLAPPABLE #-} Foldable t => Elimonoid t where
  elimonoid = foldMap (<>mempty)

data Foo a b =
    Alternate a
  | Foo b
  | Stub
  deriving (Show, Functor)

instance {-# OVERLAPS #-} Elimonoid (Foo a) where
  elimonoid (Foo x) = x
  elimonoid _       = mempty

joinE :: (Monoid a, Functor f, Elimonoid f) => f (f a) -> f a
joinE fx = elimonoid <$> fx

bindE :: (Monoid b, Functor f, Elimonoid f) => f a -> (a -> f b) -> f b
bindE mx f = joinE . (fmap f) $ mx

applyE :: (Monoid (f b), Functor f, Elimonoid f) => f (a -> b) -> f a -> f b
applyE fs xs = elimonoid $ (\f -> f <$> xs) <$> fs

class Show a => Bar a where
  bar :: a -> a -> a

instance (Show a, Monoid a) => Bar a where
  bar = (<>)
