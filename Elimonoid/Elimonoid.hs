module Elimonoid where

elimonoid :: (Monoid a, Foldable t) => t a -> a
elimonoid = foldMap ((<>)mempty)

joinE :: (Monoid a, Functor f, Foldable f) => f (f a) -> f a
joinE fx = elimonoid <$> fx

bindE :: (Monoid b, Functor f, Foldable f) => f a -> (a -> f b) -> f b
bindE mx f = joinE . (fmap f) $ mx

applyE :: (Monoid (f b), Functor f, Foldable f) => f (a -> b) -> f a -> f b
applyE fs xs = elimonoid $ (\f -> f <$> xs) <$> fs

