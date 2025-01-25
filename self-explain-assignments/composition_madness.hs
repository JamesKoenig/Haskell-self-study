-- to begin
-- (.) :: (b->c) -> (a->b) -> a -> c
-- ((+2).) :: Num c => (a -> c) -> a -> c
-- (.(+2)) :: Num b => (b -> c) -> b -> c
-- using Int:
-- ((+2).) :: (Int -> Int) -> Int -> Int -> Int

-- getting weirder:
-- fmap          :: Functor f              => (   a ->   b)       -> f a       -> f b
-- (fmap .)      :: Functor f              => (   a ->  a2  -> b) -> a1        -> f a2 -> fb
-- (. fmap)      :: Functor f              => ((f a -> f b) -> c) -> (a -> b)  -> c
-- (fmap . fmap) :: Functor f1, Functor f2 => (   a ->   b)       -> f1 (f2 a) -> f1 (f2 b)

-- simplifying a bit visually via map (same as above but with Functor [])
-- map         :: (a -> b)             -> [a]      ->  [b]
-- (map .)     :: (a1 -> a2 -> b)      -> a1       -> [a2] -> [b]
-- (.map )     :: (([a] -> [b])  -> c) -> (a -> b) ->   c
-- (map . map) :: (a -> b)             -> [[a]]    -> [[b]]

-- see also:
-- things with the signuature :: Functor f1, Functor f2 => f1 (f2 (a))
-- (Just . Just)  :: a -> Maybe (Maybe a)
-- (:[]) . (:[])  :: a -> [[a]]
-- (Just . (:[])) :: a -> Maybe [a]

-- related, but not the same
-- (pure . pure)  :: (Applicative f1, Applicative f2) -> a -> f1 (f2 a)

