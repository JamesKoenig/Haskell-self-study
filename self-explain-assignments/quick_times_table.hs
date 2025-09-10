-- a common math exercise is to make times tables e.g.
-- 1  2  3  4  5
-- 2  4  6  8 10
-- 3  6  9 12 15
-- 4  8 12 16 20
-- 5 10 15 20 25
-- make a times table m such that m !! (i-1) !! (j-1) is equal to i*j

----------------------------------- SPOILERS -----------------------------------
-- this will make a times table from 1 to 10
(\xs -> [ (*x) <$> xs | x <- xs]) [1..10]
-- alternatively
(\xs -> do { x <- xs; return $ (*x) <$> xs }) [1..10]
-- or, using bind
(\xs -> xs >>= \x -> pure $ (*x) <$> xs) [1..10]
