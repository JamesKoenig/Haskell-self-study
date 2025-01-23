-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- in this case: (<$>) :: (String -> String -> String) -> [String] -> [String->String] (f is [], String is [Char])
-- (<*>) :: Applicative f => f ( a -> b ) -> f a -> f b
-- in this case: (<*>) :: [String -> String] -> [String] -> [String]
( (\suite -> (\rank -> suite<>rank)) <$> ["S","H","C","D"] ) <*> ("A" : (show <$> [2..10]) ++ ["J","Q","K"])
