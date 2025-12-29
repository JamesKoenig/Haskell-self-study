import Prelude
import Data.Char
-- when trying to make a rot 13, I mistakenly made this oneline implementation:
f :: String -> String
f = \s -> (fmap chr) . (fmap (+65)) . (fmap (\n -> n + 13 `mod` 65)) . (fmap (\x -> x-65)) . (fmap ord) . (fmap toUpper) $ s
-- note that it does not work as intended

-- so to see if/where it cycled I wrote this function
applyN :: forall {a} {n}. (Eq n, Num n) => a -> (a -> a) -> n -> a
applyN acc _ 0 = acc
applyN acc f n = applyN (f acc) f (n-1)

-- and then ran
ns = applyN "hello" f <$> [0..]
-- note that this forms a cycle such that:
-- all values greater than 0 cycle every 32 applications of f
-- or, discarding the first (partial) cycle:
-- (ns !! n) == (ns !! m) -- forall n, m > 32, given n is congruent to m mod 32

-- ## The self study assignment is to make: ##
--  1. a non tererible implementation
--       i. it should work
--      ii. it should not make the eyes bleed
--     iii. it should not exceed 80 width
--      iv. it should not contain 6 fmaps
--  2. what's wrong with the above version?
--  3. why does the incorrect version cycle the way it does?

-- firstly, a non-terrible implementation
rotN :: Int -> String -> String
rotN n = fmap rotNch
       where rotNch  = chr . (+65) . rotNn . (\x -> x-65) . ord . toUpper
             rotNn m = mod (m+n) 26

rot13 = rotN 13

-- non abstracted version of the above:
--rot13 :: String -> String
--rot13 = fmap rot13ch
--      where rot13ch  = chr . (+65) . rot13n . (\x -> x-65) . ord . toUpper
--            rot13n n = mod (n+13) 26

-- secondly, what made the opening implementation not function as a rot13?
-- thirdly, why did the other one form a cycle mod 32?
