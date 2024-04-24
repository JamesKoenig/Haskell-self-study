-- this file contains notes and studies based on a stack overflow question,
--   which can be found at:
-- https://stackoverflow.com/questions/69988537/do-notation-and-the-list-monad

-- Alternative defines 
import Control.Applicative (Alternative)
import Control.Monad (guard)

-- variation mostly the same as posted by SO user 4castle:
--   https://stackoverflow.com/a/69989262
-- Note that Alternative is used as a type constraint instead of MonadPlus
--  There are important doctrinal discussion reasons for this that I will go
--   into detail in the notes at the end of this example code and use examples
findSumGuard :: (Monad m, Alternative m) => m Int -> m Int -> m (Int,Int,Int)
findSumGuard as bs = do
    a  <- as
    a' <- as
    b  <- bs
    guard (a+a' == b)
    return (a, a', b)

-- the originally intended runtime results of the above would be
-- > findSumGaurd [1..4] [1..4]
-- < [(1,1,2),(1,2,3),(1,3,4),(2,1,3),(2,2,4),(3,1,4)]
--
-- note (ie: "yes and,") with the abstractions used we can run this with:
-- > findSumGuard (Just 2) (Just 4)
-- < Just (2,2,4)
-- since `a <- as` lifts the Int out of Maybe as well as it does for lists

--the above, but without do notation and explicit bind syntax
--  and more descriptive variable names as a study,
--    avoids the above guard for an equivalent if-then-else as seen in another
--      response by SO user willeM_Van_Onsem:
--  https://stackoverflow.com/a/69988574
findSumGuard' :: [Int] -> [Int] -> [(Int,Int,Int)]
findSumGuard' firstInts sumCandidates =
    firstInts >>= (\firstInt ->
        firstInts >>= (\secondInt ->
            sumCandidates >>= (\candidateSum ->
                if firstInt+secondInt == candidateSum
                    then [(firstInt,secondInt,candidateSum)]
                    else []
                )
            )
        )

-- for reference in using bind (>>=) above here is the following ad-hoc example
--   (simple, in some ways derivative of the above, but, this is my work)
filterEven :: [Int] -> [Int]
filterEven ints =
    ints >>= (\int ->           --for each int in ints
        if (int `mod` 2) == 0   --if they're even
            then [int]          --return them
            else []             --otherwise return nothing
    )
-- e.g:
-- > filterEven [1..10]
-- < [2,4,6,8,10]

-- the above example, but with Maybe
filterMaybeEven :: Maybe Int -> Maybe Int
filterMaybeEven perhapsInt =
    perhapsInt >>= (\numbah ->
        if (numbah `mod` 2) == 0
            then Just numbah
            else Nothing
    )

-- e.g. 1:
-- > filterMaybeEven $ Just 4
-- < Just 4
-- > filterMaybeEven $ Just 3
-- < Nothing
--
-- e.g. 2: (using some fmap shenanigans to showcase some lifting)
-- > filterMaybeEven <$> Just <$> [1..8]
-- < [Nothing,Just 2,Nothing,Just 4,Nothing,Just 6,Nothing,Just 8]

-- the above two functions, but with do notation,
--   and monadic abstraction via the guard function
filterEvenDo :: (Monad m, Alternative m) => m Int -> m Int
filterEvenDo numbers = do
    numbah <- numbers             --conjure forth our numbah if possible
    guard (numbah `mod` 2 == 0)   --check if it's even fail if not
    return numbah                 --if successful give numbah back in its
                                  --  containing type

-- all 4 of the above filter-even commands run on the same:
-- > filterEvenDo [1..10]
-- < [2,4,6,8,10]
-- > filterEvenDo $ Just 4
-- < Just 4
-- > filterEvenDo $ Just 3
-- < Nothing
-- > filterEvenDo <$> Just <$> [1..8]
-- < [Nothing,Just 2,Nothing,Just 4,Nothing,Just 6,Nothing,Just 8]
-- and a new e.g. for good measure:
-- > Just <$> filterEvenDo [1..8]
-- < [Just 2,Just 4,Just 6,Just 8]

-- one can even import `maybeToList` from Data.Maybe (or write their own, it's
--  pretty simple) giving something along the lines of the following:
-- > import Data.Maybe (maybeToList) -- maybeToList :: Maybe a -> [a]
-- > filterEvenDo <$> Just <$> [1..10] >>= maybeToList
-- < [2,4,6,8,10]
-- this stuff is wild.  And hoogle is really useful.

{-====================DRILL DOWN ON NERDY STUFF FOLLOWS======================-}

--NB: ON MonadPlus in the original SO code, vs Alternative used in this study:
-- The original example uses the list type ([]) which satisfies a set of
--  laws (namely that operations satisfy the established laws for the Haskell
--    data types corrisponding to Monoids, left-zero, and left-distribution)
-- see:
--  https://wiki.haskell.org/MonadPlus
--
--  Unfortunately this creates a set of circumstances where some Monads satisfy
--    the different MonadPlus proposals, while others do not.  Notably in
--      certain proposasls lists satisfy them but IO and Maybe do not
--  Given that the above code was designed for lists ([]) it makes sense that
--    they erred on the side of the one that favors it
--
--but it brings the above code into the periphery of a doctrinal conflict:
--  https://wiki.haskell.org/MonadPlus_reform_proposal
--
-- Strinctly speaking though, guard's type signature is:
--   `Alternative f => Bool -> f ()`
-- Alternative is walked through in several places notably here:
--   https://wiki.haskell.org/Typeclassopedia#Definition_7
-- it is worth noting that under certain strict definitions of the proposed
--   Alternative encounters some problems that flip the script from []'s safety
--     in the SO's use of MonadPlus, as seen in the section of the above article:
--      https://wiki.haskell.org/Typeclassopedia#Laws_6
-- To quote:
-- >    Intuitively, this law states that pure should always represent a
-- >   "successful" computation. It is satisfied by Maybe, IO, and parsers.
-- >    However, it is not satisfied by lists, since lists collect all possible
-- >    results: it corresponds to [a] ++ x == [a] which is obviously false.
--
-- so in the most strict presentation of the underlying use of `guard` above
--  I distinctly erred towards its most pedantic definition as part of the
--    Alternative constraint, even if the Haskell type system currently allows
--      one to use MonadPlus for [] and Maybe monads
--

{-================================= Addenda =================================-}
{-
 - since it's useful to note, the type signatures of the protagonists of this
 -  study are:
 -
 - (>>=) :: Monad m => m a -> (a -> m b) -> m b
 - guard :: Alternative f => Bool -> f ()
 -
 - moreso for the examples above:
 - (<$>)  :: Functor f => (a -> b) -> f a -> f b -- aka "fmap", or map on lists
 -}
