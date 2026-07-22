
import Prelude
import Data.List (intercalate)

data Steppy a = Steppy { sorted   :: Bool
                       , acc      :: [a]
                       , rest     :: [a]
                       }

type EnStepped a = Either [a] (Steppy a)

instance Show a => Show (Steppy a) where
  show (Steppy sorted acc rest) =
    let showList = [show sorted, show acc, show rest] :: [String]
        shownFields = (intercalate ", " showList)     ::  String
    in "{"<>shownFields<>"}"

-- { True,       [], [1,3,2] }
-- { True,      [1],   [3,2] }
-- { True,    [3,1],     [2] }
-- { False,   [2,1],     [3] }
-- { False, [3,2,1],      [] }
-- { True,       [], [1,2,3] }
-- { True,      [1],   [2,3] }
-- { True,    [2,1],     [3] }
-- { True,  [3,2,1],      [] } -- this is our end condition

-- single step of a bubble sort
--   keeping this with Either [a] (Steppy a) so (a -> m a) remains clear
--     (vs :: Steppy a -> EnStepped a)
bstep' :: Ord a => Steppy a -> Either [a] (Steppy a)
bstep' (Steppy True  acc [] )    = Left . reverse $ acc
bstep' (Steppy False acc [] )    = pure (Steppy True [] (reverse acc))
bstep' (Steppy sorted [] (y:ys)) = pure (Steppy sorted [y] ys)
bstep' (Steppy sorted (x:xs) (y:ys)) =
  if x > y
  then pure (Steppy False  (y:xs)  (x:ys))
  else pure (Steppy sorted (y:x:xs)   ys )

stepify :: [a] -> Steppy a
stepify xs = Steppy True [] xs

trampoline :: (a -> Either b a) -> a -> b
trampoline f x =
  case f x of
    Left res  -> res
    Right arg -> trampoline f arg

bubbles' :: Ord a => [a] -> [a]
bubbles' xs = trampoline bstep' $ Steppy True [] xs

doNTimes :: Monad m => Int -> (a -> m a) -> m a -> m a
doNTimes 0 _  acc = acc
doNTimes n fm acc = doNTimes (n-1) fm (acc >>= fm)

printNGo :: (Show a, Ord a) => [a] -> IO ()
printNGo xs = go . pure . stepify $ xs
  where go :: (Show a, Ord a) => EnStepped a -> IO ()
        go (Left  lst)  = print lst >> putStrLn "done"
        go (Right step) = do
          print step
          go . bstep' $ step

printStep :: (Show a, Ord a) => EnStepped a -> IO (EnStepped a)
printStep res@(Left _) = do
  putStrLn $ "received: "<> show res <> ", work was completed"
  pure res

printStep res@(Right state) = do
  putStrLn $ "received: " <> show res <> ", stepping..."
  pure . bstep' $ state

-- for a quick shuffle-source:
interleave :: forall a. [a] -> [a] -> [a]
interleave []     ys = ys
interleave (x:xs) ys = x : (interleave ys xs)
