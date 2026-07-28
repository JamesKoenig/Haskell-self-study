# State Monad experiments

The [Haskell Mooc][1] has a section that introduces a simplified State Monad
which is a [pretty cool design pattern][2] & something I want to understand
better.

An example of its use (from my work through its homework):
```haskell
import Control.Monad.Trans.State
import Data.List (delete)
count :: Eq a => a -> State [(a,Int)] ()
count x = do
  kvs <- get
  let mvs  = lookup x kvs
      n    = maybe 0 id mvs
      rest = delete (x,n) kvs

  put $ (x,n+1):rest
```
e.g. use:
```haskell
ghci> import Data.Foldable (traverse_)
ghci> flip execState [] $ traverse_ count "hello world"
[('d',1),('l',3),('r',1),('o',2),('w',1),(' ',1),('e',1),('h',1)]
```

[1]: https://haskell.mooc.fi/part2#the-state-monad
[2]: https://wiki.haskell.org/State_Monad
