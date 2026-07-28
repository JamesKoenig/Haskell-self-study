# State Monad experiments

The [Haskell Mooc][1] has a section that introduces a simplified State Monad
which is a [pretty cool design pattern][2] & something I want to understand
better.

An example of its use (from my work through its homework):
```haskell
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
ghci> runState (count 'a' >> count 'b' >> count 'a') []
((),['a',2),('b',1)])
```

[1]: https://haskell.mooc.fi/part2#the-state-monad
[2]: https://wiki.haskell.org/State_Monad
