NB when searching through GHC source history (especially commit names), per:
[this merge](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/11058),
results for things like
[TAG GHC 6.10.1 Release](https://gitlab.haskell.org/ghc/ghc/-/tree/93af30c76013d796fd3dd0d667ad9bf274c59188)
are actually from the Haddock source tree and should be exluded.

the ancestry path:
`2b39cd941c80d2603f2480684c45dd31f9256831..a7dcf13bfbb97b20e75cc8ce650e2bb628db4660`
should be filtered out when searching for things in GHC or anything else not
Haddock related.

[`2b39cd941c80d2603f2480684c45dd31f9256831`](https://gitlab.haskell.org/ghc/ghc/-/tree/2b39cd941c80d2603f2480684c45dd31f9256831)
is the initial commit for haddock.

[`a7dcf13bfbb97b20e75cc8ce650e2bb628db4660`](https://gitlab.haskell.org/ghc/ghc/-/tree/a7dcf13bfbb97b20e75cc8ce650e2bb628db4660)
was its last commit before being merged into GHC's `master` branch.

