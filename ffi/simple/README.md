a simple hello world program that goes from haskell to C and back

### Compiling:

1. via your favorite C compiler make an object file of `hello_c.c`
2. run `ghc hello_c.o hello_h.hs`
3. run the resulting `hello_h` executable

expected output:
```
hello from haskell!
hello world from C!
back in hasell!
```
