
all: haskell executable

clean:
	rm Example_stub.h\
		Example.o\
		Example.hi\
		ex.o\
		example

haskell:
	ghc -c Example.hs

executable: haskell
	ghc --make -no-hs-main ex.c Example.o -o example
