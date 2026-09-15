GHC_LIBOPTS = --make -dynamic -shared -fPIC -flink-rts

.PHONY: lib1 lib2

all: lib1 lib2 main

lib1:
	ghc $(GHC_LIBOPTS) lib1/libMain.c lib1/libHs.hs -o lib1.so

lib2:
	echo "STUB"

main:
	echo "STUB"

clean: clean_lib1

clean_lib1:
	rm\
		lib1/libHs.o\
		lib1/libHs.hi\
		lib1/libHs_stub.h\
		lib1/libMain.o\
		lib1.so

