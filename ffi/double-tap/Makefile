GHC_LIBOPTS = --make -dynamic -shared -fPIC -flink-rts

.PHONY: lib1 lib2

all: lib1 lib2 main

lib1:
	ghc $(GHC_LIBOPTS) lib1/libMain.c lib1/libHs.hs -o lib1.so

lib2:
	echo "STUB"

# really gotta have ghc print where it stores its includes
main:
	clang\
		main.c\
		-I /usr/lib/ghc/lib/x86_64-linux-ghc-9.4.7/rts-1.0.2/include\
		-Wl,-rpath,.\
		lib1.so\
		-o doubleTap

clean: clean_lib1

clean_lib1:
	rm\
		lib1/libHs.o\
		lib1/libHs.hi\
		lib1/libHs_stub.h\
		lib1/libMain.o\
		lib1.so\
		doubleTap

