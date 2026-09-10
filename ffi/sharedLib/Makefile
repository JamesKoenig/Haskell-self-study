all: shared cCode

#TODO FIXME: get include path somehow through ghc.
#   Note: ghc --print-c-comipler-flgas/--print-global-package-db is not it
cCode:
	clang\
		Ex.c\
		-I /usr/lib/ghc/lib/x86_64-linux-ghc-9.4.7/rts-1.0.2/include\
		-Wl,-rpath,.\
		libEx.so\
		-o Exec

shared:
	ghc --make -dynamic -shared -fPIC -flink-rts Ex.hs -o libEx.so

clean:
	rm\
		Ex.o\
		Ex.hi\
		Ex_stub.h\
		libEx.so\
		Exec
