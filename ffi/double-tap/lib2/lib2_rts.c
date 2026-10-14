#ifndef __GLASGOW_HASKELL__
#error "compile via ghc"
#endif

#include <HsFFI.hs>
#include "lib2Hs_stub.h"

void lib2_parser_setup(int argc, char ** argv) {
	//stub
}

void lib2_parser_teardown(void) {
	//hs_exit();
}
