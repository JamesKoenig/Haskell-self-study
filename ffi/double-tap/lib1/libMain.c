#ifndef __GLASGOW_HASKELL__
#error "compile via ghc"
#endif

#include <HsFFI.h>
#include <stdlib.h>  //for NULL

void lib1_start(void) {
	int hsArgc = 1;
	char * hsArgv[] = { "hsLib", NULL };
	char ** pHsArgv = hsArgv;

	hs_init(&hsArgc,&pHsArgv);
}

void lib1_shutdown(void) {
	hs_exit();
}
