#ifndef __GLASGOW_HASKELL__
#error "compile via ghc"
#endif

#include <HsFFI.h>
#include <stdbool.h>
#include <stdlib.h>

/* ideally this would return true but the code simply errors and exits if
 * the rts fails to start...
 */
void hsLib_init(void) {
	int hsArgc = 1;
	char * hsArgv[] = { "hsLib", NULL };
	char ** pHsArgv = hsArgv;

	hs_init(&hsArgc,&pHsArgv);
}

void hsLib_end(void) {
	hs_exit();
}
