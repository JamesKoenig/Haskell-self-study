#include <stdlib.h>
#include <HsFFI.h>
#include <stdbool.h>
#include <assert.h>

// this has yet to be tested
bool hsLib_init(void) {
	int argcIsh = 1;
	char *argv[] = { "hsLib" };
	hs_init(&argc,&argv); //can this fail???

	return true;
}

void hsLib_end(void) {
	hs_exit();
}
