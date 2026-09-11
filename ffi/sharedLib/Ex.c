#include <stdio.h>
#include "libEx.h"

int main(void) {
	printf("hello world from C!\n");

	hsLib_init();
	fooMatic();
	printf("C, quitting\n");

	hsLib_end();
	return 0;
}
