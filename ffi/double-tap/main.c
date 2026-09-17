#include <stdio.h>
#include "lib1/iface.h"
#include "lib2/iface.h"

int main(void) {
	const int lib1Arg[] = { 4,5,6 };
	printf("hello world from the c-main function\n");
	printf("initializing lib1");
	lib1_start();
	printf("done!");

	printf("running reduce(3*{4,5,6}+7), got: %d\n",
		lib1_execute(3,(int*) lib1Arg,3,7));
	printf("done!, shutting down lib1\n");
	lib1_shutdown();
	printf("lib1 shut down!\n");

	printf("end of main\n");
	return 0;
}
