#ifndef HS_EXAMPLE_LIB1_IFACE
#define HS_EXAMPLE_LIB1_IFACE

#include <stddef.h> //for size_t

extern void lib1_start(void);
extern void lib1_shutdown(void);

extern int lib1_execute(int a, int xs[], size_t length, int b);

#endif //HS_EXAMPLE_LIB1_IFACE
