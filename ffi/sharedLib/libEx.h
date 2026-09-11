#ifndef HS_EXAMPLE_LIB
#define HS_EXAMPLE_LIB

#if defined(__cplusplus)
extern "C" {
#endif

//from C-interfacing code
extern void hsLib_init(void);
extern void hsLib_end(void);

// from haskell
extern void fooMatic(void);
#if defined(__cplusplus)
}
#endif //__cplusplus

#endif //HS_EXAMPLE_LIB
