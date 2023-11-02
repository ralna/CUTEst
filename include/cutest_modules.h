/* \file cutest_modules.h */

/*
 * assign names for each CUTEst package using the C pre-processor.
 * possibilities are long (i8) and normal (i4, default) integers and
 * half (r2), single (r4), double (r8, default) and quadruple (r16) reals
 *
 * Nick Gould for CUTEst
 * initial version, 2023-11-02
 * this version 2023-11-02
 */

#ifdef CUTEST_long
#define CUTEST_KINDS_integer CUTEST_KINDS_long
#else
#define CUTEST_KINDS_integer CUTEST_KINDS_int
#endif

#if defined CUTEST_half
#ifdef CUTEST_long
#define CUTEST_KINDS_precision CUTEST_KINDS_half_long
#else
#define CUTEST_KINDS_precision CUTEST_KINDS_half
#endif
#elif defined CUTEST_single
#ifdef CUTEST_long
#define CUTEST_KINDS_precision CUTEST_KINDS_single_long
#else
#define CUTEST_KINDS_precision CUTEST_KINDS_single
#endif
#elif defined CUTEST_quad
#ifdef CUTEST_long
#define CUTEST_KINDS_precision CUTEST_KINDS_quadruple_long
#else
#define CUTEST_KINDS_precision CUTEST_KINDS_quadruple
#endif
#else
#ifdef CUTEST_long
#define CUTEST_KINDS_precision CUTEST_KINDS_double_long
#else
#define CUTEST_KINDS_precision CUTEST_KINDS_double
#endif
#endif
