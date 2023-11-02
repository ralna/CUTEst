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
#define CUTEST_precision CUTEST_half_long
#else
#define CUTEST_KINDS_precision CUTEST_KINDS_half
#define CUTEST_precision CUTEST_half
#endif
#elif defined CUTEST_single
#ifdef CUTEST_long
#define CUTEST_KINDS_precision CUTEST_KINDS_single_long
#define CUTEST_precision CUTEST_single_long
#else
#define CUTEST_KINDS_precision CUTEST_KINDS_single
#define CUTEST_precision CUTEST_single
#endif
#elif defined CUTEST_quad
#ifdef CUTEST_long
#define CUTEST_KINDS_precision CUTEST_KINDS_quadruple_long
#define CUTEST_precision CUTEST_quadruple_long
#else
#define CUTEST_KINDS_precision CUTEST_KINDS_quadruple
#define CUTEST_precision CUTEST_quadruple
#endif
#else
#ifdef CUTEST_long
#define CUTEST_KINDS_precision CUTEST_KINDS_double_long
#define CUTEST_precision CUTEST_double_long
#else
#define CUTEST_KINDS_precision CUTEST_KINDS_double
#define CUTEST_precision CUTEST_double
#endif
#endif
