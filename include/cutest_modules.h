/*
 * \file cutest_modules.h
 * assign names for each CUTEst package using the C pre-processor.
 * possibilities are 64 bit (i8) and normal (32 bit) (i4, default) integers and
 * half (r2), single (r4), double (r8, default) and quadruple (r16) reals
 *
 * Nick Gould for CUTEst
 * initial version, 2023-11-02
 * this version 2024-06-11
 */

#ifdef INTEGER_64
#define CUTEST_KINDS_integer CUTEST_KINDS_64
#else
#define CUTEST_KINDS_integer CUTEST_KINDS_int
#endif

#if defined REAL_16
#ifdef INTEGER_64
#define CUTEST_KINDS_precision CUTEST_KINDS_half_64
#define CUTEST_precision CUTEST_half_64
#define CUTEST_PROBLEM_precision CUTEST_PROBLEM_half_64
#define CUTEST_INTERFACE_precision CUTEST_INTERFACE_half_64
#define CUTEST_LQP_precision CUTEST_LQP_half_64
#else
#define CUTEST_KINDS_precision CUTEST_KINDS_half
#define CUTEST_precision CUTEST_half
#define CUTEST_PROBLEM_precision CUTEST_PROBLEM_half
#define CUTEST_INTERFACE_precision CUTEST_INTERFACE_half
#define CUTEST_LQP_precision CUTEST_LQP_half
#endif
#elif defined REAL_32
#ifdef INTEGER_64
#define CUTEST_KINDS_precision CUTEST_KINDS_single_64
#define CUTEST_precision CUTEST_single_64
#define CUTEST_PROBLEM_precision CUTEST_PROBLEM_single_64
#define CUTEST_INTERFACE_precision CUTEST_INTERFACE_single_64
#define CUTEST_LQP_precision CUTEST_LQP_single_64
#else
#define CUTEST_KINDS_precision CUTEST_KINDS_single
#define CUTEST_precision CUTEST_single
#define CUTEST_PROBLEM_precision CUTEST_PROBLEM_single
#define CUTEST_INTERFACE_precision CUTEST_INTERFACE_single
#define CUTEST_LQP_precision CUTEST_LQP_single
#endif
#elif defined REAL_128
#ifdef INTEGER_64
#define CUTEST_KINDS_precision CUTEST_KINDS_quadruple_64
#define CUTEST_precision CUTEST_quadruple_64
#define CUTEST_PROBLEM_precision CUTEST_PROBLEM_quadruple_64
#define CUTEST_INTERFACE_precision CUTEST_INTERFACE_quadruple_64
#define CUTEST_LQP_precision CUTEST_LQP_quadruple_64
#else
#define CUTEST_KINDS_precision CUTEST_KINDS_quadruple
#define CUTEST_precision CUTEST_quadruple
#define CUTEST_PROBLEM_precision CUTEST_PROBLEM_quadruple
#define CUTEST_INTERFACE_precision CUTEST_INTERFACE_quadruple
#define CUTEST_LQP_precision CUTEST_LQP_quadruple
#endif
#else
#ifdef INTEGER_64
#define CUTEST_KINDS_precision CUTEST_KINDS_double_64
#define CUTEST_precision CUTEST_double_64
#define CUTEST_PROBLEM_precision CUTEST_PROBLEM_double_64
#define CUTEST_INTERFACE_precision CUTEST_INTERFACE_double_64
#define CUTEST_LQP_precision CUTEST_LQP_double_64
#else
#define CUTEST_KINDS_precision CUTEST_KINDS_double
#define CUTEST_precision CUTEST_double
#define CUTEST_PROBLEM_precision CUTEST_PROBLEM_double
#define CUTEST_INTERFACE_precision CUTEST_INTERFACE_double
#define CUTEST_LQP_precision CUTEST_LQP_double
#endif
#endif
