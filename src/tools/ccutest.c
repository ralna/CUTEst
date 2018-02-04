
/* ================================================== */
/* Auxilliary functions                               */
/* ================================================== */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#ifdef __cplusplus
extern "C" {   /* To prevent C++ compilers from mangling symbols */
#endif

#include "cutest.h"

  /* Track origin of error messages */

#define CUTEst_ERRQ(errcode,msg) {                                       \
    printf( "CUTEst C error:: Code = %d, Msg :: %s\n", errcode, msg );   \
    printf( "Error occured in function %s, file %s at line %d\n",       \
            __FUNCT__, __FILE__, __LINE__ );                            \
    exit( errcode );                                                    \
  }

  /* -------------------------------------------------- */

#ifdef  __FUNCT__
#undef  __FUNCT__
#endif
#define __FUNCT__ "CUTEst_malloc"
  void *CUTEst_malloc( void *object, int length, size_t s ) {
    object = malloc( length * s );
    if( !object ) CUTEst_ERRQ(-1,"Unable to allocate memory");
    return object;
  }

  /* -------------------------------------------------- */

#ifdef  __FUNCT__
#undef  __FUNCT__
#endif
#define __FUNCT__ "CUTEst_calloc"
  void *CUTEst_calloc( void *object, int length, size_t s ) {
    object = calloc( (size_t)length, s );
    if( !object ) CUTEst_ERRQ(-2,"Unable to allocate pointer");
    return object;
  }

  /* -------------------------------------------------- */

#ifdef  __FUNCT__
#undef  __FUNCT__
#endif
#define __FUNCT__ "CUTEst_realloc"
  void *CUTEst_realloc( void *object, int length, size_t s ) {
    object = realloc( object, length * s );
    if( !object ) CUTEst_ERRQ(-3,"Unable to reallocate");
    return object;
  }

  /* -------------------------------------------------- */

#ifdef  __FUNCT__
#undef  __FUNCT__
#endif
#define __FUNCT__ "CUTEst_free"
  void CUTEst_free( void **object ) {
    if( *object ) {
      free( *object );
      *object = NULL;
    }
  }

  /* -------------------------------------------------- */

#ifdef  __FUNCT__
#undef  __FUNCT__
#endif

#ifdef __cplusplus
}              /* Closing brace for  extern "C"  block */
#endif
