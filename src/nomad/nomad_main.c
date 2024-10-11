/* THIS VERSION: CUTEST 2.3 - 2024-10-11 AT 09:00 GMT */

/* ====================================================
 * CUTEst interface simulating a black box for NOMAD.
 * April 25, 2013
 *
 * D. Orban from an earlier version by S. Le Digabel.
 * ====================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {   /* To prevent C++ compilers from mangling symbols */
#endif

#ifdef FLT_MAX
#define INFINITY FLT_MAX
#else
#define INFINITY 1.0e+20
#endif

#include "cutest.h"
#include "cutest_routines.h"

  integer CUTEst_nvar;        /* number of variables */
  integer CUTEst_ncon;        /* number of constraints */

  void return_infinity(void);

  int MAINENTRY( int argc , char ** argv ) {

    FILE * f;

    char   *fname = "OUTSDIF.d"; /* CUTEst data file */
    integer funit = 42;        /* FORTRAN unit number for OUTSDIF.d */
    integer iout  = 6;         /* FORTRAN unit number for error output */
    integer io_buffer = 11;    /* Internal input/output buffer */
    integer ierr;              /* Exit flag for various calls */


    integer ncon_dummy;
    rp_ *x, *bl, *bu;
    rp_ *v = NULL, *cl = NULL, *cu = NULL;
    logical *equatn = NULL, *linear = NULL;
    integer efirst = 0, lfirst = 0, nvfrst = 0;
    logical constrained = FALSE_;

    real calls[7], cpu[4];
    integer nlin = 0, nbnds = 0, neq = 0;
    rp_ obj;
    int i;

    /* Open problem description file OUTSDIF.d */
    ierr = 0;
    FORTRAN_open( &funit, fname, &ierr );
    if ( ierr ) {
      return_infinity();
      return -1;
    }

    /* Determine problem size */
    CUTEST_cdimen( &ierr, &funit, &CUTEst_nvar, &CUTEst_ncon );
    if ( ierr ) {
      return_infinity();
      return -2;
    }

    /* Determine whether to call constrained or unconstrained tools */
    if( CUTEst_ncon ) constrained = TRUE_;

    /* Reserve memory for variables, bounds, and multipliers */
    /* and call appropriate initialization routine for CUTEst */
    MALLOC( x,      CUTEst_nvar, rp_ );
    MALLOC( bl,     CUTEst_nvar, rp_ );
    MALLOC( bu,     CUTEst_nvar, rp_ );

    if( constrained ) {
      MALLOC( equatn, CUTEst_ncon+1, logical    );
      MALLOC( linear, CUTEst_ncon+1, logical    );
      MALLOC( v,      CUTEst_ncon+1, rp_ );
      MALLOC( cl,     CUTEst_ncon+1, rp_ );
      MALLOC( cu,     CUTEst_ncon+1, rp_ );
      CUTEST_csetup( &ierr, &funit, &iout, &io_buffer,
                       &CUTEst_nvar, &CUTEst_ncon,
                       x, bl, bu, v, cl, cu,
                     equatn, linear, &efirst, &lfirst, &nvfrst );
      if ( ierr ) {
        return_infinity();
        return -3;
      }
    }
    else {
      MALLOC( equatn, 1, logical    );
      MALLOC( linear, 1, logical    );
      MALLOC( cl, 1, rp_ );
      MALLOC( cu, 1, rp_ );
      CUTEST_usetup( &ierr, &funit, &iout, &io_buffer,
                       &CUTEst_nvar, x, bl, bu );
      if ( ierr ) {
        return_infinity();
        return -3;
      }
    }

    FORTRAN_close( &funit, &ierr );

    /* If an input vector is supplied, use it.
     * Otherwise, use the problem's initial guess. */
    if( argc >= 2 ) {

      /* See if problem dimension is requested */
      if (strcmp(argv[1], "--nvar") == 0) {
        printf("%d\n", CUTEst_nvar);
        return 0;
      }

#ifdef REAL_32
      char pf[ ]="%f";
#else
      char pf[ ]="%lf";
#endif

      /* See if initial guess is requested */
      if (strcmp(argv[1], "--x0") == 0) {
        for ( i = 0; i < CUTEst_nvar; i++ )
          printf(pf, x[i]);
        printf("\n");
        return 0;
      }

      f = fopen(argv[1], "r");

      for ( i = 0 ; i < CUTEst_nvar ; i++ )
        if(fscanf(f, pf, &x[i]));

      /*
      for ( i = 0 ; i < CUTEst_nvar ; i++ )
        printf("x[%d]=%g\n",i,x[i]);
      */

      fclose(f);
    }

    if( constrained ) {
      /* Recycle the array v to store constraint values */
      CUTEST_cfn( &ierr, &CUTEst_nvar, &CUTEst_ncon, x, &obj, v );
      if ( ierr ) {
        return_infinity();
        return -4;
      }
      printf("%21.15e ", obj);
      for ( i = 0 ; i < CUTEst_ncon ; i++ )
        printf("%21.15e ", v[i]);
      printf("\n");
    }
    else {
      CUTEST_ufn( &ierr, &CUTEst_nvar , x , &obj);
      if ( ierr ) {
        return_infinity();
        return -4;
      }
      printf("%21.15e\n", obj);
    }

    /* Free workspace */
    FREE( x );
    FREE( bl );
    FREE( bu );
    FREE( v );
    FREE( cl );
    FREE( cu );
    FREE( equatn );
    FREE( linear );

    return 0;
  }

  void return_infinity(void) {
    int i;
    /* Return very large values */
    printf("%21.15e ", INFINITY);
    for ( i = 0 ; i < CUTEst_ncon ; i++ )
        printf ( "%21.15e ", INFINITY);
    printf("\n");
    return;
  }

#ifdef __cplusplus
}    /* Closing brace for  extern "C"  block */
#endif
