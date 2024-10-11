/* THIS VERSION: CUTEST 2.3 - 2024-10-11 AT 09:00 GMT */

/* ============================================
 * CUTEst interface for generic package
 *
 * D. Orban Feb. 3, 2003
 * CUTEst evolution, Nick Gould Jan 4 2013
 *
 * Take a look at $CUTEST/include/cutest.h and
 * $CUTEST/src/loqo/loqoma.c  for more examples.
 * ============================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <string.h>

#define GENCMA

#ifdef __cplusplus
extern "C" {   /* To prevent C++ compilers from mangling symbols */
#endif

#include "cutest.h"
#include "cutest_routines.h"

#define GENC    genc
#define GENSPC  genspc
#define GETINFO getinfo

rp_ GENC( rp_ );
void GENSPC( integer, char * );
void GETINFO( integer, integer, rp_ *, rp_ *,
              rp_ *, rp_ *, logical *, logical *,
              VarTypes * );


integer CUTEst_nvar;        /* number of variables */
integer CUTEst_ncon;        /* number of constraints */
integer CUTEst_nnzj;        /* number of nonzeros in Jacobian */
integer CUTEst_nnzh;        /* number of nonzeros in upper triangular
                                  part of the Hessian of the Lagrangian */

int MAINENTRY( void ){

    char *fname = "OUTSDIF.d"; /* CUTEst data file */
    integer funit = 42;        /* FORTRAN unit number for OUTSDIF.d */
    integer iout = 6;          /* FORTRAN unit number for error output */
    integer io_buffer = 11;    /* FORTRAN unit internal input/output */
    integer ierr;              /* Exit flag from OPEN and CLOSE */
    integer status;            /* Exit flag from CUTEst tools */

    VarTypes vtypes;

    rp_ *x, *bl, *bu, *dummy1, *dummy2;
    rp_ *v = NULL, *cl = NULL, *cu = NULL;
    logical *equatn = NULL, *linear = NULL;
    char *pname, *vnames, *gnames, *cptr;
    char *classification;
    char **Vnames, **Gnames; /* vnames and gnames as arrays of strings */
    logical grad;
    integer e_order = 1, l_order = 0, v_order = 0;
    logical constrained = FALSE_;

    rp_ calls[7], cpu[4];
    integer nlin = 0, nbnds = 0, neq = 0;
    rp_ dummy;
    integer ExitCode;
    int i, j;

    /* Open problem description file OUTSDIF.d */
    ierr = 0;
    FORTRAN_open( &funit, fname, &ierr );
    if ( ierr )
    {
        printf("Error opening file OUTSDIF.d.\nAborting.\n");
        exit(1);
    }

    /* Determine problem size */
    CUTEST_cdimen( &status, &funit, &CUTEst_nvar, &CUTEst_ncon );

    if ( status )
    {
        printf("** CUTEst error, status = %d, aborting\n", status);
        exit(status);
    }

    MALLOC(classification, FCSTRING_LEN + 1, char);
    CUTEST_classification( &status, &funit, classification );

    /* Determine whether to call constrained or unconstrained tools */
    if ( CUTEst_ncon ) constrained = TRUE_;

    /* Reserve memory for variables, bounds, and multipliers */
    /* and call appropriate initialization routine for CUTEst */
    MALLOC( x,      CUTEst_nvar, rp_ );
    MALLOC( bl,     CUTEst_nvar, rp_ );
    MALLOC( bu,     CUTEst_nvar, rp_ );
    if ( constrained )
    {
        MALLOC( equatn, CUTEst_ncon, logical    );
        MALLOC( linear, CUTEst_ncon, logical    );
        MALLOC( v,      CUTEst_ncon, rp_ );
        MALLOC( cl,     CUTEst_ncon, rp_ );
        MALLOC( cu,     CUTEst_ncon, rp_ );
        CUTEST_csetup( &status, &funit, &iout, &io_buffer,
                       &CUTEst_nvar, &CUTEst_ncon, x, bl, bu,
                       v, cl, cu, equatn, linear,
                       &e_order, &l_order, &v_order );
        /*printf("CUTEst_nvar = %d\n", CUTEst_nvar);
        printf("CUTEst_ncon = %d\n", CUTEst_ncon);
        printf("x = ");
        for (i = 0; i < CUTEst_nvar ; i++)
            printf("%g ", x[i]);
        printf("\n");
        printf("bl = ");
        for (i = 0; i < CUTEst_nvar ; i++)
            printf("%g ", bl[i]);
        printf("\n");
        printf("bu = ");
        for (i = 0; i < CUTEst_nvar ; i++)
            printf("%g ", bu[i]);
        printf("\n");
        printf("v = ");
        for (i = 0; i < CUTEst_ncon ; i++)
            printf("%g ", v[i]);
        printf("\n");
        printf("cl = ");
        for (i = 0; i < CUTEst_ncon ; i++)
            printf("%g ", cl[i]);
        printf("\n");
        printf("cu = ");
        for (i = 0; i < CUTEst_ncon ; i++)
            printf("%g ", cu[i]);
        printf("\n");
        printf("equatn = ");
        for (i = 0; i < CUTEst_ncon ; i++)
            printf("%d ", equatn[i]);
        printf("\n");
        printf("linear = ");
        for (i = 0; i < CUTEst_ncon ; i++)
            printf("%d ", linear[i]);
            printf("\n"); */
    }
    else
    {    CUTEST_usetup( &status, &funit, &iout, &io_buffer,
                         &CUTEst_nvar, x, bl, bu );
    /*    printf("CUTEst_nvar = %d\n", CUTEst_nvar); */
    }
    if ( status )
    {
        printf("** CUTEst error, status = %d, aborting\n", status);
        exit(status);
    }

    /* Get problem, variables and constraints names */
    MALLOC(pname, FSTRING_LEN + 1, char);
    MALLOC(vnames, CUTEst_nvar * ( FSTRING_LEN + 1 ), char); /* For Fortran */
    MALLOC(Vnames, CUTEst_nvar, char *);               /* Array of strings */
    for (i = 0; i < CUTEst_nvar; i++)
        MALLOC(Vnames[i], FSTRING_LEN + 1, char);

    if ( constrained )
    {
      MALLOC(gnames, CUTEst_ncon * ( FSTRING_LEN + 1 ), char); /* For Fortran */
      MALLOC(Gnames, CUTEst_ncon, char *);          /* Array of strings */
      for (i = 0; i < CUTEst_ncon; i++)
          MALLOC(Gnames[i], FSTRING_LEN + 1, char);
      CUTEST_cnames( &status, &CUTEst_nvar, &CUTEst_ncon,
                       pname, vnames, gnames );
    }
    else
    {
        CUTEST_unames( &status, &CUTEst_nvar, pname, vnames );
    }

    if ( status )
    {
        printf("** CUTEst error, status = %d, aborting\n", status);
        exit(status);
    }

    /* Make sure to null-terminate problem name */
    pname[FSTRING_LEN] = '\0';

    printf(" Problem: %-s\n", pname);
    printf(" Classification: %-s\n", classification);

    /* Transfer variables and constraint names into arrays of
     * null-terminated strings.
     * If you know of a simpler way to do this portably, let me know!
     */
    for (i = 0; i < CUTEst_nvar; i++)
    {
        cptr = vnames + i * ( FSTRING_LEN + 1 );
        for (j = 0; j < FSTRING_LEN; j++)
        {
            Vnames[i][j] = *cptr; 
            cptr++;
        }
        Vnames[i][FSTRING_LEN] = '\0';
    }

    for (i = 0; i < CUTEst_ncon; i++)
    {
        cptr = gnames + i * ( FSTRING_LEN + 1 );
        for (j = 0; j < FSTRING_LEN; j++)
        {
            Gnames[i][j] = *cptr;
            cptr++;
        }
        Gnames[i][FSTRING_LEN] = '\0';
    }

    /* Fortran strings no longer needed */
    FREE(vnames);
    if (constrained) FREE(gnames);

    printf(" Variable names:\n");
    for (i = 0; i < CUTEst_nvar; i++)
        printf("  %s\n", Vnames[i]);

    /* Free memory for variable names */
    for (i = 0; i < CUTEst_nvar; i++) FREE(Vnames[i]);
    FREE(Vnames);

    if ( constrained ) printf(" Constraint names:\n");
    for (i = 0; i < CUTEst_ncon; i++)
        printf("  %s\n", Gnames[i]);

    /* Free memory for constraint names */
    for (i = 0; i < CUTEst_ncon; i++) FREE(Gnames[i]);
    if (constrained) FREE(Gnames);

    /* Obtain basic info on problem */
    GETINFO(CUTEst_nvar, CUTEst_ncon, bl, bu, cl, cu,
            equatn, linear, &vtypes);

    /* Call the optimizer */
    dummy = GENC( ONE );
    ExitCode = 0;

    /* Get CUTEst statistics */
    CUTEST_creport( &status, calls, cpu );

    if ( status )
    {
        printf("** CUTEst error, status = %d, aborting\n", status);
        exit(status);
    }

    printf("\n\n ************************ CUTEst statistics ************************\n\n");
    printf(" Code used               : GENC\n");
    printf(" Problem                 : %-s\n", pname);
    printf(" # variables             = %-10d\n", (int)CUTEst_nvar);
    printf(" # constraints           = %-10d\n", (int)CUTEst_ncon);
    printf(" # linear constraints    = %-10d\n", vtypes.nlin);
    printf(" # equality constraints  = %-10d\n", vtypes.neq);
    printf(" # inequality constraints= %-10d\n", vtypes.nineq);
    printf(" # bound constraints     = %-10d\n", vtypes.nbnds);
    printf(" # objective functions   = %-15.7g\n", calls[0]);
    printf(" # objective gradients   = %-15.7g\n", calls[1]);
    printf(" # objective Hessians    = %-15.7g\n", calls[2]);
    printf(" # Hessian-vector prdct  = %-15.7g\n", calls[3]);
    if (constrained) printf(" # constraints functions = %-15.7g\n", calls[4]);
    if (constrained) printf(" # constraints gradients = %-15.7g\n", calls[5]);
    if (constrained) printf(" # constraints Hessians  = %-15.7g\n", calls[6]);
    printf(" Exit code               = %-10d\n", (int)ExitCode);
    printf(" Final f                 = %-15.7g\n", dummy);
    printf(" Set up time             = %-10.2f seconds\n", cpu[0]);
    printf(" Solve time              = %-10.2f seconds\n", cpu[1]);
    printf(" ******************************************************************\n\n");

    ierr = 0;
    FORTRAN_close( &funit, &ierr );
    if ( ierr )
    {
        printf( "Error closing %s on unit %d.\n", fname, (int)funit );
        printf( "Trying not to abort.\n" );
    }

    /* Free workspace */
    FREE( pname );
    FREE( x ); FREE( bl ); FREE( bu );
    FREE( v ); FREE( cl ); FREE( cu );
    FREE( equatn );
    FREE( linear );

    if ( constrained )
      CUTEST_cterminate( &status );
    else
      CUTEST_uterminate( &status );

    return 0;

}

#ifdef __cplusplus
}    /* Closing brace for  extern "C"  block */
#endif
