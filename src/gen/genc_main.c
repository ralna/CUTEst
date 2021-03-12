
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

#define GENCMA

#ifdef __cplusplus
extern "C" {   /* To prevent C++ compilers from mangling symbols */
#endif

#include "cutest.h"

#define GENC    genc
#define GENSPC  genspc
#define GETINFO getinfo

    doublereal GENC( doublereal );
    void GENSPC( integer, char* );
    void GETINFO( integer, integer, doublereal*, doublereal*,
    	          doublereal*, doublereal*, logical*, logical*,
		  VarTypes* );


    integer CUTEst_nvar;        /* number of variables */
    integer CUTEst_ncon;        /* number of constraints */
    integer CUTEst_nnzj;        /* number of nonzeros in Jacobian */
    integer CUTEst_nnzh;        /* number of nonzeros in upper triangular
                                  part of the Hessian of the Lagrangian */

    int MAINENTRY( void ) {

	char *fname = "OUTSDIF.d"; /* CUTEst data file */
	integer funit = 42;        /* FORTRAN unit number for OUTSDIF.d */
	integer iout = 6;          /* FORTRAN unit number for error output */
	integer io_buffer = 11;    /* FORTRAN unit internal input/output */
	integer ierr;              /* Exit flag from OPEN and CLOSE */
	integer status;            /* Exit flag from CUTEst tools */

	VarTypes vtypes;

	integer ncon_dummy;
	doublereal *x, *bl, *bu, *dummy1, *dummy2;
	doublereal *v = NULL, *cl = NULL, *cu = NULL;
	logical *equatn = NULL, *linear = NULL;
	char *pname, *vnames, *gnames, *cptr;
        char **Vnames, **Gnames; /* vnames and gnames as arrays of strings */
	logical grad;
	integer e_order = 0, l_order = 0, v_order = 0;
	logical constrained = FALSE_;

	doublereal calls[7], cpu[4];
	integer nlin = 0, nbnds = 0, neq = 0;
	doublereal dummy;
	integer ExitCode;
	int i, j;

	/* Open problem description file OUTSDIF.d */
	ierr = 0;
	FORTRAN_open( &funit, fname, &ierr );
	if( ierr ) {
	    printf("Error opening file OUTSDIF.d.\nAborting.\n");
	    exit(1);
	}

	/* Determine problem size */
	CUTEST_cdimen( &status, &funit, &CUTEst_nvar, &CUTEst_ncon );

	if( status ) {
          printf("** CUTEst error, status = %d, aborting\n", status);
	    exit(status);
	}

	/* Determine whether to call constrained or unconstrained tools */
	if( CUTEst_ncon ) constrained = TRUE_;

	/* Seems to be needed for some Solaris C compilers */
	ncon_dummy = CUTEst_ncon + 1;

	/* Reserve memory for variables, bounds, and multipliers */
	/* and call appropriate initialization routine for CUTEst */
	MALLOC( x,      CUTEst_nvar, doublereal );
	MALLOC( bl,     CUTEst_nvar, doublereal );
	MALLOC( bu,     CUTEst_nvar, doublereal );
	if( constrained ) {
	    MALLOC( equatn, CUTEst_ncon+1, logical    );
	    MALLOC( linear, CUTEst_ncon+1, logical    );
	    MALLOC( v,      CUTEst_ncon+1, doublereal );
	    MALLOC( cl,     CUTEst_ncon+1, doublereal );
	    MALLOC( cu,     CUTEst_ncon+1, doublereal );
	    CUTEST_csetup( &status, &funit, &iout, &io_buffer,
                    &CUTEst_nvar, &CUTEst_ncon, x, bl, bu,
		    v, cl, cu, equatn, linear, 
		    &e_order, &l_order, &v_order );
	} else {
	    MALLOC( equatn, 1, logical    );
	    MALLOC( linear, 1, logical    );
            MALLOC( cl, 1, doublereal );
	    MALLOC( cu, 1, doublereal );
	    CUTEST_usetup( &status, &funit, &iout,  &io_buffer,
                           &CUTEst_nvar, x, bl, bu );
	}

	if( status ) {
          printf("** CUTEst error, status = %d, aborting\n", status);
	    exit(status);
	}

	/* Get problem, variables and constraints names */
	MALLOC(pname, FSTRING_LEN+1, char);
        MALLOC(vnames, CUTEst_nvar * FSTRING_LEN, char);     /* For Fortran */
        MALLOC(Vnames, CUTEst_nvar, char*);          /* Array of strings */
        for(i = 0; i < CUTEst_nvar; i++)
          MALLOC(Vnames[i], FSTRING_LEN+1, char);

	if( constrained ) {
          MALLOC(gnames, CUTEst_ncon * FSTRING_LEN, char);   /* For Fortran */
          MALLOC(Gnames, CUTEst_ncon, char*);        /* Array of strings */
          for(i = 0; i < CUTEst_ncon; i++)
            MALLOC(Gnames[i], FSTRING_LEN+1, char);
          CUTEST_cnames( &status, &CUTEst_nvar, &CUTEst_ncon, 
                         pname, vnames, gnames );
	} else {
          CUTEST_unames( &status, &CUTEst_nvar, pname, vnames );
	}

	if( status ) {
          printf("** CUTEst error, status = %d, aborting\n", status);
	    exit(status);
	}

	/* Make sure to null-terminate problem name */
	pname[FSTRING_LEN] = '\0';

        /* Transfer variables and constraint names into arrays of
         * null-terminated strings.
         * If you know of a simpler way to do this portably, let me know!
         */
        for(i = 0; i < CUTEst_nvar; i++) {
          cptr = vnames + i * FSTRING_LEN;
          for(j = 0; j < FSTRING_LEN; j++) {
            Vnames[i][j] = *cptr;
            cptr++;
          }
          Vnames[i][FSTRING_LEN] = '\0';
        }

        for(i = 0; i < CUTEst_ncon; i++) {
          cptr = vnames + i * FSTRING_LEN;
          for(j = 0; j < FSTRING_LEN; j++) {
            Gnames[i][j] = *cptr;
            cptr++;
          }
          Gnames[i][FSTRING_LEN] = '\0';
        }

        /* Fortran strings no longer needed */
        FREE(vnames);
        if(constrained) FREE(gnames);

        printf("Variable names:\n");
        for(i = 0; i < CUTEst_nvar; i++)
          printf("  %s\n", Vnames[i]);

        /* Free memory for variable names */
        for(i = 0; i < CUTEst_nvar; i++) FREE(Vnames[i]);
        FREE(Vnames);

        if( constrained ) printf("Constraint names:\n");
        for(i = 0; i < CUTEst_ncon; i++)
          printf("  %s\n", Gnames[i]);

        /* Free memory for constraint names */
        for(i = 0; i < CUTEst_ncon; i++) FREE(Gnames[i]);
        if(constrained) FREE(Gnames);

	/* Obtain basic info on problem */
	if (!constrained) {
	    equatn[0] = FALSE_;
	    linear[0] = FALSE_;
    }
	GETINFO(CUTEst_nvar, CUTEst_ncon, bl, bu, cl, cu, 
                equatn, linear, &vtypes);

	/* Call the optimizer */
	dummy = GENC( ONE );
	ExitCode = 0;

	/* Get CUTEst statistics */
	CUTEST_creport( &status, calls, cpu );

	if( status ) {
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
        if(constrained)	printf(" # constraints functions = %-15.7g\n", calls[4]);
        if(constrained) printf(" # constraints gradients = %-15.7g\n", calls[5]);
        if(constrained) printf(" # constraints Hessians  = %-15.7g\n", calls[6]);
	printf(" Exit code               = %-10d\n", (int)ExitCode);
	printf(" Final f                 = %-15.7g\n",dummy);
	printf(" Set up time             = %-10.2f seconds\n", cpu[0]);
	printf(" Solve time              = %-10.2f seconds\n", cpu[1]);
	printf(" ******************************************************************\n\n");

	ierr = 0;
	FORTRAN_close( &funit, &ierr );
	if( ierr ) {
	    printf( "Error closing %s on unit %d.\n", fname, (int)funit );
	    printf( "Trying not to abort.\n" );
	}

	/* Free workspace */
	FREE( pname );
	FREE( x ); FREE( bl ); FREE( bu );
	FREE( v ); FREE( cl ); FREE( cu );
	FREE( equatn );
	FREE( linear );


	return 0;

    }

#ifdef __cplusplus
}    /* Closing brace for  extern "C"  block */
#endif
