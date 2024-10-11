/* THIS VERSION: CUTEST 2.3 - 2024-10-11 AT 09:00 GMT */

/* ===========================================
 * CUTEst interface to derivative checker
 * 
 * CUTEr version, D. Orban , June 2011
 * CUTEst evolution, Nick Gould Jan 4 2013
 *
 * Take a look at $CUTEST/include/cutest.h and
 * $CUTEST/src/loqo/loqoma.c for more examples
 * ===========================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define DERCHKMA

#ifdef __cplusplus
extern "C" {   /* To prevent C++ compilers from mangling symbols */
#endif

#include "cutest.h"
#include "cutest_routines.h"

#define DERCHK    derchk
#define DERCHKSPC derchkspc
#define GETINFO   getinfo

    rp_ DERCHK(rp_);
    void DERCHKSPC(integer, char*);
    void GETINFO( integer, integer, rp_*, rp_*,
                  rp_*, rp_*, logical*, logical*,
                  VarTypes* );

    integer CUTEst_nvar;        /* number of variables */
    integer CUTEst_ncon;        /* number of constraints */
    integer CUTEst_nnzj;        /* number of nonzeros in Jacobian */
    integer CUTEst_nnzh;        /* number of nonzeros in upper triangular
                                  part of the Hessian of the Lagrangian */

    int MAINENTRY(void) {

        char *fname = "OUTSDIF.d"; /* CUTEst data file */
        integer funit = 42;        /* FORTRAN unit number for OUTSDIF.d */
        integer io_buffer = 11;    /* FORTRAN unit for internal i/o */
        integer iout = 6;          /* FORTRAN unit number for error output */
        integer ierr;              /* Exit flag from OPEN and CLOSE */
        integer status;            /* Exit flag from CUTEst tools */

        VarTypes vtypes;

        integer ncon_dummy;
        rp_ *x, *bl, *bu, *dummy1, *dummy2;
        rp_ *v = NULL, *cl = NULL, *cu = NULL;
        logical *equatn = NULL, *linear = NULL;
        char *pname, *vnames, *gnames, *cptr;
        char **Vnames, **Gnames; /* vnames and gnames as arrays of strings */
	logical grad;
	integer e_order = 0, l_order = 0, v_order = 0;
        logical constrained = FALSE_;
        logical header_unset;

        rp_ calls[7], cpu[4];
        integer nlin = 0, nbnds = 0, neq = 0;
        rp_ dummy;
        integer ExitCode;
        int i, j;

        rp_ h, fxp, fxm, approx, derr, xi, der_max;
        rp_ *cxp, *cxm, *g;
        int nerr = 0;

#ifdef REAL_32
    h = 1.0e-4;
    der_max = 1.0e-2;
#else
    h = 1.0e-8;
    der_max = 1.0e-4;
#endif

        /* Open problem description file OUTSDIF.d */
        ierr = 0;
        FORTRAN_open(&funit, fname, &ierr);
        if (ierr) {
            printf("Error opening file OUTSDIF.d.\nAborting.\n");
            exit(1);
        }

        /* Determine problem size */
        CUTEST_cdimen( &status, &funit, &CUTEst_nvar, &CUTEst_ncon);
        if (status) {
            printf("CUTEst error.\nAborting.\n");
            exit(2);
        }

        /* Determine whether to call constrained or unconstrained tools */
        if (CUTEst_ncon) constrained = TRUE_;

        /* Seems to be needed for some Solaris C compilers */
        ncon_dummy = CUTEst_ncon + 1;

        /* Reserve memory for variables, bounds, and multipliers */
        /* and call appropriate initialization routine for CUTEst */
        MALLOC(x,      CUTEst_nvar, rp_);
        MALLOC(bl,     CUTEst_nvar, rp_);
        MALLOC(bu,     CUTEst_nvar, rp_);
        if (constrained) {
            MALLOC(equatn, CUTEst_ncon+1, logical);
            MALLOC(linear, CUTEst_ncon+1, logical);
            MALLOC(v,      CUTEst_ncon+1, rp_);
            MALLOC(cl,     CUTEst_ncon+1, rp_);
            MALLOC(cu,     CUTEst_ncon+1, rp_);
            CUTEST_csetup( &status, &funit, &iout, &io_buffer,
                &CUTEst_nvar, &CUTEst_ncon, x, bl, bu,
                v, cl, cu, equatn, linear, 
                &e_order, &l_order, &v_order );
            if (status) {
                printf("CUTEst error.\nAborting.\n");
                exit(2);
            }
        } else {
            MALLOC(equatn, 1, logical);
            MALLOC(linear, 1, logical);
            MALLOC(cl, 1, rp_);
            MALLOC(cu, 1, rp_);
            CUTEST_usetup( &status, &funit, &iout, &io_buffer, &CUTEst_nvar, 
                           x, bl, bu);
            if (status) {
                printf("CUTEst error.\nAborting.\n");
                exit(2);
            }
        }

        /* Get problem, variables and constraints names */
        MALLOC(pname, FSTRING_LEN+1, char);
            MALLOC(vnames, CUTEst_nvar * FSTRING_LEN, char);  /* For Fortran */
            MALLOC(Vnames, CUTEst_nvar, char*);          /* Array of strings */
            for (i = 0; i < CUTEst_nvar; i++)
            MALLOC(Vnames[i], FSTRING_LEN+1, char);

        if (constrained) {
            MALLOC(gnames, CUTEst_ncon * FSTRING_LEN, char);   /* For Fortran */
            MALLOC(Gnames, CUTEst_ncon, char*);        /* Array of strings */
            for (i = 0; i < CUTEst_ncon; i++)
                MALLOC(Gnames[i], FSTRING_LEN+1, char);
            CUTEST_cnames( &status, &CUTEst_nvar, &CUTEst_ncon, 
                           pname, vnames, gnames);
            if (status) {
                printf("CUTEst error.\nAborting.\n");
                exit(2);
            }
        } else {
            CUTEST_unames( &status, &CUTEst_nvar, pname, vnames);
            if (status) {
                printf("CUTEst error.\nAborting.\n");
                exit(2);
            }
        }

        /* Make sure to null-terminate problem name */
        pname[FSTRING_LEN] = '\0';

        /* Transfer variables and constraint names into arrays of
         * null-terminated strings.
         * If you know of a simpler way to do this portably, let me know!
         */
        for (i = 0; i < CUTEst_nvar; i++) {
          cptr = vnames + i * FSTRING_LEN;
          for (j = 0; j < FSTRING_LEN; j++) {
            Vnames[i][j] = *cptr;
            cptr++;
          }
          Vnames[i][FSTRING_LEN] = '\0';
        }

        for (i = 0; i < CUTEst_ncon; i++) {
          cptr = vnames + i * FSTRING_LEN;
          for (j = 0; j < FSTRING_LEN; j++) {
            Gnames[i][j] = *cptr;
            cptr++;
          }
          Gnames[i][FSTRING_LEN] = '\0';
        }

        /* Fortran strings no longer needed */
        FREE(vnames);
        if (constrained) FREE(gnames);

    /* Obtain basic info on problem */
    if (constrained) {
        GETINFO( CUTEst_nvar, CUTEst_ncon, bl, bu, cl, cu, equatn,
             linear, &vtypes );
    } else {
        equatn[0] = FALSE_;
        linear[0] = FALSE_;
        GETINFO( CUTEst_nvar, 1, bl, bu, cl, cu, equatn,
             linear, &vtypes );
    }

    /* Scramble initial point. */
    for (i = 0; i < CUTEst_nvar; i++)
        x[i] = pow(-1,i) * (2*i+1);

    /* Evaluate gradient at initial point. */
    MALLOC(g, CUTEst_nvar, rp_);
    CUTEST_ugr( &status, &CUTEst_nvar, x, g);
    if (status) {
        printf("CUTEst error.\nAborting.\n");
        exit(2);
    }

    /* Check first derivatives of objective and constraints. */

    if (constrained) {
        MALLOC(cxp, CUTEst_ncon+1, rp_);
        MALLOC(cxm, CUTEst_ncon+1, rp_);
    }
    header_unset = TRUE_;
    for (i = 0; i < CUTEst_nvar; i++) {
        xi = x[i];
        x[i] = xi + h;
        if (constrained) {
            CUTEST_cfn( &status, &CUTEst_nvar, &CUTEst_ncon, 
                        x, &fxp, cxp);
            if (status) {
                printf("CUTEst error.\nAborting.\n");
                exit(2);
            }
         } else {
            CUTEST_ufn( &status, &CUTEst_nvar, x, &fxp);
            if (status) {
                printf("CUTEst error.\nAborting.\n");
                exit(2);
            }
        }
        x[i] = xi - h;
        if (constrained) {
            CUTEST_cfn( &status, &CUTEst_nvar, &CUTEst_ncon, 
                        x, &fxm, cxm);
            if (status) {
              printf("CUTEst error.\nAborting.\n");
              exit(2);
            }
          } else {
                CUTEST_ufn( &status, &CUTEst_nvar, x, &fxm);
            if (status) {
              printf("CUTEst error.\nAborting.\n");
              exit(2);
            }
          }  
        x[i] = xi;      /* Restore x */

        /* Check i-th derivative of objective */
        approx = (fxp-fxm)/(2*h);
        derr = fabs(g[i] - approx)/fmax(1,fabs(g[i]));
        if (derr > der_max) {
            if (header_unset) {
              fprintf(stderr, "%10s  %22s  %22s  %7s\n",
              "Variable", "AD", "Numerical", "Error");
              header_unset = FALSE_;
            } 
            nerr++;
            fprintf(stderr, "%10s  %22.15e  %22.15e  %7.1e\n",
                    Vnames[i], g[i], approx, derr);
        }
    }
    FREE(g);
    if (constrained) {
        FREE(cxp);
        FREE(cxm);
    }

    /* Get CUTEst statistics */
    CUTEST_creport( &status, calls, cpu);
    if (status) {
      printf("CUTEst error.\nAborting.\n");
      exit(2);
    }

    printf("\n\n ************************ CUTEst statistics");
    printf(" ************************\n\n");
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
    printf(" # constraints functions = %-15.7g\n", calls[4]);
    printf(" # constraints gradients = %-15.7g\n", calls[5]);
    printf(" # constraints Hessians  = %-15.7g\n", calls[6]);
    printf(" Exit code               = %-10d\n", nerr);
    printf(" Final f                 = %-15.7g\n", fxp);
    printf(" Set up time             = %-10.2f seconds\n", cpu[0]);
    printf(" Solve time              = %-10.2f seconds\n", cpu[1]);
    printf(" *********************************************");
    printf("*********************\n\n");

    ierr = 0;
    FORTRAN_close(&funit, &ierr);
    if (ierr)
        printf("Error closing %s on unit %d.\n", fname, (int)funit);

    /* Free workspace */
    FREE(pname);
    FREE(x); FREE(bl); FREE(bu);
    FREE(v); FREE(cl); FREE(cu);
    FREE(equatn);
    FREE(linear);

    /* Free memory for variable names */
    for (i = 0; i < CUTEst_nvar; i++) FREE(Vnames[i]);
    FREE(Vnames);

    /* Free memory for constraint names */
    for (i = 0; i < CUTEst_ncon; i++) FREE(Gnames[i]);
    if (constrained) FREE(Gnames);

    CUTEST_uterminate( &status );

    return 0;

    }

    void getinfo( integer n, integer m, rp_ *bl, rp_ *bu,
		  rp_ *cl, rp_ *cu, logical *equatn, 
                  logical *linear, VarTypes *vartypes ) {

	int i;

	vartypes->nlin = 0; vartypes->neq = 0; vartypes->nbnds = 0;
	vartypes->nrange = 0;
	vartypes->nlower = 0; vartypes->nupper = 0; vartypes->nineq = 0;
	vartypes->nineq_lin = 0; vartypes->nineq_nlin = 0;
	vartypes->neq_lin = 0; vartypes->neq_nlin = 0;

	for( i = 0; i < n; i++ )
	    if( bl[i] > -CUTE_INF || bu[i] < CUTE_INF ) vartypes->nbnds++;
	for( i = 0; i < m; i++ ) {
		if( linear[i] ) vartypes->nlin++;
		if( equatn[i] ) {
			vartypes->neq++;
			if( linear[i] )
				vartypes->neq_lin++;
			else
				vartypes->neq_nlin++;
		} else {
			if( cl[i] > -CUTE_INF ) {
				if( cu[i] < CUTE_INF )
					vartypes->nrange++;
				else {
					vartypes->nlower++; vartypes->nineq++;
                }
			} else {
				if( cu[i] < CUTE_INF ) {
					vartypes->nupper++; vartypes->nineq++;
                }
			}
			if( !equatn[i] && linear[i] ) {
				vartypes->nineq_lin++;
			} else {
				vartypes->nineq_nlin++;
			}
		}
	}
	return;
    }

#ifdef __cplusplus
}    /* Closing brace for  extern "C"  block */
#endif
