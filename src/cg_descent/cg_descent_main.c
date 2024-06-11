/* THIS VERSION: CUTEST 2.3 - 2024-06-11 AT 11:40 GMT */

/* ====================================================
 * CUTEst interface for cg_descent     April. 5, 2014
 *
 * W. Hager
 *
 * (Based on CUTEr gencma.c of D. Orban, Feb 3, 2003)
 * (CUTEst evolution, Nick Gould, Apr 2, 2014)
 * ====================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define CG_DESCENTMA

#define MAXLINE 256

#ifdef __cplusplus
extern "C" {   /* To prevent C++ compilers from mangling symbols */
#endif

/*
#include "../../include/cutest.h"
#include "../../include/cg_user.h"
*/

#include "cutest.h"
#include "cutest_routines.h"
#include "cg_user.h"

  /*
#ifdef Isg95
#define MAINENTRY MAIN_
#else
#define MAINENTRY main
#endif
  */

/* prototypes */
rp_ cg_value
(
    rp_ *x,
    INT     n
) ;

void cg_grad
(
    rp_  *g,
    rp_  *x,
    INT      n
) ;

rp_ cg_valgrad
(
    rp_  *g,
    rp_  *x,
    INT      n
) ;

/* global variables */
    integer CUTEst_nvar;        /* number of variables */
    integer CUTEst_ncon;        /* number of constraints */

/* main program */
    int MAINENTRY( void ) {

        /* wall clock: */
/*      struct timeval tv ;
        int sec, usec ;
        rp_ walltime ; */
        char *fname = "OUTSDIF.d"; /* CUTEst data file */
        integer funit = 42;        /* FORTRAN unit number for OUTSDIF.d */
        integer io_buffer = 11;    /* FORTRAN unit for internal i/o */
        integer iout = 6;          /* FORTRAN unit number for error output */
        integer ierr;              /* Exit flag from OPEN and CLOSE */
        integer status;            /* Exit flag from CUTEst tools */
        rp_  grad_tol = 1.e-6; /* required gradient tolerance */

        VarTypes vtypes;

        integer    ncon_dummy ;
        rp_ *x, *bl, *bu ;
        char       *pname, *vnames ;
        logical     efirst = FALSE_, lfirst = FALSE_, nvfrst = FALSE_, grad;
        logical     constrained = FALSE_;

        rp_  calls[7], cpu[4];
        integer     nlin = 0, nbnds = 0, neq = 0;
        integer     ExitCode;
        int         i, status_cg_descent ;
        char        fgets_status ;
        char        line[1024];
        char        s [MAXLINE+1] ;

        FILE *spec ;
        cg_stats Stats ;
        cg_parameter cg_parm ;

        /* Open problem description file OUTSDIF.d */
        ierr = 0;

        printf("a\n") ;
        FORTRAN_open( &funit, fname, &ierr ) ;
        if( ierr ) {
            printf("Error opening file OUTSDIF.d.\nAborting.\n") ;
            exit(1) ;
        }

        /* Get problem name (this works under gfortran, but not all compilers*/
        /*
        MALLOC( pname,  FSTRING_LEN+1, char );
        CUTEST_pname_r( &status, &funit, pname ) ;
        if (status) {
            printf("** CUTEst error, status = %d, aborting\n", status);
            exit(status);
        }
        */
        /* Make sure to null-terminate problem name */
        /*
        pname[FSTRING_LEN] = '\0';
        i = FSTRING_LEN - 1;
        while(i-- > 0 && pname[i] == ' ') {
          pname[i] = '\0';
        }
        */
        /* printf (" ** the problem is %s\n", pname ) ; */

        /* Determine problem size */
        CUTEST_cdimen_r( &status, &funit, &CUTEst_nvar, &CUTEst_ncon) ;
        if (status) {
            printf("** CUTEst error, status = %d, aborting\n", status);
            exit(status);
        }
        /* Determine whether to call constrained or unconstrained tools */
        if( CUTEst_ncon ) constrained = TRUE_;

        /* stop if the problem has constraints */
        if( constrained ) {
           printf (" ** the problem %s has %i constraints\n",
                      pname,  CUTEst_ncon ) ;
           printf ("    cg_descent is for unconstrained optimization\n") ;
           exit( -1 ) ;
        }

        /* Seems to be needed for some Solaris C compilers */
        ncon_dummy = CUTEst_ncon + 1;

        /* Reserve memory for variables, bounds, and multipliers */
        /* and call appropriate initialization routine for CUTEst */
        MALLOC( x,  CUTEst_nvar, rp_ ) ;
        MALLOC( bl, CUTEst_nvar, rp_ ) ;
        MALLOC( bu, CUTEst_nvar, rp_ ) ;
        CUTEST_usetup_r( &status, &funit, &iout, &io_buffer, &CUTEst_nvar,
                         x, bl, bu ) ;
        if (status) {
            printf("** CUTEst error, status = %d, aborting\n", status);
            exit(status);
        }

        /* Get problem name */

        MALLOC( pname,  FSTRING_LEN+1, char );
        CUTEST_probname_r( &status, pname ) ;
        if (status) {
            printf("** CUTEst error, status = %d, aborting\n", status);
            exit(status);
        }

        /* Make sure to null-terminate problem name */

        pname[FSTRING_LEN] = '\0';
        i = FSTRING_LEN - 1;
        while(i-- > 0 && pname[i] == ' ') {
          pname[i] = '\0';
        }

        /*printf ("Problem: %s (n = %i)\n", pname, CUTEst_nvar ) ;*/

        /* MALLOC(vnames, CUTEst_nvar*FSTRING_LEN, char);
           CUTEST_unames_r( &status, &CUTEst_nvar, pname, vnames);
           if( status ) {
              printf("** CUTEst error, status = %d, aborting\n", status);
              exit(status);
           }
           FREE(vnames) ;
        */

        /* Set any parameter values here
           First read in the default parameter values */
        cg_default (&cg_parm) ;

        /* Parameter values are overwritten using any values stored in the
           CG_DESCENT.SPC file. The format of the file is parameter name at
           the start of the line followed by one or more spaces and then
           the parameter value.  A new value for grad_tol, the solution
           tolerance, can also be specified in the SPC file.  Note that
           the parameter names are case sensitive.  See cg_user.h for
           defaults and for the parameter names and descriptions. */

        spec = fopen ("CG_DESCENT.SPC", "r") ;

#ifdef REAL_32
        char pg[ ]="%g";
#else
        char pg[ ]="%lg";
#endif

        if ( spec != NULL )
        {
           while (fgets (s, MAXLINE, spec) != (char *) NULL)
           {
               int sl ;
               /* determine the parameter and its value */
               sl = strlen("grad_tol") ;
               if (strncmp (s, "grad_tol", sl) == 0)
               {
                   sscanf (s+sl, pg, &grad_tol) ;
                   continue ;
               }
               sl = strlen("PrintFinal") ;
               if (strncmp (s, "PrintFinal", sl) == 0)
               {
                   sscanf (s+sl, "%d", &cg_parm.PrintFinal) ;
                   continue ;
               }
               sl = strlen("PrintLevel") ;
               if (strncmp (s, "PrintLevel", sl) == 0)
               {
                   sscanf (s+sl, "%d", &cg_parm.PrintLevel) ;
                   continue ;
               }
               sl = strlen("PrintParms") ;
               if (strncmp (s, "PrintParms", sl) == 0)
               {
                   sscanf (s+sl, "%d", &cg_parm.PrintParms) ;
                   continue ;
               }
               sl = strlen("LBFGS") ;
               if (strncmp (s, "LBFGS", sl) == 0)
               {
                   sscanf (s+sl, "%d", &cg_parm.LBFGS) ;
                   continue ;
               }
               sl = strlen("memory") ;
               if (strncmp (s, "memory", sl) == 0)
               {
                   sscanf (s+sl, "%d", &cg_parm.memory) ;
                   continue ;
               }
               sl = strlen("SubCheck") ;
               if (strncmp (s, "SubCheck", sl) == 0)
               {
                   sscanf (s+sl, "%d", &cg_parm.SubCheck) ;
                   continue ;
               }
               sl = strlen("SubSkip") ;
               if (strncmp (s, "SubSkip", sl) == 0)
               {
                   sscanf (s+sl, "%d", &cg_parm.SubSkip) ;
                   continue ;
               }
               sl = strlen("eta0") ;
               if (strncmp (s, "eta0", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.eta0) ;
                   continue ;
               }
               sl = strlen("eta1") ;
               if (strncmp (s, "eta1", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.eta1) ;
                   continue ;
               }
               sl = strlen("eta2") ;
               if (strncmp (s, "eta2", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.eta2) ;
                   continue ;
               }
               sl = strlen("AWolfe") ;
               if (strncmp (s, "AWolfe", sl) == 0)
               {
                   sscanf (s+sl, "%d", &cg_parm.AWolfe) ;
                   continue ;
               }
               sl = strlen("AWolfeFac") ;
               if (strncmp (s, "AWolfeFac", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.AWolfeFac) ;
                   continue ;
               }
               sl = strlen("Qdecay") ;
               if (strncmp (s, "Qdecay", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.Qdecay) ;
                   continue ;
               }
               sl = strlen("StopRule") ;
               if (strncmp (s, "StopRule", sl) == 0)
               {
                   sscanf (s+sl, "%d", &cg_parm.StopRule) ;
                   continue ;
               }
               sl = strlen("StopFac") ;
               if (strncmp (s, "StopFac", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.StopFac) ;
                   continue ;
               }
               sl = strlen("PertRule") ;
               if (strncmp (s, "PertRule", sl) == 0)
               {
                   sscanf (s+sl, "%d", &cg_parm.PertRule) ;
                   continue ;
               }
               sl = strlen("eps") ;
               if (strncmp (s, "eps", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.eps) ;
                   continue ;
               }
               sl = strlen("egrow") ;
               if (strncmp (s, "egrow", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.egrow) ;
                   continue ;
               }
               sl = strlen("QuadStep") ;
               if (strncmp (s, "QuadStep", sl) == 0)
               {
                   sscanf (s+sl, "%d", &cg_parm.QuadStep) ;
                   continue ;
               }
               sl = strlen("QuadCutOff") ;
               if (strncmp (s, "QuadCutOff", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.QuadCutOff) ;
                   continue ;
               }
               sl = strlen("QuadSafe") ;
               if (strncmp (s, "QuadSafe", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.QuadSafe) ;
                   continue ;
               }
               sl = strlen("UseCubic") ;
               if (strncmp (s, "UseCubic", sl) == 0)
               {
                   sscanf (s+sl, "%d", &cg_parm.UseCubic) ;
                   continue ;
               }
               sl = strlen("CubicCutOff") ;
               if (strncmp (s, "CubicCutOff", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.CubicCutOff) ;
                   continue ;
               }
               sl = strlen("SmallCost") ;
               if (strncmp (s, "SmallCost", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.SmallCost) ;
                   continue ;
               }
               sl = strlen("debug") ;
               if (strncmp (s, "debug", sl) == 0)
               {
                   sscanf (s+sl, "%d", &cg_parm.debug) ;
                   continue ;
               }
               sl = strlen("debugtol") ;
               if (strncmp (s, "debugtol", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.debugtol) ;
                   continue ;
               }
               sl = strlen("step") ;
               if (strncmp (s, "step", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.step) ;
                   continue ;
               }
               sl = strlen("maxit") ;
               if (strncmp (s, "maxit", sl) == 0)
               {
                   sscanf (s+sl, "%ld", &cg_parm.maxit) ;
                   continue ;
               }
               sl = strlen("ntries") ;
               if (strncmp (s, "ntries", sl) == 0)
               {
                   sscanf (s+sl, "%d", &cg_parm.ntries) ;
                   continue ;
               }
               sl = strlen("ExpandSafe") ;
               if (strncmp (s, "ExpandSafe", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.ExpandSafe) ;
                   continue ;
               }
               sl = strlen("SecantAmp") ;
               if (strncmp (s, "SecantAmp", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.SecantAmp) ;
                   continue ;
               }
               sl = strlen("RhoGrow") ;
               if (strncmp (s, "RhoGrow", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.RhoGrow) ;
                   continue ;
               }
               sl = strlen("neps") ;
               if (strncmp (s, "neps", sl) == 0)
               {
                   sscanf (s+sl, "%d", &cg_parm.neps) ;
                   continue ;
               }
               sl = strlen("nshrink") ;
               if (strncmp (s, "nshrink", sl) == 0)
               {
                   sscanf (s+sl, "%d", &cg_parm.nshrink) ;
                   continue ;
               }
               sl = strlen("nline") ;
               if (strncmp (s, "nline", sl) == 0)
               {
                   sscanf (s+sl, "%d", &cg_parm.nline) ;
                   continue ;
               }
               sl = strlen("restart_fac") ;
               if (strncmp (s, "restart_fac", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.restart_fac) ;
                   continue ;
               }
               sl = strlen("feps") ;
               if (strncmp (s, "feps", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.feps) ;
                   continue ;
               }
               sl = strlen("nan_rho") ;
               if (strncmp (s, "nan_rho", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.nan_rho) ;
                   continue ;
               }
               sl = strlen("nan_decay") ;
               if (strncmp (s, "nan_decay", sl) == 0)
               {
                   sscanf (s+sl, pg, &cg_parm.nan_decay) ;
                   continue ;
               }
           }
           fclose (spec) ;
        }

        /* Call the optimizer */

        /* wall clock: */
/*      gettimeofday (&tv, NULL) ;
        sec = tv.tv_sec ;
        usec = tv.tv_usec ; */
        status_cg_descent = cg_descent (x, CUTEst_nvar, &Stats, &cg_parm,
                            grad_tol, cg_value, cg_grad, cg_valgrad, NULL) ;
/*      gettimeofday (&tv, NULL) ;
        walltime = tv.tv_sec - sec + (rp_) (tv.tv_usec - usec) /1.e6 ;*/

        ExitCode = 0;

        /* Get CUTEst statistics */
        CUTEST_creport_r( &status, calls, cpu) ;
        if (status) {
          printf("** CUTEst error, status = %d, aborting\n", status);
          exit(status);
        }

        /* print statistics if so desired */
        /*
        printf ("%10s %6i %7li %7li %7li %5i %16.7f %16.7f %9.3f\n",
            pname, CUTEst_nvar, Stats.iter, Stats.nfunc, Stats.ngrad,
            status_cg_descent, Stats.gnorm, Stats.f, cpu [1]) ;
        */
/*          status, Stats.gnorm, Stats.f, walltime) ;*/

        printf(" *********************** CUTEst statistics ************************\n") ;
        printf(" Code used               : cg_descent\n") ;
        printf(" Problem                 : %-s\n", pname) ;
        printf(" # variables             = %-10d\n", CUTEst_nvar) ;
        /*        printf(" # bound constraints     = %-10d\n", vtypes.nbnds) ;*/
        printf(" # iterations            = %li\n", Stats.iter) ;
        printf(" # objective functions   = %-15.7g\n", calls[0]) ;
        printf(" # objective gradients   = %-15.7g\n", calls[1]) ;
        printf(" # objective Hessians    = %-15.7g\n", calls[2]) ;
        printf(" # Hessian-vector prdct  = %-15.7g\n", calls[3]) ;
        printf(" Exit code               = %-10d\n", ExitCode) ;
        printf(" Final f                 = %-15.7g\n",Stats.f) ;
        printf(" Final ||g||             = %-15.7g\n",Stats.gnorm) ;
        printf(" Set up time             = %-10.2f seconds\n", cpu[0]) ;
        printf(" Solve time              = %-10.2f seconds\n", cpu[1]) ;
        printf(" ******************************************************************\n") ;

        ierr = 0;
        FORTRAN_close( &funit, &ierr ) ;
        if( ierr ) {
            printf( "Error closing %s on unit %d.\n", fname, funit ) ;
            printf( "Trying not to abort.\n" ) ;
        }

        /* Free workspace */
        FREE( pname ) ;
        FREE( x ) ; FREE( bl ) ; FREE( bu ) ;

        CUTEST_uterminate_r( &status ) ;

        return 0;
    }

#ifdef __cplusplus
}    /* Closing brace for  extern "C"  block */
#endif
rp_ cg_value
(
    rp_ *x,
    INT     n
)
{
    rp_ f ;
    integer status;

    CUTEST_ufn_r( &status, &CUTEst_nvar, x, &f) ;
    if ((status == 1) || (status == 2)) {
        printf("** CUTEst error, status = %d, aborting\n", status);
        exit(status);
    }

    return (f) ;
}

void cg_grad
(
    rp_  *g,
    rp_  *x,
    INT      n
)
{
    integer status;
    CUTEST_ugr_r( &status, &CUTEst_nvar, x, g) ;
    if ((status == 1) || (status == 2)) {
        printf("** CUTEst error, status = %d, aborting\n", status);
        exit(status);
    }
}

rp_ cg_valgrad
(
    rp_  *g,
    rp_  *x,
    INT      n
)
{
    logical grad ;
    rp_ f ;
    integer status;
    grad = 1 ;
    CUTEST_uofg_r( &status, &CUTEst_nvar, x, &f, g, &grad ) ;
    if ((status == 1) || (status == 2)) {
        printf("** CUTEst error, status = %d, aborting\n", status);
        exit(status);
    }
    return (f) ;
}
