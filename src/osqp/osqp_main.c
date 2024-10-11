/* THIS VERSION: CUTEST 2.3 - 2024-10-11 AT 09:00 GMT */

/* ===========================================
 * CUTEst interface to OSQP
 *
 * Nick Gould Dec 11 2017
 *
 * Take a look at $CUTEST/include/cutest.h and
 * $CUTEST/src/loqo/loqoma.c for more examples
 * ===========================================
 */

/*
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
*/
#include "osqp.h"

#ifdef __cplusplus
extern "C" {   /* To prevent C++ compilers from mangling symbols */
#endif

#include "cutest.h"
#include "cutest_routines.h"

#define MAXLINE 256

integer CUTEst_nvar;  /* number of variables */
integer CUTEst_ncon;  /* number of constraints */
integer CUTEst_nnza;  /* number of nonzeros in Jacobian */
integer CUTEst_nnzh;  /* number of nonzeros in upper triangular
                        part of the Hessian of the Lagrangian */

void coo2csc(integer n, integer nz, rp_ *coo_val, integer *coo_row,
             integer *coo_col, c_float *csr_val, c_int *csr_col,
             c_int *row_start);
void sort(c_int *ind, c_float *val, c_int start, c_int end);
void getinfo( integer, integer, rp_*, rp_*,
              rp_*, rp_*, logical*, logical*,
              VarTypes* );

/* main program for calls to OSQP */

int MAINENTRY(void) {

     char *fname = "OUTSDIF.d"; /* CUTEst data file */
     integer funit = 42;        /* FORTRAN unit number for OUTSDIF.d */
     integer io_buffer = 11;    /* FORTRAN unit for internal i/o */
     integer iout = 6;          /* FORTRAN unit number for error output */
     integer ierr;              /* Exit flag from OPEN and CLOSE */
     integer status;            /* Exit flag from CUTEst tools */

     VarTypes vtypes;

     integer ncon_dummy, nb, ncon_total, nnza_dummy, nnzh_dummy, a_ne;
     integer *A_row, *A_col, *H_row, *H_col;

     rp_ f;
     rp_ *x, *x0, *xl, *xu, *A_val, *H_val;
     rp_ *y = NULL, *y0 = NULL, *c = NULL, *cl = NULL, *cu = NULL;

     logical *equatn = NULL, *linear = NULL;

     c_int osqp_m, osqp_n, osqp_P_nnz, osqp_A_nnz ;
     c_int *osqp_P_i, *osqp_P_p, *osqp_A_i, *osqp_A_p ;
     c_float *osqp_q, *osqp_l, *osqp_u, *osqp_P_x, *osqp_A_x ;

     char *pname, *xnames, *cnames, *cptr;
     char **Xnames, **Cnames; /* xnames and cnames as arrays of strings */
     logical grlagf;
     integer e_order = 0, l_order = 0, v_order = 0;
     logical constrained = FALSE_;

     rp_ calls[7], cpu[4];
     integer nlin = 0, nbnds = 0, neq = 0;
     rp_ r_string;
     rp_ dummy;
     int i_string;
     integer ExitCode;
     int i, j, m, n, nc ;
     c_int osqp_status ;

     FILE *spec, *solution, *results ;

     rp_ h, fxp, fxm, approx, derr, xi;
     rp_ *cxp, *cxm, *g;
     int nerr = 0;
     char s [MAXLINE+1] ;

     /* open problem description file OUTSDIF.d */
     ierr = 0;
     FORTRAN_open(&funit, fname, &ierr);
     if (ierr) {
         printf("Error opening file OUTSDIF.d.\nAborting.\n");
         exit(1);
     }

     /* determine problem size */

     CUTEST_cdimen( &status, &funit, &CUTEst_nvar, &CUTEst_ncon);
     if (status) {
         printf("CUTEst error.\nAborting.\n");
         exit(2);
     }
     n = CUTEst_nvar; m = CUTEst_ncon;

     /* determine whether to call constrained or unconstrained tools */

     if (CUTEst_ncon) constrained = TRUE_;

     /* seems to be needed for some Solaris C compilers */
     ncon_dummy = CUTEst_ncon + 1;

     /* reserve memory for variables, bounds, and multipliers */
     /* and call appropriate initialization routine for CUTEst */

     MALLOC(x, CUTEst_nvar, rp_);
     MALLOC(xl, CUTEst_nvar, rp_);
     MALLOC(xu, CUTEst_nvar, rp_);
     if (constrained) {
       MALLOC(equatn, CUTEst_ncon+1, logical);
       MALLOC(linear, CUTEst_ncon+1, logical);
       MALLOC(y, CUTEst_ncon, rp_);
       MALLOC(cl, CUTEst_ncon, rp_);
       MALLOC(cu, CUTEst_ncon, rp_);
       CUTEST_csetup( &status, &funit, &iout, &io_buffer,
                        &CUTEst_nvar, &CUTEst_ncon, x, xl, xu,
                        y, cl, cu, equatn, linear,
                        &e_order, &l_order, &v_order );
       if (status) {
           printf("CUTEst error.\nAborting.\n");
           exit(2);
       }
      FREE(y);
     } else {
       CUTEST_usetup( &status, &funit, &iout, &io_buffer, &CUTEst_nvar,
                        x, xl, xu);
       if (status) {
           printf("CUTEst error.\nAborting.\n");
           exit(2);
       }
     }

     /* get problem, variables and constraints names */

     MALLOC(pname, FSTRING_LEN+1, char);
     MALLOC(xnames, CUTEst_nvar * FSTRING_LEN, char);  /* For Fortran */
     MALLOC(Xnames, CUTEst_nvar, char*);               /* Array of strings */
     for (i = 0; i < CUTEst_nvar; i++)
       MALLOC(Xnames[i], FSTRING_LEN+1, char);

     if (constrained) {
         MALLOC(cnames, CUTEst_ncon * FSTRING_LEN, char); /* For Fortran */
         MALLOC(Cnames, CUTEst_ncon, char*);              /* Array of strings */
         for (i = 0; i < CUTEst_ncon; i++)
             MALLOC(Cnames[i], FSTRING_LEN+1, char);
         CUTEST_cnames( &status, &CUTEst_nvar, &CUTEst_ncon,
                          pname, xnames, cnames);
         if (status) {
             printf("CUTEst error.\nAborting.\n");
             exit(2);
         }
     } else {
         CUTEST_unames( &status, &CUTEst_nvar, pname, xnames);
         if (status) {
             printf("CUTEst error.\nAborting.\n");
             exit(2);
         }
     }

     /* Make sure to null-terminate problem name */

     pname[FSTRING_LEN] = '\0';

    /*  write a summary of the results to a file */

    results = fopen ("OSQP.res", "a") ;
    if ( results != NULL )
    {
      fprintf( results, "%-12s %7d %7d", pname, n, m );
    }

     /* transfer variables and constraint names into arrays of
      * null-terminated strings.
      * If you know of a simpler way to do this portably, let us know!
      */

     for (i = 0; i < CUTEst_nvar; i++) {
       cptr = xnames + i * FSTRING_LEN;
       for (j = 0; j < FSTRING_LEN; j++) {
         Xnames[i][j] = *cptr;
         cptr++;
       }
       Xnames[i][FSTRING_LEN] = '\0';
     }

     for (i = 0; i < CUTEst_ncon; i++) {
       cptr = cnames + i * FSTRING_LEN;
       for (j = 0; j < FSTRING_LEN; j++) {
         Cnames[i][j] = *cptr;
         cptr++;
       }
       Cnames[i][FSTRING_LEN] = '\0';
     }

     /* Fortran strings no longer needed */

     FREE(xnames);
     if (constrained) FREE(cnames);

     /* Obtain basic info on problem */

      if (constrained) {
         getinfo( CUTEst_nvar, CUTEst_ncon, xl, xu, cl, cu, equatn,
                  linear, &vtypes );
       FREE(equatn); FREE(linear);
     } else {
         equatn[0] = FALSE_;
         linear[0] = FALSE_;
         getinfo( CUTEst_nvar, 1, xl, xu, cl, cu, equatn,
                  linear, &vtypes );
     }

     /* set x0 to zero to determine the constant and derivative terms for the
        problem functions */

     MALLOC(x0, CUTEst_nvar, rp_);
     for (i = 0; i < CUTEst_nvar; i++) {
       x0[i] = 0.0;
     }

     /* evaluate the problem functions at x0 */

     if (constrained) {
       MALLOC(c, CUTEst_ncon, rp_);
       CUTEST_cfn( &status, &CUTEst_nvar, &CUTEst_ncon, x0, &f, c);
       if (status) {
         printf("CUTEst error.\nAborting.\n");
         exit(2);
       }
     } else {
       CUTEST_ufn( &status, &CUTEst_nvar, x0, &f);
       if (status) {
         printf("CUTEst error.\nAborting.\n");
         exit(2);
       }
     }

     /* include variables with simple bounds in the constraint tally */

     nb =  0 ;
     for (i = 0; i < CUTEst_nvar; i++) {
       if ( ( xl[i] > - OSQP_INFTY ) || ( xu[i]  < OSQP_INFTY ) ) nb++ ;
     }

  /* set scalar and vector data as required by osqp */

     ncon_total = CUTEst_ncon + nb;
     osqp_n = CUTEst_nvar ;
     osqp_m = ncon_total ;

     MALLOC( osqp_l, ncon_total, c_float );
     MALLOC( osqp_u, ncon_total, c_float );

     if (constrained) {
       for (i = 0; i < CUTEst_ncon; i++) {
         if ( cl[i] > - OSQP_INFTY ) {
           osqp_l[i] = cl[i] - c[i];
         } else {
           osqp_l[i] = cl[i] ;
         }
         if ( cu[i] < OSQP_INFTY ) {
           osqp_u[i] = cu[i] - c[i];
         } else {
           osqp_u[i] = cu[i] ;
         }
       }
       FREE(c);
     }

     /*
    for (i = 0 ; i < CUTEst_nvar; i++) {
      printf( "xl,u[%d]=%g, %g\n", xl[i], xu[i]);
    }
     */
     ncon_total = CUTEst_ncon - 1;
     for (i = 0; i < CUTEst_nvar; i++) {
       if ( ( xl[i] > - OSQP_INFTY ) || ( xu[i]  < OSQP_INFTY ) ) {
         ncon_total++ ;
         osqp_l[ncon_total] = xl[i] ;
         osqp_u[ncon_total] = xu[i] ;
       }
     }

  /* determine the number of nonzeros in the Hessian and, if needed, Jacobian */

     CUTEST_cdimsh( &status, &CUTEst_nnzh );
     MALLOC(g, CUTEst_nvar, rp_);
     MALLOC( H_val, CUTEst_nnzh, rp_ );
     MALLOC( H_row, CUTEst_nnzh, integer );
     MALLOC( H_col, CUTEst_nnzh, integer );

     if ( status ) {
         printf("** CUTEst error, status = %d, aborting\n", status);
         exit(status);
     }

   /* evaluate the gradient, Jacobian (if needed) and Hessian at x0 */

     a_ne = - 1;

     if (constrained) {

       /* Determine the number of nonzeros in Jacobian */
       CUTEST_cdimsj( &status, &CUTEst_nnza );

       if( status ) {
          printf("** CUTEst error, status = %d, aborting\n", status);
          exit(status);
       }

      /* set y0 to zero to determine the derivative terms for the
        problem functions */

       MALLOC(y0, CUTEst_ncon, rp_);
       for (i = 0; i < CUTEst_ncon; i++) {
         y0[i] = 0.0;
       }

       MALLOC( A_val, CUTEst_nnza + nb, rp_ );
       MALLOC( A_row, CUTEst_nnza + nb, integer );
       MALLOC( A_col, CUTEst_nnza + nb, integer );
       grlagf = FALSE_;
       /* Here, dummys will be set to nnza/nnzh again */
       CUTEST_csgrsh( &status, &CUTEst_nvar, &CUTEst_ncon, x0, y0, &grlagf,
                        &nnza_dummy, &CUTEst_nnza, A_val, A_col, A_row,
                        &nnzh_dummy, &CUTEst_nnzh, H_val, H_row, H_col );
       /*
       printf(" h_nnz = %d %d\n", nnzh_dummy, CUTEst_nnzh );
       */

       if( status ) {
          printf("** CUTEst error, status = %d, aborting\n", status);
          exit(status);
       }
       FREE(y0);

       /* Convert A to 0-based indexing */

       for (i = 0; i < CUTEst_nnza; i++) {
        A_row[i]--;
        A_col[i]--;
       }

        /* untangle the objective gradient and Jacobian */

       for (i = 0; i < CUTEst_nvar; i++) {
         g[i] = 0.0;
       }

       for (i = 0; i < CUTEst_nnza; i++) {
         if ( A_val[ i ] != 0.0 ) {
           if ( A_row[ i ] >= 0 ) {
             a_ne++;
             A_row[ a_ne ] = A_row[ i ];
             A_col[ a_ne ] = A_col[ i ];
             A_val[ a_ne ] = A_val[ i ];
           } else {
             g[ A_col[ i ] ] = A_val[ i ];
           }
         }
       }
     } else {
       CUTEST_ugrsh( &status, &CUTEst_nvar, x0, g,
                       &nnzh_dummy, &CUTEst_nnzh, H_val, H_row, H_col );
     }

     /* Convert H to 0-based indexing */

     for (i = 0; i < CUTEst_nnzh; i++) {
      H_row[i]--;
      H_col[i]--;
     }

     /*
     printf(" H \n" );
     for (i = 0; i < CUTEst_nnzh; i++) {
       printf(" %g %d %d \n",  H_val[i], H_row[i], H_col[i] );
     }
     */

/* insert simple bounds on the variables into the Jacobian */

     nc = CUTEst_ncon - 1;
     for (i = 0; i < CUTEst_nvar; i++) {
       if ( ( xl[i] > - OSQP_INFTY ) || ( xu[i]  < OSQP_INFTY ) ) {
         a_ne++ ;
         nc++ ;
         A_row[ a_ne ] = nc;
         A_col[ a_ne ] = i;
         A_val[ a_ne ] = 1.0;
       }
     }
     CUTEst_nnza = a_ne + 1;
     FREE(x) ;

     /*
     printf(" A \n" );
     for (i = 0; i < CUTEst_nnza; i++) {
       printf(" %g %d %d \n",  A_val[i], A_row[i], A_col[i] );
     }
     */

     MALLOC( osqp_q, CUTEst_nvar, c_float );
     for (i = 0; i < CUTEst_nvar; i++) {
       osqp_q[i] = g[i];
     }
     FREE(g);

  /* convert A and H from co-ordinate to compressed-sparse column format */

     osqp_P_nnz = CUTEst_nnzh;
     MALLOC( osqp_P_x, CUTEst_nnzh, c_float );
     MALLOC( osqp_P_i, CUTEst_nnzh, c_int );
     MALLOC( osqp_P_p, CUTEst_nvar + 1, c_int );
     coo2csc( CUTEst_nvar, CUTEst_nnzh, H_val, H_row, H_col,
              osqp_P_x, osqp_P_i, osqp_P_p );
     FREE( H_row ) ; FREE( H_col ) ; FREE( H_val ) ;

     osqp_A_nnz = CUTEst_nnza;
     MALLOC( osqp_A_x, CUTEst_nnza, c_float );
     MALLOC( osqp_A_i, CUTEst_nnza, c_int );
     MALLOC( osqp_A_p, CUTEst_nvar + 1, c_int );
     coo2csc( CUTEst_nvar, CUTEst_nnza, A_val, A_row, A_col,
              osqp_A_x, osqp_A_i, osqp_A_p );
     FREE( A_val ) ; FREE( A_row ) ; FREE( A_col ) ;

/* populate data */

    OSQPData * data = (OSQPData *)c_malloc(sizeof(OSQPData));
    data->n = osqp_n;
    data->m = osqp_m;
    data->P = csc_matrix(data->n, data->n, osqp_P_nnz, osqp_P_x, osqp_P_i,
                         osqp_P_p);
    data->q = osqp_q;
    data->A = csc_matrix(data->m, data->n, osqp_A_nnz, osqp_A_x, osqp_A_i,
                         osqp_A_p);
    data->l = osqp_l;
    data->u = osqp_u;

    /*
    printf(" m, n, nb = %d, %d, %d\n", data->m, data->n, nb );
    printf(" p_nnz, a_nnz = %d, %d\n", osqp_P_nnz, osqp_A_nnz );
    for (i = 0; i < data->n; i++) {
      printf(" q = %g\n", osqp_q[i] );
    }
    for (i = 0; i < data->m; i++) {
      printf(" l, u = %g, %g\n", osqp_l[i], osqp_u[i] );
    }

    for (j = 0; j < data->n; j++) {
      for (i = osqp_P_p[j]; i < osqp_P_p[j+1]; i++) {
        printf(" h, i, j = %g, %d, %d\n", osqp_P_x[i], j, osqp_P_i[i] );
      }
    }

    for (j = 0; j < data->n; j++) {
      for (i = osqp_A_p[j]; i < osqp_A_p[j+1]; i++) {
        printf(" a, i, j = %g, %d, %d\n", osqp_A_x[i], j, osqp_A_i[i] );
      }
    }
    */

/* define Solver settings as default */

    OSQPSettings * settings = (OSQPSettings *)c_malloc(sizeof(OSQPSettings));
    osqp_set_default_settings(settings);

/* Parameter settings are overwritten using any values stored in the
   OSQP.SPC file. The format of the file is parameter name at the start
   of the line followed by one or more spaces and then the parameter value.
   A new value for max_iter, the iteration limit, can be specified in the
   SPC file.  Note that the parameter names are case sensitive. See
   $OSQP/include/types.h for the parameter names and descriptions and
   $OSQP/include/constants.h for default values  */

#ifdef REAL_32
    char pg[ ]="%g";
#else
    char pg[ ]="%lg";
#endif

    spec = fopen ("OSQP.SPC", "r") ;
    if ( spec != NULL )
    {
       while (fgets (s, MAXLINE, spec) != (char *) NULL)
       {
           int sl ;

           /* determine the parameter and its value */

           sl = strlen("rho") ;
           if (strncmp (s, "rho", sl) == 0)
           {
               sscanf (s+sl, pg, &r_string );
               settings->rho = r_string ;
               continue ;
           }
           sl = strlen("sigma") ;
           if (strncmp (s, "sigma", sl) == 0)
           {
             sscanf (s+sl, pg, &r_string );
               settings->sigma = r_string ;
               continue ;
           }
           sl = strlen("scaling") ;
           if (strncmp (s, "scaling", sl) == 0)
           {
               sscanf (s+sl, "%d", &i_string );
               settings->scaling = i_string ;
               continue ;
           }
#if EMBEDDED != 1
           sl = strlen("adaptive_rho") ;
           if (strncmp (s, "adaptive_rho", sl) == 0)
           {
               sscanf (s+sl, "%d", &i_string );
               settings->adaptive_rho = i_string ;
               continue ;
           }
           sl = strlen("adaptive_rho_interval") ;
           if (strncmp (s, "adaptive_rho_interval", sl) == 0)
           {
               sscanf (s+sl, "%d", &i_string );
               settings->adaptive_rho_interval = i_string ;
               continue ;
           }
           sl = strlen("adaptive_rho_tolerance") ;
           if (strncmp (s, "adaptive_rho_tolerance", sl) == 0)
           {
             sscanf (s+sl, pg, &r_string );
               settings->adaptive_rho_tolerance = r_string ;
               continue ;
           }
#ifdef PROFILING
           sl = strlen("adaptive_rho_fraction") ;
           if (strncmp (s, "adaptive_rho_fraction", sl) == 0)
           {
             sscanf (s+sl, pg, &r_string );
               settings->adaptive_rho_fraction = r_string ;
               continue ;
           }
#endif // Profiling
#endif // EMBEDDED != 1
           sl = strlen("max_iter") ;
           if (strncmp (s, "max_iter", sl) == 0)
           {
               sscanf (s+sl, "%d", &i_string );
               settings->max_iter = i_string ;
               continue ;
           }
           sl = strlen("eps_abs") ;
           if (strncmp (s, "eps_abs", sl) == 0)
           {
             sscanf (s+sl, pg, &r_string );
               settings->eps_abs = r_string ;
               continue ;
           }
           sl = strlen("eps_rel") ;
           if (strncmp (s, "eps_rel", sl) == 0)
           {
             sscanf (s+sl, pg, &r_string );
               settings->eps_rel = r_string ;
               continue ;
           }
           sl = strlen("eps_prim_inf") ;
           if (strncmp (s, "eps_prim_inf", sl) == 0)
           {
             sscanf (s+sl, pg, &r_string );
               settings->eps_prim_inf = r_string ;
               continue ;
           }
           sl = strlen("eps_dual_inf") ;
           if (strncmp (s, "eps_dual_inf", sl) == 0)
           {
             sscanf (s+sl, pg, &r_string );
               settings->eps_dual_inf = r_string ;
               continue ;
           }
           sl = strlen("alpha") ;
           if (strncmp (s, "alpha", sl) == 0)
           {
             sscanf (s+sl, pg, &r_string );
               settings->alpha = r_string ;
               continue ;
           }
           sl = strlen("linsys_solver") ;
           if (strncmp (s, "linsys_solver", sl) == 0)
           {
               sscanf (s+sl, "%d", &i_string );
               settings->linsys_solver = i_string ;
               continue ;
           }
#ifndef EMBEDDED
           sl = strlen("delta") ;
           if (strncmp (s, "delta", sl) == 0)
           {
             sscanf (s+sl, pg, &r_string );
               settings->delta = r_string ;
               continue ;
           }
           sl = strlen("polish") ;
           if (strncmp (s, "polish", sl) == 0)
           {
               sscanf (s+sl, "%d", &i_string );
               settings->polish = i_string ;
               continue ;
           }
           sl = strlen("polish_refine_iter") ;
           if (strncmp (s, "polish_refine_iter", sl) == 0)
           {
               sscanf (s+sl, "%d", &i_string );
               settings->polish_refine_iter = i_string ;
               continue ;
           }
           sl = strlen("verbose") ;
           if (strncmp (s, "verbose", sl) == 0)
           {
               sscanf (s+sl, "%d", &i_string );
               settings->verbose = i_string ;
               continue ;
           }
#endif
           sl = strlen("scaled_termination") ;
           if (strncmp (s, "scaled_termination", sl) == 0)
           {
               sscanf (s+sl, "%d", &i_string );
               settings->scaled_termination = i_string ;
               continue ;
           }
           sl = strlen("check_termination") ;
           if (strncmp (s, "check_termination", sl) == 0)
           {
               sscanf (s+sl, "%d", &i_string );
               settings->check_termination = i_string ;
               continue ;
           }
           sl = strlen("warm_start") ;
           if (strncmp (s, "warm_start", sl) == 0)
           {
               sscanf (s+sl, "%d", &i_string );
               settings->warm_start = i_string ;
               continue ;
           }
#ifdef PROFILING
           sl = strlen("time_limit") ;
           if (strncmp (s, "time_limit", sl) == 0)
           {
             sscanf (s+sl, pg, &r_string );
               settings->time_limit = r_string ;
               continue ;
           }
#endif // Profiling
       }
       fclose (spec) ;
    }

   /* setup workspace */

    OSQPWorkspace * work;

    work = osqp_setup(data, settings);

   /* solve Problem */

    osqp_status = osqp_solve(work);

    if (osqp_status) {
      /*     printf( "osqp_status %7d\n", osqp_status ); */
        exit(osqp_status);
    }

    /*  write the solution to a file */

    solution = fopen ("OSQP.sol", "w") ;
    if ( solution != NULL )
    {
      fprintf( solution, " Problem: %-12s\n\n", pname );
      i = work->info->status_val;
      fprintf( solution, " OSQP output status %d = %-32s\n",
               i, work->info->status );
      fprintf( solution, " optimal value = %15.8E\n", work->info->obj_val);

    /* unravel the dual variables from the osqp multipliers */

      ncon_total = CUTEst_ncon - 1;
      for (i = 0; i < CUTEst_nvar; i++) {
        if ( ( xl[i] > - OSQP_INFTY ) || ( xu[i]  < OSQP_INFTY ) ) {
          ncon_total++ ;
          x0[i] = work->solution->y[ncon_total];
        }
      }

      /* print the solution values */

      fprintf( solution, "\n Solution:\n\n");
      fprintf( solution, "      i name            x_l          x           x_u"
              "          dual\n" );
      for (i = 0 ; i < data->n ; i++) {
        fprintf( solution, "%7d %10s %12.5E %12.5E %12.5E %12.5E\n",
           i, Xnames[i], xl[i], work->solution->x[i], xu[i], x0[i]);
      }

      if (constrained) {

      /* compute the residual A x */

        MALLOC( c, data->m, rp_);
        for (i = 0 ; i < data->m ; i++) c[i]=0.0;
        for (j = 0 ; j < data->n ; j++) {
          for (i = osqp_A_p[j] ; i < osqp_A_p[j+1] ; i++) {
            c[osqp_A_i[i]] = c[osqp_A_i[i]]
              + osqp_A_x[i] * work->solution->x[j];
          }
        }

        /* print the constraint values */

        fprintf( solution, "\n Constraints:\n\n");
        fprintf( solution, "      i name            c_l          c          "
                " c_u       multiplier\n" );
        for (i = 0 ; i < CUTEst_ncon ; i++) {
          fprintf( solution, "%7d %10s %12.5E %12.5E %12.5E %12.5E\n",
             i, Cnames[i], osqp_l[i], c[i], osqp_u[i], work->solution->y[i]);
        }
      FREE(c);
      }
      fclose(solution);
    }

    /*  write a summary of the results to a file */

    if ( results != NULL )
    {
      i = work->info->iter;
      j = work->info->status_val;
      fprintf( results, " %15.8E %7.1E %7.1E %6d %8.2F %2d\n",
               work->info->obj_val, work->info->pri_res,
               work->info->dua_res, i, work->info->solve_time, j );
      fclose(results);
    }

    FREE(x0); FREE(xl); FREE(xu);
    if (constrained) {
      FREE(cl);
      FREE(cu);
    }

 /* Get CUTEst statistics */

    CUTEST_creport( &status, calls, cpu);
    if (status) {
      printf("CUTEst error.\nAborting.\n");
      exit(2);
    }

    printf(" ************************ CUTEst statistics");
    printf(" ************************\n");
    printf(" Package used             : OSQP\n");
    printf(" Problem                  : %-s\n", pname);
    printf(" # variables              = %-10d\n", (int)CUTEst_nvar);
    printf(" # constraints            = %-10d\n", (int)CUTEst_ncon);
    printf(" # linear constraints     = %-10d\n", vtypes.nlin);
    printf(" # equality constraints   = %-10d\n", vtypes.neq);
    printf(" # inequality constraints = %-10d\n", vtypes.nineq);
    printf(" # bound constraints      = %-10d\n", vtypes.nbnds);
    /*
    printf(" # objective functions    = %-15.7g\n", calls[0]);
    printf(" # objective gradients    = %-15.7g\n", calls[1]);
    printf(" # objective Hessians     = %-15.7g\n", calls[2]);
    printf(" # Hessian-vector prdct   = %-15.7g\n", calls[3]);
    printf(" # constraints functions  = %-15.7g\n", calls[4]);
    printf(" # constraints gradients  = %-15.7g\n", calls[5]);
    printf(" # constraints Hessians   = %-15.7g\n", calls[6]);
    */
    i = work->info->status_val;
    printf(" Exit code                = %d == %-32s\n", i, work->info->status);
    printf(" Final f                  = %-15.7g\n", work->info->obj_val);
    printf(" Set up time              = %-7.5f seconds\n", cpu[0]);
    printf(" Solve time               = %-7.5f seconds\n", cpu[1]);
    printf(" *********************************************");
    printf("*********************\n");

     ierr = 0;
     FORTRAN_close(&funit, &ierr);
     if (ierr)
       printf("Error closing %s on unit %d.\n", fname, (int)funit);

 /* Free workspace */

     FREE(pname);

 /* Free memory for variable names */

   for (i = 0; i < CUTEst_nvar; i++) FREE(Xnames[i]);
      FREE(Xnames);

 /* Free memory for constraint names */

    for (i = 0; i < CUTEst_ncon; i++) FREE(Cnames[i]);
    if (constrained) FREE(Cnames);

    /* free CUTEst workspace */

    if (constrained) {
      CUTEST_cterminate( &status );
    } else {
      CUTEST_uterminate( &status );
    }

/* Cleanup */

    osqp_cleanup(work);
    c_free(data->A);
    c_free(data->P);
    c_free(data);
    c_free(settings);
    FREE( osqp_q ); FREE( osqp_l ); FREE( osqp_u );
    FREE( osqp_P_x ); FREE( osqp_P_i ); FREE( osqp_P_p );
    FREE( osqp_A_x ); FREE( osqp_A_i ); FREE( osqp_A_p );

    return 0;
}

/* convert an m by n matrix input in co-ordinate form (row,col,val)[i]
   for i=0:nnz-1 to compact sparse column format(row,val)[j] for
   j=ptr[i]:ptr[i+1]-1 and i=0:n-1 */

void coo2csc(integer n, integer nz, rp_ *coo_val, integer *coo_row,
             integer *coo_col, c_float *csr_val, c_int *csr_row,
             c_int *col_start)
{
  integer i, l;

  for (i=0; i<=n; i++) col_start[i] = 0;

  /* determine column lengths */

  for (i=0; i<nz; i++) col_start[coo_col[i]+1]++;

  /* set the starting address for each column */

  for (i=0; i<n; i++) col_start[i+1] += col_start[i];

  /* pass through the structure once more, filling in the csc matrix. */

  for (l=0; l<nz; l++){
    i = col_start[coo_col[l]];
    csr_val[i] = coo_val[l];
    csr_row[i] = coo_row[l];
    col_start[coo_col[l]]++;
  }

  /* shift back column starting addresses */

  for (i=n; i>0; i--) col_start[i] = col_start[i-1];
  col_start[0] = 0;

  /* sort each column by increasing row index */

  for (i=0; i<n; i++){
    sort (csr_row, csr_val, col_start[i], col_start[i+1]);
  }
}

/* Sort the components of the vectors (val,ind) so that the entries of
   ind between start and end-1 are in increasing order. N.B. this is
   not as efficient as a proper sort (e.g. quicksort), but should suffice
   when start and end are close */

void sort(c_int *ind, c_float *val, c_int start, c_int end)
{
  c_int i, j, i_temp;
  c_float d_temp;

  for (i=end-1; i>start; i--)
    for(j=start; j<i; j++)
      if (ind[j] > ind[j+1]){
	i_temp=ind[j];
	ind[j]=ind[j+1];
	ind[j+1]=i_temp;
        d_temp=val[j];
        val[j]=val[j+1];
        val[j+1]=d_temp;
      }
}

/* obtain information about the problem */

void getinfo( integer n, integer m, rp_ *xl, rp_ *xu,
              rp_ *cl, rp_ *cu, logical *equatn,
              logical *linear, VarTypes *vartypes ) {

  int i;

  vartypes->nlin = 0; vartypes->neq = 0; vartypes->nbnds = 0;
  vartypes->nrange = 0;
  vartypes->nlower = 0; vartypes->nupper = 0; vartypes->nineq = 0;
  vartypes->nineq_lin = 0; vartypes->nineq_nlin = 0;
  vartypes->neq_lin = 0; vartypes->neq_nlin = 0;

  for( i = 0; i < n; i++ )
      if( xl[i] > -CUTE_INF || xu[i] < CUTE_INF ) vartypes->nbnds++;
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
