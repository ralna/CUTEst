/* THIS VERSION: CUTEST 2.3 - 2024-10-11 AT 09:00 GMT */

/* ================================================
 * CUTEst interface to KNITRO 7           
 *
 * D. Orban, April 14, 2011
 * CUTEst evoluation, Nick Gould, January 15th 2013
 * update for KNITRO 13, Christoph Hansknecht, June 23rd 2023
 *
 * ================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define SZ_VERSION_LEN 128

#define CHECK_KNITRO_EXIT_CODE(command, message)                    \
  do                                                                \
  {                                                                 \
    const int ExitCode = (command);                                 \
    if(ExitCode)                                                    \
    {                                                               \
      fprintf(stderr, "Could not register FC callback function\n"); \
      goto terminate;                                               \
    }                                                               \
  } while(0)

#define KNITRO_main

#ifdef __cplusplus
extern "C" {   /* To prevent C++ compilers from mangling symbols */
#endif

#include "cutest.h"
#include "cutest_routines.h"
#include "knitro.h"

  logical somethingTrue = TRUE_;
  logical somethingFalse = FALSE_;
  integer CUTEst_nvar;        /* number of variables */
  integer CUTEst_ncon;        /* number of constraints */
  integer CUTEst_lcjac;       /* length of Jacobian arrays */
  integer CUTEst_nnzj;        /* number of nonzeros in Jacobian */
  integer CUTEst_nnzh;        /* number of nonzeros in upper triangular
                                part of the Hessian of the Lagrangian */
  integer *jacIndexVars, *jacIndexCons, *hessIndexRows, *hessIndexCols;
  rp_ *CUTEst_Jac, *CUTEst_Hess, *Hv, f;

  /* ======================================================================== */
  /* Callback function to evaluate the objective function and constraints */
  /* ======================================================================== */

  int  callbackEvalFC (KN_context_ptr            kc,
                       CB_context_ptr            cb,
                       KN_eval_request_ptr const evalRequest,
                       KN_eval_result_ptr const  evalResult,
                       void* const               userParams) {

    int i;
    integer status;

    if (evalRequest->type != KN_RC_EVALFC) {
      fprintf (stderr, "*** callbackEvalFC incorrectly called with code %d\n",
               evalRequest->type);
      return -1;
    }

    const double* x = evalRequest->x;
    double* obj = evalResult->obj;
    double* c = evalResult->c;

    if (CUTEst_ncon > 0) {
      CUTEST_cfn( &status, &CUTEst_nvar, &CUTEst_ncon, x, obj, c);
      /*for (i = 0; i < CUTEst_nvar; i++) */
      /*    printf(" x = %5d  %22.15e\n", i, x[i]); */
      /*printf(" f = %22.15e\n", obj[0]); */
      /*for (i = 0; i < CUTEst_ncon; i++) */
      /*    printf(" c = %5d  %22.15e\n", i, c[i]); */
    } else {
      CUTEST_ufn( &status, &CUTEst_nvar, x, obj);
    }

    if( status ) {
       printf("** CUTEst error, status = %d, aborting\n", status);
       exit(status);
    }
    return 0;
  }

  /* ======================================================================== */
  /* Callback function to evaluate the objective gradient and Jacobian */
  /* ======================================================================== */

  int  callbackEvalGA (KN_context_ptr            kc,
                       CB_context_ptr            cb,
                       KN_eval_request_ptr const evalRequest,
                       KN_eval_result_ptr const  evalResult,
                       void* const               userParams) {

    int i;
    integer status;

    if (evalRequest->type != KN_RC_EVALGA) {
      fprintf (stderr, "*** callbackEvalGA incorrectly called with code %d\n",
               evalRequest->type);
      return -1;
    }

    const double* x = evalRequest->x;
    const double* lambda = evalRequest->lambda;

    double* objGrad = evalResult->objGrad;
    double* jac = evalResult->jac;

    if (CUTEst_ncon > 0) {

      CUTEST_csgr( &status,&CUTEst_nvar, &CUTEst_ncon, x, lambda, 
                     &somethingFalse, &CUTEst_nnzj, &CUTEst_lcjac, 
                     CUTEst_Jac, jacIndexVars, jacIndexCons);

      for (i = 0; i < CUTEst_nvar; i++) {
        objGrad[i] = 0.0;
      }

      int offset = 0;

      for (i = 0; i < CUTEst_nnzj; i++) {
        if (jacIndexCons[i] == 0) {
          objGrad[ (int)(jacIndexVars[i])-1] = CUTEst_Jac[i];
          ++offset;
        }
        else {
          jac[i - offset] = CUTEst_Jac[i];
        }
      }
      /*CCFSG(&CUTEst_nvar, &CUTEst_ncon, x, &CUTEst_ncon, c, &CUTEst_nnzj, */
      /*      &CUTEst_lcjac, jac, jacIndexVars, jacIndexCons, &somethingTrue); */
      /*COFG(&CUTEst_nvar, x, &f, objGrad, &somethingTrue); */

    } else {
      CUTEST_ugr( &status, &CUTEst_nvar, x, objGrad);
    }

    if( status ) {
       printf("** CUTEst error, status = %d, aborting\n", status);
       exit(status);
    }
    /* for (i = 0; i < CUTEst_nvar; i++) */
    /*  printf(" g = %22.15e\n", objGrad[i]); */

    return 0;
  }

  /* ======================================================================== */
  /* Callback function to evaluate the Hessian or Hessian-vector product */
  /* ======================================================================== */

  int  callbackEvalHess (KN_context_ptr            kc,
                         CB_context_ptr            cb,
                         KN_eval_request_ptr const evalRequest,
                         KN_eval_result_ptr const  evalResult,
                         void* const               userParams) {

    int i;
    integer status;

    const double* x = evalRequest->x;
    const double* lambda = evalRequest->lambda;

    if (evalRequest->type == KN_RC_EVALH) {

      double* hessian = evalResult->hess;

      if (CUTEst_ncon > 0) {
        CUTEST_csh( &status,&CUTEst_nvar, &CUTEst_ncon, x, lambda, 
                      &CUTEst_nnzh, &CUTEst_nnzh, hessian, hessIndexRows, 
                      hessIndexCols);
      } else {
        CUTEST_ush( &status,&CUTEst_nvar, x, &CUTEst_nnzh,
                      &CUTEst_nnzh, hessian, hessIndexRows, hessIndexCols);
      }

      if( status ) {
         printf("** CUTEst error, status = %d, aborting\n", status);
         exit(status);
      }

      /* Adjust indices to be zero-based. */
      for (i = 0; i < CUTEst_nnzh; i++) {
          hessIndexRows[i] = (int)(hessIndexRows[i] - 1);
          hessIndexCols[i] = (int)(hessIndexCols[i] - 1);
          /*printf(" H = %5d  %5d  %22.15e\n", hessIndexRows[i], */
          /*        hessIndexCols[i], hessian[i]); */

      }

    } else if (evalRequest->type == KN_RC_EVALHV) {

      double* hessVector = evalResult->hessVec;

      if (! Hv) MALLOC(Hv, CUTEst_nvar, rp_);

      if (CUTEst_ncon > 0)
        CUTEST_chprod( &status,&CUTEst_nvar, &CUTEst_ncon, &somethingTrue,
                         x, lambda, hessVector, Hv);
      else
        CUTEST_uhprod( &status,&CUTEst_nvar, &somethingTrue, x, 
                         hessVector, Hv);

      if( status ) {
         printf("** CUTEst error, status = %d, aborting\n", status);
         exit(status);
      }

      for (i = 0; i < CUTEst_nvar; i++) hessVector[i] = Hv[i];

    } else {
      fprintf(stderr, "*** callbackEvalHess incorrectly called with code %d\n",
              evalRequest->type);
      return -1;
    }

    return 0;
  }

  /* ======================================================================== */
  /* Main function */
  /* ======================================================================== */

  int MAINENTRY(void) {

    char *fname = "OUTSDIF.d"; /* CUTEst data file */
    integer funit = 42;        /* FORTRAN unit number for OUTSDIF.d */
    integer iout = 6;          /* FORTRAN unit number for error output */
    integer ierr;              /* Exit flag from OPEN and CLOSE */
    integer io_buffer = 11;    /* FORTRAN unit internal input/output */
    integer status;            /* Exit flag from CUTEst tools */

    KN_context *kc;
    CB_context *cb;

    KNINT* consIndex;

    char szVersion[SZ_VERSION_LEN];

    VarTypes vtypes;

    integer ncon_dummy;
    rp_ *x, *bl, *bu, *dummy1, *dummy2;
    rp_ *v = NULL, *cl = NULL, *cu = NULL;
    logical *equatn = NULL, *linear = NULL;
    char *pname, *vnames, *cnames;
    char** all_vnames, **all_cnames;
    integer e_order = 1, l_order = 1, v_order = 0;
    logical grad;
    logical constrained = FALSE_;

    rp_ *c, f;

    rp_ calls[7], cpu[4];
    integer nlin = 0, nbnds = 0, neq = 0;
    rp_ dummy;
    integer ExitCode;
    int nHessOpt, i;

    /* Open problem description file OUTSDIF.d */
    ierr = 0;
    FORTRAN_open(&funit, fname, &ierr);
    if (ierr) {
      printf("Error opening file OUTSDIF.d.\nAborting.\n");
      exit(1);
    }

    /* Determine problem size */
    CUTEST_cdimen( &status, &funit, &CUTEst_nvar, &CUTEst_ncon);
    /*
          printf (" ** the problem has %i constraints\n", 
                      &CUTEst_ncon ) ;
    */
    if ( status ) {
       printf("** CUTEst error, status = %d, aborting\n", status);
       exit(status);
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

    MALLOC(consIndex,  CUTEst_ncon, KNINT);

    if (constrained) {
      MALLOC(equatn, CUTEst_ncon+1,            logical   );
      MALLOC(linear, CUTEst_ncon+1,            logical   );
      MALLOC(v,      CUTEst_ncon+CUTEst_nvar+1, rp_);
      MALLOC(cl,     CUTEst_ncon+1,            rp_);
      MALLOC(cu,     CUTEst_ncon+1,            rp_);
      CUTEST_csetup( &status, &funit, &iout, &io_buffer, 
                       &CUTEst_nvar, &CUTEst_ncon, x, bl, bu,
                       v, cl, cu, equatn, linear,
                       &e_order, &l_order, &v_order);
    } else {
      MALLOC(equatn, 1,            logical   );
      MALLOC(linear, 1,            logical   );
      MALLOC(cl,     1,            rp_);
      MALLOC(cu,     1,            rp_);
      MALLOC(v,      CUTEst_nvar+1, rp_);
      CUTEST_usetup( &status, &funit, &iout, &io_buffer, 
                       &CUTEst_nvar, x, bl, bu);
    }

    if( status ) {
       printf("** CUTEst error, status = %d, aborting\n", status);
       exit(status);
    }

    /* Get problem name */
    MALLOC(pname, FSTRING_LEN+1, char);
    MALLOC(vnames, CUTEst_nvar*FSTRING_LEN, char);
    MALLOC(all_vnames, CUTEst_nvar, char*);

    if (constrained) {
      MALLOC(cnames, CUTEst_ncon*FSTRING_LEN, char);
      MALLOC(all_cnames, CUTEst_ncon*FSTRING_LEN, char*);

      CUTEST_cnames( &status, &CUTEst_nvar, &CUTEst_ncon, 
                       pname, vnames, cnames);

      for(i = 0; i < CUTEst_ncon;++i)
      {
        all_cnames[i] = cnames + (i*FSTRING_LEN);
      }

    } else
      CUTEST_unames( &status, &CUTEst_nvar, pname, vnames);

    for(i = 0; i < CUTEst_nvar;++i)
    {
      all_vnames[i] = vnames + (i*FSTRING_LEN);
    }

    if( status ) {
       printf("** CUTEst error, status = %d, aborting\n", status);
       exit(status);
    }

    /* Make sure to null-terminate problem name */
    pname[FSTRING_LEN] = '\0';
    i = FSTRING_LEN - 1;
    while(i-- > 0 && pname[i] == ' ') {
      pname[i] = '\0';
    }

    /* Obtain basic info on problem */
    /*
    if (constrained)
      GETINFO(CUTEst_nvar, CUTEst_ncon, bl, bu, cl, cu, equatn, linear, &vtypes);
    else {
      equatn[0] = FALSE_;
      linear[0] = FALSE_;
      GETINFO(CUTEst_nvar, 1, bl, bu, cl, cu, equatn, linear, &vtypes);
    }
    */

    /* Initialize KNITRO context */
    KN_get_release(SZ_VERSION_LEN, szVersion);

    ExitCode = KN_new(&kc);

    if (kc == NULL) {
      fprintf(stderr, "Failed to find a valid KNITRO license.\n");
      fprintf(stderr, "%s\n", szVersion);
      exit(1);
    }

    /* Read spec file */
    if (KN_load_param_file(kc, "knitro.opt")) {
      fprintf(stderr, "Cannot open spec file knitro.opt...");
      fprintf(stderr, " using all default options\n");
    }

    if (KN_get_int_param_by_name(kc, "hessopt", &nHessOpt)) {
      exit(-1);
    }

    /* Obtain Jacobian sparsity pattern and initial objective value */
#ifdef KNIT_DEBUG
    fprintf(stderr, "Obtaining Jacobian sparsity pattern...\n");
#endif

    if (constrained) {

      /* Constrained problem. */

      CUTEST_cdimsj( &status, &CUTEst_lcjac);

      if( status ) {
         printf("** CUTEst error, status = %d, aborting\n", status);
         exit(status);
      }
      MALLOC(jacIndexVars, CUTEst_lcjac + 1, integer   );
      MALLOC(jacIndexCons, CUTEst_lcjac + 1, integer   );
      MALLOC(CUTEst_Jac,    CUTEst_lcjac + 1, rp_);

      MALLOC(c, CUTEst_ncon, rp_);

      CUTEST_ccfsg( &status,&CUTEst_nvar, &CUTEst_ncon, x, c, &CUTEst_nnzj,
                      &CUTEst_lcjac, CUTEst_Jac, jacIndexVars, 
                      jacIndexCons, &somethingTrue);
      if( status ) {
         printf("** CUTEst error, status = %d, aborting\n", status);
         exit(status);
      }

      CUTEST_cfn( &status,&CUTEst_nvar, &CUTEst_ncon, x, &f, c);

      if( status ) {
         printf("** CUTEst error, status = %d, aborting\n", status);
         exit(status);
      }

      FREE(c);

      for(i = 0; i < CUTEst_ncon; i++) {
        consIndex[i] = (KNINT) i;
      }
    } else {

      /* Unconstrained problem. */

      jacIndexVars = NULL;
      jacIndexCons = NULL;
      CUTEst_Jac = NULL;
      CUTEST_ufn( &status,&CUTEst_nvar, x, &f);
    }

    /* Obtain Hessian sparsity pattern */
#ifdef KNIT_DEBUG
    fprintf(stderr, "Obtaining Hessian sparsity pattern...\n");
#endif
    CUTEST_cdimsh( &status, &CUTEst_nnzh);

    if( status ) {
       printf("** CUTEst error, status = %d, aborting\n", status);
       exit(status);
    }

    MALLOC(hessIndexRows, CUTEst_nnzh, integer   );
    MALLOC(hessIndexCols, CUTEst_nnzh, integer   );
    MALLOC(CUTEst_Hess,    CUTEst_nnzh, rp_);

    CUTEST_csh( &status, &CUTEst_nvar, &CUTEst_ncon, x, v, &CUTEst_nnzh, 
                  &CUTEst_nnzh, CUTEst_Hess, hessIndexRows, hessIndexCols);

    if( status ) {
       printf("** CUTEst error, status = %d, aborting\n", status);
       exit(status);
    }

    /* Convert to 0-based indexing */
    for (i = 0; i < CUTEst_nnzh; i++) {
      hessIndexRows[i] = (int)(hessIndexRows[i] - 1);
      hessIndexCols[i] = (int)(hessIndexCols[i] - 1);
      /* printf("hessIndexRows[%d]=%d\n", i, hessIndexRows[i]); */
      /* printf("hessIndexCols[%d]=%d\n", i, hessIndexCols[i]); */
    }

    int consJac_nnz = 0;

    if(constrained)
    {
      int offset = 0;

      for (i = 0; i < CUTEst_nnzj; i++) {

        /* objective */
        if ( jacIndexCons[i] == 0 )
        {
          ++offset;
        }
        else
        {
          jacIndexVars[i - offset] = (int)(jacIndexVars[i] - 1);
          jacIndexCons[i - offset] = (int)(jacIndexCons[i] - 1);
          ++consJac_nnz;

          /* printf("jacIndexVars[%d]=%d\n", i - offset, jacIndexVars[i - offset]); */
          /* printf("jacIndexCons[%d]=%d\n", i - offset, jacIndexCons[i - offset]); */
        }
      }

    }

    /* Convert infinite bounds. */
    for (i = 0; i < CUTEst_nvar; i++) {
      if (bl[i] == -CUTE_INF) bl[i] = -KN_INFINITY;
      if (bu[i] ==  CUTE_INF) bu[i] =  KN_INFINITY;
    }
    for (i = 0; i < CUTEst_ncon; i++) {
      if (cl[i] == -CUTE_INF) cl[i] = -KN_INFINITY;
      if (cl[i] ==  CUTE_INF) cu[i] =  KN_INFINITY;
    }

    CHECK_KNITRO_EXIT_CODE(KN_add_vars(kc, CUTEst_nvar, NULL),
                           "Failed to add variables\n");

    CHECK_KNITRO_EXIT_CODE(KN_add_cons(kc, CUTEst_ncon, NULL),
                           "Failed to add constraints\n");

    CHECK_KNITRO_EXIT_CODE(KN_set_var_lobnds_all(kc, bl),
                           "Failed to set lower variable bounds\n");
    CHECK_KNITRO_EXIT_CODE(KN_set_var_upbnds_all(kc, bu),
                           "Failed to set upper variable bounds\n");

    CHECK_KNITRO_EXIT_CODE(KN_set_con_lobnds_all(kc, cl),
                           "Failed to set lower constraint bounds\n");
    CHECK_KNITRO_EXIT_CODE(KN_set_con_upbnds_all(kc, cu),
                           "Failed to set upper constraint bounds\n");

    CHECK_KNITRO_EXIT_CODE(KN_set_var_primal_init_values_all(kc, x),
                           "Failed to set primal initial values\n");

    CHECK_KNITRO_EXIT_CODE(KN_set_var_names_all(kc, all_vnames),
                           "Failed to set variable names");

    FREE(all_vnames);
    FREE(vnames);

    if (constrained)
    {
      CHECK_KNITRO_EXIT_CODE(KN_set_con_names_all(kc, all_cnames),
                             "Failed to set constraint names");

      FREE(all_cnames);
      FREE(cnames);
    }

    /* Register callback functions */
#ifdef KNIT_DEBUG
    fprintf(stderr, "Registering callback functions...\n");
#endif

    CHECK_KNITRO_EXIT_CODE(KN_add_eval_callback(kc, KNTRUE, CUTEst_ncon, consIndex, &callbackEvalFC, &cb),
                           "Could not register FC callback function\n");

    CHECK_KNITRO_EXIT_CODE(KN_set_cb_grad(kc, cb, KN_DENSE, NULL, consJac_nnz, jacIndexCons, jacIndexVars, &callbackEvalGA),
                           "Could not register FC callback function\n");

    if ((nHessOpt == KN_HESSOPT_EXACT) || (nHessOpt == KN_HESSOPT_PRODUCT)) {
      CHECK_KNITRO_EXIT_CODE(KN_set_cb_hess(kc, cb, CUTEst_nnzh, hessIndexRows, hessIndexCols, &callbackEvalHess),
                             "Could not register FC callback function\n");
    }

#ifdef KNIT_DEBUG
    fprintf(stderr, "Initializing KNITRO data structure...\n");
#endif

#ifdef KNIT_DEBUG
    fprintf(stderr, "Exit code from initialization: %d\n", ExitCode);
#endif

    if(!kc || ExitCode)
    {
      fprintf(stderr, "Knitro-CUTEst:: Error creating Knitro data structure\n");
      exit(1);
    }

    printf("Knitro-CUTEst:: Starting solve\n");

    /* Call the optimizer */
    ExitCode = KN_solve(kc);

    /* Release KNITRO context */
#ifdef KNIT_DEBUG
    fprintf(stderr, "Terminating...\n");
#endif
    if (KN_free(&kc)) {
      fprintf(stderr, "Knitro-CUTEst:: Error freeing Knitro data structure\n");
    }

    /* Get CUTEst statistics */
    CUTEST_creport( &status, calls, cpu);

    if( status ) {
       printf("** CUTEst error, status = %d, aborting\n", status);
       exit(status);
    }

    printf("\n\n ************************ CUTEst statistics ************************\n\n");
    printf(" Code used               : %-15s\n", szVersion);
    printf(" Problem                 : %-s\n", pname);
    printf(" # variables             = %-10d\n", CUTEst_nvar);
    printf(" # constraints           = %-10d\n", CUTEst_ncon);
    /*
    printf(" # linear constraints    = %-10d\n", vtypes.nlin);
    printf(" # equality constraints  = %-10d\n", vtypes.neq);
    printf(" # inequality constraints= %-10d\n", vtypes.nineq);
    printf(" # bound constraints     = %-10d\n", vtypes.nbnds);
    */
    printf(" # objective functions   = %-15.7g\n", calls[0]);
    printf(" # objective gradients   = %-15.7g\n", calls[1]);
    printf(" # objective Hessians    = %-15.7g\n", calls[2]);
    printf(" # Hessian-vector prdct  = %-15.7g\n", calls[3]);
    printf(" # constraints functions = %-15.7g\n", calls[4]);
    printf(" # constraints gradients = %-15.7g\n", calls[5]);
    printf(" # constraints Hessians  = %-15.7g\n", calls[6]);
    printf(" Exit code               = %-10d\n", ExitCode);
    printf(" Final f                 = %-15.7g\n", f);
    printf(" Set up time             = %-10.2f seconds\n", cpu[0]);
    printf(" Solve time              = %-10.2f seconds\n", cpu[1]);
    printf(" ******************************************************************\n\n");

  terminate:

    ierr = 0;
    FORTRAN_close(&funit, &ierr);
    if (ierr) {
      fprintf(stderr, "Error closing %s on unit %d.\n", fname, funit);
      fprintf(stderr, "Trying not to abort.\n");
    }

    /* Free workspace */
    FREE(pname);
    FREE(x); FREE(bl); FREE(bu);
    FREE(v); FREE(cl); FREE(cu);
    FREE(equatn);
    FREE(linear);

    if (constrained) {
      FREE(CUTEst_Jac);
      FREE(jacIndexVars);
      FREE(jacIndexCons);
    }
    FREE(hessIndexRows);
    FREE(hessIndexCols);
    FREE(CUTEst_Hess);
    if (Hv) FREE(Hv);

    FREE(consIndex);

    CUTEST_cterminate( &status );

    if( status ) {
       printf("** CUTEst error, status = %d, aborting\n", status);
       exit(status);
    }

    return 0;

  }

#ifdef __cplusplus
}    /* Closing brace for  extern "C"  block */
#endif
