
/* ================================================
 * CUTEst interface to KNITRO 7           
 *
 * D. Orban, April 14, 2011
 * CUTEst evoluation, Nick Gould, January 15th 2013
 *
 * ================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define KNITRO_main

#ifdef __cplusplus
extern "C" {   /* To prevent C++ compilers from mangling symbols */
#endif

#include "cutest.h"
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
  doublereal *CUTEst_Jac, *CUTEst_Hess, *Hv, f;

  /* ======================================================================== */
  /* Callback function to evaluate the objective function and constraints */
  /* ======================================================================== */

  int  callbackEvalFC (const int             evalRequestCode,
                       const int             n,
                       const int             m,
                       const int             nnzJ,
                       const int             nnzH,
                       const double * const  x,
                       const double * const  lambda,
                       double * const  obj,
                       double * const  c,
                       double * const  objGrad,
                       double * const  jac,
                       double * const  hessian,
                       double * const  hessVector,
                       void   *        userParams) {

    int i;
    integer status;

    if (evalRequestCode != KTR_RC_EVALFC) {
      fprintf (stderr, "*** callbackEvalFC incorrectly called with code %d\n",
               evalRequestCode);
      return -1;
    }

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

  int  callbackEvalGA (const int             evalRequestCode,
                       const int             n,
                       const int             m,
                       const int             nnzJ,
                       const int             nnzH,
                       const double * const  x,
                       const double * const  lambda,
                       double * const  obj,
                       double * const  c,
                       double * const  objGrad,
                       double * const  jac,
                       double * const  hessian,
                       double * const  hessVector,
                       void   *        userParams) {

    int i;
    integer status;

    if (evalRequestCode != KTR_RC_EVALGA) {
      fprintf (stderr, "*** callbackEvalGA incorrectly called with code %d\n",
               evalRequestCode);
      return -1;
    }

    if (CUTEst_ncon > 0) {

      CUTEST_csgr( &status,&CUTEst_nvar, &CUTEst_ncon, x, lambda, 
                   &somethingFalse, &CUTEst_nnzj, &CUTEst_lcjac, 
                   CUTEst_Jac, jacIndexVars, jacIndexCons);
      for (i = 0; i < CUTEst_nvar; i++) objGrad[i] = 0.0;
      for (i = 0; i < CUTEst_nnzj; i++)
        if (jacIndexCons[i] == 0)
          objGrad[ (int)(jacIndexVars[i])-1] = CUTEst_Jac[i];
        else
          jac[i] = CUTEst_Jac[i];
      /*CCFSG(&CUTEst_nvar, &CUTEst_ncon, x, &CUTEst_ncon, c, &CUTEst_nnzj, */
      /*      &CUTEst_lcjac, jac, jacIndexVars, jacIndexCons, &somethingTrue); */
      /*COFG(&CUTEst_nvar, x, &f, objGrad, &somethingTrue); */

    } else
      CUTEST_ugr( &status,&CUTEst_nvar, x, objGrad);

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

  int  callbackEvalHess (const int             evalRequestCode,
                         const int             n,
                         const int             m,
                         const int             nnzJ,
                         const int             nnzH,
                         const double * const  x,
                         const double * const  lambda,
                         double * const  obj,
                         double * const  c,
                         double * const  objGrad,
                         double * const  jac,
                         double * const  hessian,
                         double * const  hessVector,
                         void   *        userParams) {

    int i;
    integer status;

    if (evalRequestCode == KTR_RC_EVALH) {

      if (CUTEst_ncon > 0) {
        CUTEST_csh( &status,&CUTEst_nvar, &CUTEst_ncon, x, lambda, &CUTEst_nnzh,
             &CUTEst_nnzh, hessian, hessIndexRows, hessIndexCols);
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

    } else if (evalRequestCode == KTR_RC_EVALHV) {

      if (! Hv) MALLOC(Hv, CUTEst_nvar, doublereal);

      if (CUTEst_ncon > 0)
        CUTEST_chprod( &status,&CUTEst_nvar, &CUTEst_ncon, &somethingTrue, x, 
                       lambda, hessVector, Hv);
      else
        CUTEST_uhprod( &status,&CUTEst_nvar, &somethingTrue, x, hessVector, Hv);

      if( status ) {
         printf("** CUTEst error, status = %d, aborting\n", status);
         exit(status);
      }

      for (i = 0; i < CUTEst_nvar; i++) hessVector[i] = Hv[i];

    } else {
      fprintf(stderr, "*** callbackEvalHess incorrectly called with code %d\n",
              evalRequestCode);
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

    KTR_context *KnitroData;
    int *cTypes;
    char      szVersion[15 + 1];

    VarTypes vtypes;

    int *cType;
    integer ncon_dummy;
    doublereal *x, *bl, *bu, *dummy1, *dummy2;
    doublereal *v = NULL, *cl = NULL, *cu = NULL;
    logical *equatn = NULL, *linear = NULL;
    char *pname, *vnames, *gnames;
    integer e_order = 1, l_order = 1, v_order = 0;
    logical grad;
    logical constrained = FALSE_;

    doublereal *c, f;

    doublereal calls[7], cpu[2];
    integer nlin = 0, nbnds = 0, neq = 0;
    doublereal dummy;
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
    MALLOC(x,      CUTEst_nvar, doublereal);
    MALLOC(bl,     CUTEst_nvar, doublereal);
    MALLOC(bu,     CUTEst_nvar, doublereal);
    MALLOC(cType,  CUTEst_ncon, integer);
    if (constrained) {
      MALLOC(equatn, CUTEst_ncon+1,            logical   );
      MALLOC(linear, CUTEst_ncon+1,            logical   );
      MALLOC(v,      CUTEst_ncon+CUTEst_nvar+1, doublereal);
      MALLOC(cl,     CUTEst_ncon+1,            doublereal);
      MALLOC(cu,     CUTEst_ncon+1,            doublereal);
      CUTEST_csetup( &status, &funit, &iout, &io_buffer, 
                     &CUTEst_nvar, &CUTEst_ncon, x, bl, bu,
                     v, cl, cu, equatn, linear,
                     &e_order, &l_order, &v_order);
    } else {
      MALLOC(equatn, 1,            logical   );
      MALLOC(linear, 1,            logical   );
      MALLOC(cl,     1,            doublereal);
      MALLOC(cu,     1,            doublereal);
      MALLOC(v,      CUTEst_nvar+1, doublereal);
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
    if (constrained) {
      MALLOC(gnames, CUTEst_ncon*FSTRING_LEN, char);
      CUTEST_cnames( &status, &CUTEst_nvar, &CUTEst_ncon, 
                     pname, vnames, gnames);
      FREE(gnames);
    } else
      CUTEST_unames( &status, &CUTEst_nvar, pname, vnames);

    if( status ) {
       printf("** CUTEst error, status = %d, aborting\n", status);
       exit(status);
    }

    FREE(vnames);

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
    KTR_get_release(15, szVersion);
    KnitroData = KTR_new();
    if (KnitroData == NULL) {
      fprintf(stderr, "Failed to find a valid KNITRO license.\n");
      fprintf(stderr, "%s\n", szVersion);
      exit(1);
    }

    /* Read spec file */
    if (KTR_load_param_file(KnitroData, "knitro.opt")) {
      fprintf(stderr, "Cannot open spec file knitro.opt...");
      fprintf(stderr, " using all default options\n");
    }
    if (KTR_get_int_param_by_name(KnitroData, "hessopt", &nHessOpt))
      exit(-1);

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
      MALLOC(CUTEst_Jac,    CUTEst_lcjac + 1, doublereal);

      MALLOC(c, CUTEst_ncon, doublereal);

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

      /* Convert to 0-based indexing */
      for (i = 0; i < CUTEst_nnzj; i++) {
        jacIndexVars[i] = (int)(jacIndexVars[i] - 1);
        jacIndexCons[i] = (int)(jacIndexCons[i] - 1);
        /*   printf("jacIndexVars[%d]=%d\n", i, jacIndexVars[i]); */
        /* printf("jacIndexCons[%d]=%d\n", i, jacIndexCons[i]); */
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
    MALLOC(CUTEst_Hess,    CUTEst_nnzh, doublereal);

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

    /* Register callback functions */
#ifdef KNIT_DEBUG
    fprintf(stderr, "Registering callback functions...\n");
#endif
    if (KTR_set_func_callback(KnitroData, &callbackEvalFC)) {
      fprintf(stderr, "Could not register FC callback function\n");
      goto terminate;
    }
    if (KTR_set_grad_callback(KnitroData, &callbackEvalGA)) {
      fprintf(stderr, "Could not register FC callback function\n");
      goto terminate;
    }
    if ((nHessOpt == KTR_HESSOPT_EXACT) || (nHessOpt == KTR_HESSOPT_PRODUCT))
      if (KTR_set_hess_callback(KnitroData, &callbackEvalHess)) {
        fprintf(stderr, "Could not register Hess callback function\n");
        goto terminate;
      }

    /* Convert infinite bounds. */
    for (i = 0; i < CUTEst_nvar; i++) {
        if (bl[i] == -CUTE_INF) bl[i] = -KTR_INFBOUND;
        if (bu[i] ==  CUTE_INF) bu[i] =  KTR_INFBOUND;
    }
    for (i = 0; i < CUTEst_ncon; i++) {
        if (cl[i] == -CUTE_INF) cl[i] = -KTR_INFBOUND;
        if (cl[i] ==  CUTE_INF) cu[i] =  KTR_INFBOUND;
    }

    MALLOC(cTypes, CUTEst_ncon, int);
    for (i = 0; i < CUTEst_ncon; i++)
        if (linear[i])
          cTypes[i] = KTR_CONTYPE_LINEAR;
        else
          cTypes[i] = KTR_CONTYPE_GENERAL;

#ifdef KNIT_DEBUG
    fprintf(stderr, "Initializing KNITRO data structure...\n");
#endif

    ExitCode = KTR_init_problem(KnitroData,
                                 (int)CUTEst_nvar,
                                 KTR_OBJGOAL_MINIMIZE,
                                 KTR_OBJTYPE_GENERAL,
                                 bl, bu,
                                 (int)CUTEst_ncon,
                                 cTypes,
                                 cl, cu,
                                 (int)CUTEst_nnzj,
                                 (int *)jacIndexVars,
                                 (int *)jacIndexCons,
                                 (int)CUTEst_nnzh,
                                 (int *)hessIndexRows,
                                 (int *)hessIndexCols,
                                 x,
                                 NULL);

#ifdef KNIT_DEBUG
    fprintf(stderr, "Exit code from initialization: %d\n", ExitCode);
#endif

    /* Call the optimizer */
    ExitCode = KTR_solve(KnitroData, x, v, 0, &f,
                          NULL, NULL, NULL, NULL, NULL, NULL);

    /* Release KNITRO context */
#ifdef KNIT_DEBUG
    fprintf(stderr, "Terminating...\n");
#endif
    if (KTR_free(&KnitroData))
      fprintf(stderr, "Knitro-CUTEst:: Error freeing Knitro data structure\n");

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
    FREE(cTypes);

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

