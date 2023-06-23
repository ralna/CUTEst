/* ==========================================================
 * CUTEst interface to KNITRO 7           
 *
 * D. Orban, April 14, 2011
 * CUTEst evoluation, Nick Gould, January 15th 2013
 * update for KNITRO 13, Christoph Hansknecht, June 23rd 2023
 *
 * ==========================================================
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
