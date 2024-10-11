/* THIS VERSION: CUTEST 2.3 - 2024-10-11 AT 09:00 GMT */

/*
 * A grand unified Octave gateway for the CUTEst tools.
 * This interface brings together the unconstrained, constrained,
 * dense and sparse versions of the CUTEst tools.
 *
 * In order to unify the tools and be able to use the same Octave commands on
 * both constrained and unconstrained problems, the tool names in this
 * interface differ from the those in the old Fortran gateway routine.
 *
 * Tool       CUTEst library function(s)   Purpose
 * --------------------------------------------------------------------------
 * dims       cdimen                      Obtain problem dimensions
 * setup      usetup / csetup             Setup problem data structure
 * obj        uofg / cofg                 Evaluate objective function value
 *                                         and its gradient if requested
 *
 * grad       ugr / cgr / cigr            Evaluate objective function or
 *                                        constraint gradient
 *
 * sobj       cofsg                       Evaluate objective function value
 *                                         and its sparse gradient if requested
 *
 * sgrad      ugr / cisgr                 Evaluate objective function or
 *                                        constraint gradient as a sparse vector
 *
 * objcons    cfn                         Evaluate objective and constraints
 *
 * cons       ccfg / ccifg                Evaluate constraint bodies
 *                                         and their gradients if requested.
 *                                        Evaluate a single constraint value
 *                                         and its gradient if requested
 *
 * scons      ccfsg / ccifsg              Evaluate constraint bodies and
 *                                         Jacobian in sparse format.
 *                                        Evaluate a single constraint value
 *                                         and its gradient as a sparse vector
 *
 * lag        clfg                        Evaluate Lagrangian function value
 *                                         and its gradient if requested
 *
 * lagjac     cgr                         Evaluate Jacobian and gradient of
 *                                         either objective or Lagrangian
 *
 * slagjac    csgr                        Evaluate Jacobian in sparse format
 *                                         and gradient of either objective or
 *                                         Lagrangian as a sparse vector
 *
 * Jprod      cjprod                      Evaluate the matrix-vector product
 *                                         between the Jacobian and a vector
 *
 * Jtprod     cjprod                      Evaluate the matrix-vector product
 *                                         between the transpose Jacobian and
 *                                         a vector
 *
 * hess       udh / cdh                   Evaluate the Hessian matrix of the
 *                                         Lagrangian, or of the objective if
 *                                         the problem is unconstrained
 *
 * ihess      udh / cidh                  Evaluate the Hessian matrix of the
 *                                         i-th problem function (i=0 is the
 *                                         objective function), or of the
 *                                         objective if problem is unconstrained
 *
 * hprod      uprod / cprod               Evaluate the matrix-vector product
 *                                         between the Hessian of the
 *                                         Lagrangian
 *                                         (or the objective if unconstrained)
 *                                         and a vector
 *
 * gradhess    ugrdh / cgrdh              Evaluate the gradient of either the
 *                                         objective or the Lagrangian, the
 *                                         Jacobian (or its transpose) and the
 *                                         Hessian of the Lagrangian in dense
 *                                         format
 *
 * sphess     ush / csh                   Evaluate the Hessian matrix of the
 *                                         Lagrangian, or of the objective if
 *                                         the problem is unconstrained, in
 *                                         sparse format
 *
 * isphess    ush / cish                  Evaluate the Hessian matrix of the
 *                                         i-th problem function (i=0 is the
 *                                         objective function), or of the
 *                                         objective if problem is
 *                                         unconstrained, in sparse format
 *
 * varnames   varnames                    Obtain variable names as strings
 *
 * connames   cnames                      Obtain constraint names as strings
 *
 * terminate  uterminate / cterminate     Remove existing internal workspace
 *
 * CUTEr version:
 *     D. Orban, Montreal, January 2007
 * CUTEst version additions:
 *     Nick Gould, January 2013
 * This version for Octave:
 *     Romana Jezek, 2023
 */

/* -------------------------------------------------------------------------- */
/* Includes */

#include "mex.h"
#include "cutest.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* For versions of the Matlab API prior to 7.3 - 
   depricatd as Matlab no longer has the MX_API_VER preprocessor macro ... duh!
#if (MX_API_VER < 0x07030000)
typedef int mwSize;
typedef int mwIndex;
#endif
*/

#ifndef MWSIZE_MAX
    #define  mwIndex        int
    #define  mwSignedIndex  int
    #define  mwSize         int
#endif

/* Safeguard against C++ symbol mangling */
#ifdef __cplusplus
extern "C" {
#endif

  /* Macro-commands */
  /* ----------------------------------------------------------------------- */
#define isInteger(x) (mxIsInt8((x)) || mxIsUint8((x)) || mxIsInt16((x)) || mxIsUint16((x)) || mxIsInt32((x)) || mxIsUint32((x)) || mxIsInt64((x)) || mxIsUint64((x)))

#define STR_LEN 10

  /* ----------------------------------------------------------------------- */
  /* Persistent data */
  static integer CUTEst_nvar = 0;                    /* number of variables */
  static integer CUTEst_ncon = 0;                  /* number of constraints */
  static integer CUTEst_nnzj = 0;                        /* nnz in Jacobian */
  static integer CUTEst_nnzh = 0;        /* nnz in upper triangular Hessian */
  static integer CUTEst_dertype = 2;     /* derivative type */
  static char setupCalled = 0;     /* Flag to indicate if setup was called */
  static char dataFileOpen = 0;     /* Flag to indicate if OUTSDIf is open */
  static char onlyConst[] = "%-s only available for constrained problems\n";

  /* ------------------------------------------------------------------------*/
  /* Prototypes */

  void mexFunction(int nlhs, mxArray *plhs[],
                   int nrhs, const mxArray *prhs[]);

  mxArray *SparseVector(int n, int nnz, integer *index, double *val);


  mxArray *extractSparseVector(int nrow, int ncol, int nnz, int nnzV,
                               integer *irow, integer *jcol, double *val);

  mxArray *coordToMatlabSparse(int nrow, int ncol, int nnz,
                               integer *irow, integer *jcol, double *val);

  void quicksortFollow(mwIndex x[], double follower[],
                       mwIndex first, mwIndex last);

  void quicksort_cutest(mwIndex numbers[], double values[],
                        mwIndex left, mwIndex right);

  int partition(mwIndex y[], double follower[], mwIndex f, mwIndex l);
  void swap(mwIndex y[], double follower[], mwIndex el1, mwIndex el2);


  /* ----------------------------------------------------------------------- */
  /* Main entry point */
  void mexFunction(int nlhs, mxArray *plhs[],
                    int nrhs, const mxArray *prhs[]) {

    /* ------------------------------------------------------------------ */

    /* Field names for problem data structure */
    const char
      field_n[] = "n",
      field_m[] = "m",
      field_nnzh[] = "nnzh",
      field_nnzj[] = "nnzj",
      field_x[] = "x",
      field_bl[] = "bl",
      field_bu[] = "bu",
      field_v[] = "v",
      field_cl[] = "cl",
      field_cu[] = "cu",
      field_equatn[] = "equatn",
      field_linear[] = "linear",
      field_pbname[] = "name";

    const char *fieldNames[] = { field_n, field_m, field_nnzh, field_nnzj,
                                 field_x, field_bl, field_bu, field_v,
                                 field_cl, field_cu, field_equatn,
                                 field_linear, field_pbname };

    int  nFields = sizeof(fieldNames)/sizeof(fieldNames[0]);

    integer      icon, *icon_ptr, *dertype_ptr;

    integer      zero = 0;
    integer     *irow, *jcol, *irow2, *jcol2;
    integer      nnzgci, nnzjplusn, offdiag_nnzh, nnzhi, nnzh2, lj;

    char *toolName = NULL;
    char  fName[] = "OUTSDIF.d";
    integer  funit = 42;          /* FORTRAN unit number for OUTSDIF.d */
    integer  iout = 6;         /* FORTRAN unit number for error output */
    integer  ioErr;                   /* Exit flag from OPEN and CLOSE */
    integer io_buffer = 11;    /* FORTRAN unit internal input/output */
    integer status;            /* Exit flag from CUTEst tools */

    char msgBuf[256];

    int *iir = NULL;


    int bufLen, errCopy;
    double *nptr = NULL, *mptr = NULL, *nnzjptr = NULL, *nnzhptr = NULL;
    doublereal *x = NULL, *bl = NULL, *bu = NULL, *v = NULL, *cl = NULL,
      *cu = NULL, *f = NULL, *c = NULL, *g = NULL, *J = NULL,
      *H = NULL;

    doublereal *p, *r;

    logical *equatn = NULL, *linear = NULL;
    /* logical  efirst = TRUE_, lfirst = TRUE_, nvfrst = FALSE_; */
    integer e_order = 1, l_order = 1, v_order = 0;
    logical  somethingFalse = FALSE_, somethingTrue = TRUE_;
    logical  individual;

    /* Matlab logicals are not the same as CUTEst logicals */
    mxLogical *eFirst, *lFirst, *nvFirst;
    bool    *bool_equatn, *bool_linear;

    char      probName[STR_LEN+1];
    char     *cptr;
    char     *Fvnames, *Fcnames;   /* For Fortran */
    char    **vNames, **cNames;    /* C arrays of strings */
    mxArray *Mn = NULL, *Mm = NULL, *Mnnzh = NULL, *Mnnzj = NULL,
      *Mx = NULL, *Mbl = NULL, *Mbu = NULL, *Mequatn = NULL,
      *Mlinear = NULL, *Mv = NULL, *Mcl = NULL, *Mcu = NULL,
      *MprobName = NULL;

    mxLogical *gradfptr, *jtransptr;
    logical gradf, jtrans;

    mwIndex *ir, *jptr;
    int  i, j;

    mxArray *matrix;    /* Output Matlab sparse matrix */


    /* ------------------------------------------------------------------ */

    if (nrhs == 0) mexErrMsgTxt("At least one argument must be given\n");

    if (mxIsChar(prhs[0]) != 1)
      mexErrMsgTxt("First argument must be the tool name\n");

    bufLen = mxGetN(prhs[0]) + 1;

    if (! (toolName = (char*)mxCalloc(bufLen, sizeof(char))))
      mexErrMsgTxt("Could not allocate memory to read tool name\n");

    errCopy = mxGetString(prhs[0], toolName, bufLen);
    if (errCopy) mexWarnMsgTxt("Tool name was truncated by mistake\n");

#ifdef MXDEBUG
    mexPrintf("Calling %-s with %-d input arg(s) and %-d output arg(s)\n",
               toolName, nrhs-1, nlhs);
#endif

    /* ------------------------------------------------------------------ */

    /* Obtain problem dimensions.
     * usage: cutest_dims()
     */
    if (strcmp(toolName, "dims") == 0) {
      if (nlhs != 2) mexErrMsgTxt("cutest_dims returns 2 output values\n");
      if (nrhs > 1) mexWarnMsgTxt("cutest_dims takes no input argument\n");

#ifdef MXDEBUG
      mexPrintf("Opening data file\n");
#endif
      ioErr = 0;
      if (! dataFileOpen) FORTRAN_open(&funit, fName, &ioErr);
      if (ioErr) mexErrMsgTxt("Error opening file OUTSDIF.d\n");
      dataFileOpen = 1;

#ifdef MXDEBUG
      mexPrintf("Calling CDIMEN\n");
#endif
      CUTEST_cdimen( &status, &funit, &CUTEst_nvar, &CUTEst_ncon);
      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }

#ifdef MXDEBUG
      mexPrintf("  n = %-d, m = %-d\n", CUTEst_nvar, CUTEst_ncon);
#endif

      plhs[0] = mxCreateDoubleMatrix(1, 1, mxREAL);
      nptr = mxGetPr(plhs[0]);
      *nptr = (double)CUTEst_nvar;

      plhs[1] = mxCreateDoubleMatrix(1, 1, mxREAL);
      mptr = mxGetPr(plhs[1]);
      *mptr = (double)CUTEst_ncon;

      mxFree((void *)toolName);
      return;
    }

    /* Setup problem and return a Matlab structure with all data.
     * Usage:  prob = cutest_setup()
     */
    if (strcmp(toolName, "setup") == 0) {

      if (setupCalled) mexErrMsgTxt("cutest_setup: cutest_terminate must be called first\n");

      if (nlhs != 1) mexErrMsgTxt("cutest_setup returns one output\n");
      if (nrhs > 1) {
        if (nrhs == 2) {
          if (! isInteger(prhs[1]) && ! mxIsDouble(prhs[1]))
            mexErrMsgTxt("cutest_setup: derivative type must be integer\n");

          if (isInteger(prhs[1])) {
            dertype_ptr = (integer *)mxGetData(prhs[2]);
            CUTEst_dertype = dertype_ptr[0];
          } else {
            CUTEst_dertype = (integer)*mxGetPr(prhs[1]);
          }
        }
        else if (nrhs == 5) {
          /* Check input arguments type */
          if (! isInteger(prhs[1]) && ! mxIsDouble(prhs[1]))
            mexErrMsgTxt("cutest_setup: derivative type must be integer\n");

          if (isInteger(prhs[1])) {
            dertype_ptr = (integer *)mxGetData(prhs[1]);
            CUTEst_dertype = dertype_ptr[0];
          } else {

            CUTEst_dertype = (integer)*mxGetPr(prhs[1]);
          }
          for (i = 2; i < 5; i++)
            if (!mxIsLogicalScalar(prhs[i]))
              mexWarnMsgTxt("Setup args 2-4 must be logicals\n");

          /* Read input arguments */
          eFirst  = mxGetLogicals(prhs[2]);
          lFirst  = mxGetLogicals(prhs[3]);
          nvFirst = mxGetLogicals(prhs[4]);

          /*          efirst = *eFirst  ? TRUE_ : FALSE_;
          lfirst = *lFirst  ? TRUE_ : FALSE_;
          nvfrst = *nvFirst ? TRUE_ : FALSE_; */
        }
        else {
            mexErrMsgTxt("setup takes 0, 1 or 4 arguments\n");
        }
      }
      else {
        CUTEst_dertype = 2;
      }

      if (CUTEst_dertype < 0 || CUTEst_dertype > 2) {
        CUTEst_dertype = 2;
      }

#ifdef MXDEBUG
      mexPrintf("Opening data file\n");
#endif
      ioErr = 0;
      if (! dataFileOpen) FORTRAN_open(&funit, fName, &ioErr);
      if (ioErr) mexErrMsgTxt("Error opening file OUTSDIF.d\n");

#ifdef MXDEBUG
      mexPrintf("Calling CDIMEN\n");
#endif
      CUTEST_cdimen( &status, &funit, &CUTEst_nvar, &CUTEst_ncon);
      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }
#ifdef MXDEBUG
      mexPrintf("  n = %-d, m = %-d\n", CUTEst_nvar, CUTEst_ncon);

      mexPrintf("Allocating double/logical work space\n");
#endif
      Mx  = mxCreateDoubleMatrix(CUTEst_nvar, 1, mxREAL);
      Mbl = mxCreateDoubleMatrix(CUTEst_nvar, 1, mxREAL);
      Mbu = mxCreateDoubleMatrix(CUTEst_nvar, 1, mxREAL);
      Mv  = mxCreateDoubleMatrix(CUTEst_ncon, 1, mxREAL);
      Mcl = mxCreateDoubleMatrix(CUTEst_ncon, 1, mxREAL);
      Mcu = mxCreateDoubleMatrix(CUTEst_ncon, 1, mxREAL);

#ifdef MXDEBUG
      mexPrintf("Transfering pointers\n");
#endif
      x  = (doublereal *)mxGetData(Mx);
      bl = (doublereal *)mxGetData(Mbl);
      bu = (doublereal *)mxGetData(Mbu);
      equatn = (logical *)mxCalloc(CUTEst_ncon, sizeof(logical));
      linear = (logical *)mxCalloc(CUTEst_ncon, sizeof(logical));
      v  = (doublereal *)mxGetData(Mv);
      cl = (doublereal *)mxGetData(Mcl);
      cu = (doublereal *)mxGetData(Mcu);

#ifdef MXDEBUG
      mexPrintf("Calling [UC]SETUP\n");
#endif
      if (CUTEst_ncon > 0)
        CUTEST_csetup( &status, &funit, &iout, &io_buffer,
                       &CUTEst_nvar, &CUTEst_ncon, x, bl, bu,
                       v, cl, cu, equatn, linear,
                       &e_order, &l_order, &v_order );
      else
        CUTEST_usetup( &status, &funit, &iout, &io_buffer,
                       &CUTEst_nvar, x, bl, bu );
      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }
#ifdef MXDEBUG
      mexPrintf("  n = %-d, m = %-d\n", CUTEst_nvar, CUTEst_ncon);
#endif

      /* Transfer equatn and logical */
      Mequatn = mxCreateLogicalMatrix(CUTEst_ncon, 1);
      Mlinear = mxCreateLogicalMatrix(CUTEst_ncon, 1);
      bool_equatn = (bool *)mxGetData(Mequatn);
      bool_linear = (bool *)mxGetData(Mlinear);

      for (i = 0; i < CUTEst_ncon; i++) {
        bool_equatn[i] = equatn[i] ? true : false;
        bool_linear[i] = linear[i] ? true : false;
      }

      /* Free temporary logical arrays */
      mxFree(equatn);
      mxFree(linear);

#ifdef MXDEBUG
      mexPrintf("Calling CDIMSH/CDIMSJ\n");
#endif

      if (CUTEst_dertype == 2) {
        if (CUTEst_ncon > 0)
          CUTEST_cdimsh( &status, &CUTEst_nnzh);
        else
          CUTEST_udimsh( &status, &CUTEst_nnzh);

        if (status != 0) {
            sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
            mexErrMsgTxt(msgBuf);
          }
      } else {
        CUTEst_nnzh = - 1 ;
      }
      if (CUTEst_dertype > 0) {
        if (CUTEst_ncon > 0) {
          CUTEST_cdimsj( &status, &CUTEst_nnzj);
          CUTEst_nnzj -= CUTEst_nvar;
        }
      } else {
        CUTEst_nnzj = - 1 ;
      }
#ifdef MXDEBUG
      mexPrintf("  nnzh = %-d, nnzj = %-d\n", CUTEst_nnzh, CUTEst_nnzj);
      mexPrintf("Finding out problem name\n");
#endif
      CUTEST_probname( &status, probName );
      probName[STR_LEN] = '\0';
      MprobName = mxCreateString(probName);

#ifdef MXDEBUG
      mexPrintf("  %-s\n", probName);
      mexPrintf("Closing data file\n");
#endif
      FORTRAN_close(&funit, &ioErr);
      if (ioErr) mexWarnMsgTxt("Error closing file OUTSDIF.d\n");
      dataFileOpen = 0;

#ifdef MXDEBUG
      mexPrintf("Storing integer data\n");
#endif
      Mn = mxCreateDoubleMatrix(1, 1, mxREAL);
      nptr = mxGetPr(Mn);
      *nptr = (double)CUTEst_nvar;

      Mm = mxCreateDoubleMatrix(1, 1, mxREAL);
      mptr = mxGetPr(Mm);
      *mptr = (double)CUTEst_ncon;

      Mnnzh = mxCreateDoubleMatrix(1, 1, mxREAL);
      nnzhptr = mxGetPr(Mnnzh);
      *nnzhptr = (double)CUTEst_nnzh;

      Mnnzj = mxCreateDoubleMatrix(1, 1, mxREAL);
      nnzjptr = mxGetPr(Mnnzj);
      *nnzjptr = (double)CUTEst_nnzj;

#ifdef MXDEBUG
      mexPrintf("Building struct with %-d fields\n", nFields);
#endif
      plhs[0] = mxCreateStructMatrix(1, 1, nFields, fieldNames);
      mxSetField(plhs[0], 0, field_n,      Mn       );
      mxSetField(plhs[0], 0, field_m,      Mm       );
      mxSetField(plhs[0], 0, field_nnzh,   Mnnzh    );
      mxSetField(plhs[0], 0, field_nnzj,   Mnnzj    );
      mxSetField(plhs[0], 0, field_x,      Mx       );
      mxSetField(plhs[0], 0, field_bl,     Mbl      );
      mxSetField(plhs[0], 0, field_bu,     Mbu      );
      mxSetField(plhs[0], 0, field_v,      Mv       );
      mxSetField(plhs[0], 0, field_cl,     Mcl      );
      mxSetField(plhs[0], 0, field_cu,     Mcu      );
      mxSetField(plhs[0], 0, field_equatn, Mequatn  );
      mxSetField(plhs[0], 0, field_linear, Mlinear  );
      mxSetField(plhs[0], 0, field_pbname, MprobName);

      setupCalled = 1;
      mxFree((void *)toolName);
      return;
    }

    /* ------------------------------------------------------------------ */

    if (! setupCalled) mexErrMsgTxt("cutest_setup must be called first\n");

    /* Obtain variable names
     * Usage: vnames = cutest_varnames( &status, )
     */
    if (strcmp(toolName, "varnames") == 0) {

      if (nlhs != 1) mexErrMsgTxt("varnames returns a single output\n");
      if (nrhs > 1)
        mexWarnMsgTxt("varnames does not take input arguments\n");

      MALLOC(Fvnames, CUTEst_nvar * STR_LEN, char);
      if (!Fvnames)
        mexErrMsgTxt("cutest_varnames: Error allocating room for variable names\n");

      CUTEST_varnames( &status, &CUTEst_nvar, Fvnames);

      /* Transfer to a C array of strings.
       * If you know of a cleaner and portable way to do this, please
       * let me know!
       */
      MALLOC(vNames, CUTEst_nvar, char*);
      for (i = 0; i < CUTEst_nvar; i++) {
        MALLOC(vNames[i], STR_LEN+1, char);
        cptr = Fvnames + i * STR_LEN;
        for (j = 0; j < STR_LEN; j++) {
          vNames[i][j] = *cptr;
          cptr++;
        }
        vNames[i][STR_LEN] = '\0';
      }
      FREE(Fvnames);

      plhs[0] = mxCreateCharMatrixFromStrings((mwSize)CUTEst_nvar,
                                              (const char **)vNames);

      for (i = 0; i < CUTEst_nvar; i++) FREE(vNames[i]);
      FREE(vNames);
      mxFree((void *)toolName);
      return;
    }

    /* ------------------------------------------------------------------ */

    /* Obtain constraint names
     * Usage: cnames = cutest_connames( &status, )
     */
    if (strcmp(toolName, "connames") == 0) {

      if (CUTEst_ncon == 0) {
        sprintf(msgBuf, onlyConst, toolName);
        mexErrMsgTxt(msgBuf);
      }

      if (nlhs != 1) mexErrMsgTxt("cutest_connames returns a single output\n");
      if (nrhs > 1)
        mexWarnMsgTxt("cutest_connames does not take input arguments\n");

      MALLOC(Fcnames, CUTEst_ncon * STR_LEN, char);
      if (!Fcnames)
        mexErrMsgTxt("cutest_connames: Error allocating room for constraint names\n");

      CUTEST_connames( &status, &CUTEst_ncon, Fcnames);
      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }

      /* Transfer to a C array of strings.
       * If you know of a cleaner and portable way to do this, please
       * let me know!
       */
      MALLOC(cNames, CUTEst_ncon, char*);
      for (i = 0; i < CUTEst_ncon; i++) {
        MALLOC(cNames[i], STR_LEN+1, char);
        cptr = Fcnames + i * STR_LEN;
        for (j = 0; j < STR_LEN; j++) {
          cNames[i][j] = *cptr;
          cptr++;
        }
        cNames[i][STR_LEN] = '\0';
      }
      FREE(Fcnames);

      plhs[0] = mxCreateCharMatrixFromStrings((mwSize)CUTEst_ncon,
                                              (const char **)cNames);

      for (i = 0; i < CUTEst_ncon; i++) FREE(cNames[i]);
      FREE(cNames);
      mxFree((void *)toolName);
      return;
    }

    /* ------------------------------------------------------------------ */

    /* Evaluate objective function value and constraint bodies.
     * Usage:  [f,c] = cutest_objcons(x)
     */
    if (strcmp(toolName, "objcons") == 0) {

      if (CUTEst_ncon == 0) {
        sprintf(msgBuf, onlyConst, toolName);
        mexWarnMsgTxt(msgBuf);
      }
      if (nrhs != 2) mexErrMsgTxt("cutest_objcons: Please specify x\n");
      if (nlhs != 2) mexErrMsgTxt("cutest_objcons: Need two output arguments\n");

      if (! mxIsDouble(prhs[1]))
        mexErrMsgTxt("cutest_objcons: Input array must have type double\n");

      if (mxGetNumberOfElements(prhs[1]) != CUTEst_nvar)
        mexErrMsgTxt("cutest_objcons: Input array has erroneous size\n");

      x = (doublereal *)mxGetData(prhs[1]);
      plhs[0] = mxCreateDoubleMatrix(1, 1, mxREAL);
      f = (doublereal *)mxGetData(plhs[0]);
      plhs[1] = mxCreateDoubleMatrix(CUTEst_ncon, 1, mxREAL);
      c = (doublereal *)mxGetData(plhs[1]);

      CUTEST_cfn( &status, &CUTEst_nvar, &CUTEst_ncon, x, f, c);
      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }

      mxFree((void *)toolName);
      return;
    }

    /* =============== Dense first derivative tools ===================== */

    /* Return function value and gradient if requested.
     * Usage:  f = cutest_obj(x)  or  [f,g] = cutest_obj(x)
     */
    if (strcmp(toolName, "obj") == 0) {

      if (nrhs != 2) mexErrMsgTxt("cutest_obj: Please specify x\n");
      if (nlhs < 1) mexErrMsgTxt("cutest_obj: Please specify an output argument\n");
      if (nlhs > 2) mexErrMsgTxt("cutest_obj: Too many output arguments\n");

      if (! mxIsDouble(prhs[1]))
        mexErrMsgTxt("cutest_obj: Input array must have type double\n");

      if (mxGetNumberOfElements(prhs[1]) != CUTEst_nvar)
        mexErrMsgTxt("cutest_obj: Input array has erroneous size\n");

      x  = (doublereal *)mxGetData(prhs[1]);
      plhs[0] = mxCreateDoubleMatrix(1, 1, mxREAL);
      f  = (doublereal *)mxGetData(plhs[0]);
      if (nlhs == 2) {
        plhs[1] = mxCreateDoubleMatrix(CUTEst_nvar, 1, mxREAL);
        g  = (doublereal *)mxGetData(plhs[1]);
      }

      if (CUTEst_ncon == 0)
        if (nlhs == 1)
          CUTEST_uofg( &status, &CUTEst_nvar, x, f, NULL, &somethingFalse);
        else
          CUTEST_uofg( &status, &CUTEst_nvar, x, f, g, &somethingTrue);
      else
        if (nlhs == 1)
          CUTEST_cofg( &status, &CUTEst_nvar, x, f, NULL, &somethingFalse);
        else
          CUTEST_cofg( &status, &CUTEst_nvar, x, f, g, &somethingTrue);

      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }

      mxFree((void *)toolName);
      return;
    }

    /* Return function value and sparse gradient if requested.
     * Usage:  f = cutest_sobj(x)  or  [f,sg] = cutest_sobj(x)
     */
    if (strcmp(toolName, "sobj") == 0) {

      if (nrhs != 2) mexErrMsgTxt("cutest_sobj: Please specify x\n");
      if (nlhs < 1) mexErrMsgTxt("cutest_sobj: Please specify an output argument\n");
      if (nlhs > 2) mexErrMsgTxt("cutest_sobj: Too many output arguments\n");

      if (! mxIsDouble(prhs[1]))
        mexErrMsgTxt("cutest_sobj: Input array must have type double\n");

      if (mxGetNumberOfElements(prhs[1]) != CUTEst_nvar)
        mexErrMsgTxt("cutest_sobj: Input array has erroneous size\n");

      x  = (doublereal *)mxGetData(prhs[1]);
      plhs[0] = mxCreateDoubleMatrix(1, 1, mxREAL);
      f  = (doublereal *)mxGetData(plhs[0]);

      /*
      if (nlhs == 2) {

        plhs[1] = mxCreateDoubleMatrix(CUTEst_nvar, 1, mxREAL);
        g  = (doublereal *)mxGetData(plhs[1]);
        } */

      if (CUTEst_ncon == 0) {
        /* sobj does not apply to unconstrained problems.
         * Return dense gradient if requested.
         */
        sprintf(msgBuf, onlyConst, toolName);
        mexWarnMsgTxt(msgBuf);
        if (nlhs == 1)
          CUTEST_uofg( &status, &CUTEst_nvar, x, f, NULL, &somethingFalse);
        else {
          plhs[1] = mxCreateDoubleMatrix(CUTEst_nvar, 1, mxREAL);
          g  = (doublereal *)mxGetData(plhs[1]);
          CUTEST_uofg( &status, &CUTEst_nvar, x, f, g, &somethingTrue);
        }
      }
      else
        if (nlhs == 1) {
          CUTEST_cofsg( &status, &CUTEst_nvar, x, f,  &zero,  &zero,
                        NULL, NULL, &somethingFalse);
          if (status != 0) {
            sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
            mexErrMsgTxt(msgBuf);
          }
          }
        else {
          nnzgci = CUTEst_nvar;
          ir = (integer *)mxCalloc(nnzgci, sizeof(integer));
          g = (doublereal *)mxCalloc(nnzgci, sizeof(doublereal));
          integer nnzgci0 = nnzgci;
          CUTEST_cofsg( &status, &CUTEst_nvar, x, f, &nnzgci, &nnzgci0, g,
                        (integer *)ir, &somethingTrue);
          if (status != 0) {
            sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
            mexErrMsgTxt(msgBuf);
          }
          plhs[1] = SparseVector(CUTEst_nvar, nnzgci, ir, (double *)g);

          mxFree(ir);
          mxFree(g);
        }



      mxFree((void *)toolName);
      return;
    }

    /* =============== Dense first derivative tools ===================== */

    /* Return Lagrangian function value and gradient if requested.
     * Usage:  f = cutest_lag(x,y)  or  [f,g] = cutest_lag(x,y)
     */
    if (strcmp(toolName, "lag") == 0) {

      if (nrhs < 2 || nrhs > 3)
        mexErrMsgTxt("cutest_lag: Please specify x and v\n");
      if (nlhs < 1) mexErrMsgTxt("cutest_lag: Please specify an output argument\n");
      if (nlhs > 2) mexErrMsgTxt("cutest_lag: Too many output arguments\n");
      if (! mxIsDouble(prhs[1]))
        mexErrMsgTxt("cutest_lag: 1st input array must be of type double\n");
      if (! mxIsDouble(prhs[2]))
        mexErrMsgTxt("cutest_lag: 2nd input array must be of type double\n");

      x = (doublereal *)mxGetData(prhs[1]);
      v = (doublereal *)mxGetData(prhs[2]);
      plhs[0] = mxCreateDoubleMatrix(1, 1, mxREAL);
      f  = (doublereal *)mxGetData(plhs[0]);
      if (nlhs == 2) {
        plhs[1] = mxCreateDoubleMatrix(CUTEst_nvar, 1, mxREAL);
        g  = (doublereal *)mxGetData(plhs[1]);
      }

      if (nlhs == 1)
        CUTEST_clfg( &status, &CUTEst_nvar, &CUTEst_ncon, x, v,
                     f, NULL, &somethingFalse);
      else
        CUTEST_clfg( &status, &CUTEst_nvar, &CUTEst_ncon,x, v,
                     f, g, &somethingTrue);

      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }

      mxFree((void *)toolName);
      return;
    }

    /* ------------------------------------------------------------------ */

    /* Return function gradient.
     * Usage:  g = cutest_grad(x) or g = cutest_grad(x,i)
     */
    if (strcmp(toolName, "grad") == 0) {

      if (nrhs < 2 || nrhs > 4)
        mexErrMsgTxt("cutest_grad: Please specify x and possibly index\n");
      if (nlhs < 1) mexErrMsgTxt("cutest_grad: Please specify an output argument\n");
      if (nlhs > 2) mexErrMsgTxt("cutest_grad: Too many output arguments\n");

      if (! mxIsDouble(prhs[1]))
        mexErrMsgTxt("cutest_grad: Input array must have type double\n");

      if (mxGetNumberOfElements(prhs[1]) != CUTEst_nvar)
        mexErrMsgTxt("cutest_grad: Input array has erroneous size\n");

      if (nrhs == 3) {
        if (! isInteger(prhs[2]) && ! mxIsDouble(prhs[2]))
          mexErrMsgTxt("cutest_cons: Constraint index must be integer\n");

        if (isInteger(prhs[2])) {
          icon_ptr = (integer *)mxGetData(prhs[2]);
          icon = icon_ptr[0];
        } else
          icon = (integer)*mxGetPr(prhs[2]);

        if (icon < 0 || icon > CUTEst_ncon) {
          sprintf(msgBuf,
                   "cutest_cons: Invalid constraint index %-d\n", icon);
          mexErrMsgTxt(msgBuf);
        }
      } else {
        icon = 0;
      }

      x  = (doublereal *)mxGetData(prhs[1]);
      plhs[0] = mxCreateDoubleMatrix(CUTEst_nvar, 1, mxREAL);
      g  = (doublereal *)mxGetData(plhs[0]);

      /*      if (nrhs == 2) {
        if (CUTEst_ncon > 0)
          CUTEST_cigr( &status, &CUTEst_nvar, 0, x, g);
        else
          CUTEST_ugr( &status, &CUTEst_nvar, x, g);
          } else { */
        if (CUTEst_ncon > 0)
          CUTEST_cigr( &status, &CUTEst_nvar, &icon, x, g);
        else
          CUTEST_ugr( &status, &CUTEst_nvar, x, g);
/*      } */
      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }

      mxFree((void *)toolName);
      return;
    }


    /* ------------------------------------------------------------------ */

    /* Return function gradient.
     * Usage:  g = cutest_sgrad(x) or g = cutest_sgrad(x,i)
     */
    if (strcmp(toolName, "sgrad") == 0) {

      if (nrhs < 2 || nrhs > 4)
        mexErrMsgTxt("cutest_grad: Please specify x and possibly index\n");
      if (nlhs < 1) mexErrMsgTxt("cutest_grad: Please specify an output argument\n");
      if (nlhs > 2) mexErrMsgTxt("cutest_grad: Too many output arguments\n");

      if (! mxIsDouble(prhs[1]))
        mexErrMsgTxt("cutest_grad: Input array must have type double\n");

      if (mxGetNumberOfElements(prhs[1]) != CUTEst_nvar)
        mexErrMsgTxt("cutest_grad: Input array has erroneous size\n");

      if (nrhs == 3) {
        if (! isInteger(prhs[2]) && ! mxIsDouble(prhs[2]))
          mexErrMsgTxt("cutest_cons: Constraint index must be integer\n");

        if (isInteger(prhs[2])) {
          icon_ptr = (integer *)mxGetData(prhs[2]);
          icon = icon_ptr[0];
        } else
          icon = (integer)*mxGetPr(prhs[2]);

        if (icon < 0 || icon > CUTEst_ncon) {
          sprintf(msgBuf,
                   "cutest_cons: Invalid constraint index %-d\n", icon);
          mexErrMsgTxt(msgBuf);
        }
      } else {
        icon = 0;
      }

      x  = (doublereal *)mxGetData(prhs[1]);

      if (CUTEst_ncon == 0) {
        /* sobj does not apply to unconstrained problems.
         * Return dense gradient if requested.
         */
        sprintf(msgBuf, onlyConst, toolName);
        mexWarnMsgTxt(msgBuf);
        plhs[0] = mxCreateDoubleMatrix(CUTEst_nvar, 1, mxREAL);
        g  = (doublereal *)mxGetData(plhs[0]);
        CUTEST_ugr( &status, &CUTEst_nvar, x, g );
      } else {
        nnzgci = CUTEst_nvar;
        ir = (integer *)mxCalloc(nnzgci, sizeof(integer));
        g = (doublereal *)mxCalloc(nnzgci, sizeof(doublereal));
        integer nnzgci0 = nnzgci;
        CUTEST_cisgr( &status, &CUTEst_nvar, &icon, x, &nnzgci, &nnzgci0, g,
                      ir);
        if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }
        plhs[0] = SparseVector(CUTEst_nvar, nnzgci, (integer *)ir, (double *)g);

        mxFree(ir);
        mxFree(g);
      }
      mxFree((void *)toolName);
      return;
    }

    /* ------------------------------------------------------------------ */

    /* Return constraint bodies and Jacobian if requested.
     * Usage:   c = cutest_cons(x)    or    [c,J] = cutest_cons(x)
     *         ci = cutest_cons(x,i)  or  [ci,gi] = cutest_cons(x,i)
     */
    if (strcmp(toolName, "cons") == 0) {

      if (CUTEst_ncon == 0) {
        sprintf(msgBuf, onlyConst, toolName);
        mexWarnMsgTxt(msgBuf);
      }
      if (nrhs < 2 || nrhs > 4)
        mexErrMsgTxt("cutest_cons: Please specify x and possibly index\n");
      if (nlhs < 1) mexErrMsgTxt("cutest_cons: Please specify an output argument\n");
      if (nlhs > 2) mexErrMsgTxt("cutest_cons: Too many output arguments\n");

      if (! mxIsDouble(prhs[1]))
        mexErrMsgTxt("cutest_cons: Input array must have type double\n");

      if (mxGetNumberOfElements(prhs[1]) != CUTEst_nvar)
        mexErrMsgTxt("cutest_cons: Input array has erroneous size\n");

      if (nrhs == 3) {
        if (! isInteger(prhs[2]) && ! mxIsDouble(prhs[2]))
          mexErrMsgTxt("cutest_cons: Constraint index must be integer\n");

        if (isInteger(prhs[2])) {
          icon_ptr = (integer *)mxGetData(prhs[2]);
          icon = icon_ptr[0];
        } else
          icon = (integer)*mxGetPr(prhs[2]);

        if (icon <= 0 || icon > CUTEst_ncon) {
          sprintf(msgBuf,
                   "cutest_cons: Invalid constraint index %-d\n", icon);
          mexErrMsgTxt(msgBuf);
        }
      }

      x  = (doublereal *)mxGetData(prhs[1]);

      if (nrhs == 2) {
        /* Constraint bodies (and Jacobian) were requested */
        plhs[0] = mxCreateDoubleMatrix(CUTEst_ncon, 1, mxREAL);
        c  = (doublereal *)mxGetData(plhs[0]);
        if (nlhs == 2) {
          plhs[1] =mxCreateDoubleMatrix(CUTEst_ncon,CUTEst_nvar,mxREAL);
          J  = (doublereal *)mxGetData(plhs[1]);
        }

        if (nlhs == 1)
          CUTEST_ccfg( &status, &CUTEst_nvar, &CUTEst_ncon, x, c,
                &somethingFalse, &zero, &zero, NULL, &somethingFalse);
        else
          CUTEST_ccfg( &status, &CUTEst_nvar, &CUTEst_ncon, x, c,
                &somethingFalse, &CUTEst_ncon, &CUTEst_nvar, J,
                &somethingTrue);
      } else {
        /* Single constraint value (and gradient) were requested */
        plhs[0] = mxCreateDoubleMatrix(1, 1, mxREAL);
        c = (doublereal *)mxGetData(plhs[0]);
        if (nlhs == 2) {
          plhs[1] = mxCreateDoubleMatrix(CUTEst_nvar, 1, mxREAL);
          g = (doublereal *)mxGetData(plhs[1]);
        }

        if (nlhs == 1)         /* Only constraint value is requested */
          CUTEST_ccifg( &status, &CUTEst_nvar, &icon, x, c,
                        NULL, &somethingFalse);
        else           /* Constraint value and gradient are requested */
          CUTEST_ccifg( &status, &CUTEst_nvar, &icon, x, c,
                        g, &somethingTrue);
      }
      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }

      mxFree((void *)toolName);
      return;
    }

    /* ------------------------------------------------------------------ */

    /* Return the gradient of the objective or Lagrangian and Jacobian
     * Usage:  [g,J] = cutest_lagjac(x)  or  [g,J] = cutest_lagjac(x,v)
     */
    if (strcmp(toolName, "lagjac") == 0) {

      if (CUTEst_ncon == 0) {
        sprintf(msgBuf, onlyConst, toolName);
        mexWarnMsgTxt(msgBuf);
      }
      if (nrhs < 2 || nrhs > 3)
        mexErrMsgTxt("cutest_lagjac: Please specify x and possibly v\n");
      if (nlhs != 2)
        mexErrMsgTxt("cutest_lagjac: Two output arguments returned\n");

      if (! mxIsDouble(prhs[1]))
        mexErrMsgTxt("cutest_lagjac: Input array must be of type double\n");

      if (nrhs == 3)
        if (! mxIsDouble(prhs[2]))
          mexErrMsgTxt("cutest_lagjac: Input array must have type double\n");

      x = (doublereal *)mxGetData(prhs[1]);
      if (nrhs == 3) v = (doublereal *)mxGetData(prhs[2]);
      plhs[0] = mxCreateDoubleMatrix(CUTEst_nvar, 1, mxREAL);
      g = (doublereal *)mxGetData(plhs[0]);
      plhs[1] = mxCreateDoubleMatrix(CUTEst_ncon, CUTEst_nvar, mxREAL);
      J  = (doublereal *)mxGetData(plhs[1]);

      if (nrhs == 2)             /* g = gradient of objective function */
        CUTEST_cgr( &status, &CUTEst_nvar, &CUTEst_ncon, x, NULL,
            &somethingFalse, g, &somethingFalse, &CUTEst_ncon, &CUTEst_nvar, J);
      else                                /* g = gradient of Lagrangian */
        CUTEST_cgr( &status, &CUTEst_nvar, &CUTEst_ncon, x, v,
            &somethingTrue, g, &somethingFalse, &CUTEst_ncon, &CUTEst_nvar, J);
      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }

      mxFree((void *)toolName);
      return;
    }

    /* ============== Sparse first derivative tools ===================== */

    /* Return constraint bodies and sparse Jacobian
     * or return a single constraint value and its gradient in sparse format
     * Usage:  [c,J] = cutest_scons(x)
     *         [ci, sgci] = cutest_scons(x, i)
     */
    if (strcmp(toolName, "scons") == 0) {

      if (CUTEst_ncon == 0) {
        sprintf(msgBuf, onlyConst, toolName);
        mexWarnMsgTxt(msgBuf);
      }
      if (nrhs < 2 || nrhs > 4)
        mexErrMsgTxt("cutest_scons: Please specify x and possibly index\n");
      if (nlhs != 2)
        mexErrMsgTxt("cutest_scons: Two output arguments returned\n");

      if (! mxIsDouble(prhs[1]))
        mexErrMsgTxt("cutest_scons: Input array must be of type double\n");

      if (mxGetNumberOfElements(prhs[1]) != CUTEst_nvar)
        mexErrMsgTxt("cutest_scons: Input array has erroneous size\n");

      if (nrhs == 3) {
        if (! isInteger(prhs[2]) && ! mxIsDouble(prhs[2]))
          mexErrMsgTxt("cutest_iscons: Constraint index must be integer\n");

        if (isInteger(prhs[2])) {
          icon_ptr = (integer *)mxGetData(prhs[2]);
          icon = icon_ptr[0];
        } else
          icon = (integer)*mxGetPr(prhs[2]);

        if (icon <= 0 || icon > CUTEst_ncon) {
          sprintf(msgBuf,
                  "cutest_iscons: Invalid constraint index %-d\n",icon);
          mexErrMsgTxt(msgBuf);
        }
      }

      x = (doublereal *)mxGetData(prhs[1]);

      if (nrhs == 2) {
        /* Constraint bodies and sparse Jacobian were requested */
        plhs[0] = mxCreateDoubleMatrix(CUTEst_ncon, 1, mxREAL);
        c = (doublereal *)mxGetData(plhs[0]);
        J = (doublereal *)mxCalloc(CUTEst_nnzj, sizeof(doublereal));
        irow = (integer *)mxCalloc(CUTEst_nnzj, sizeof(integer));
        jcol = (integer *)mxCalloc(CUTEst_nnzj, sizeof(integer));
        integer CUTEst_nnzj0 = CUTEst_nnzj;

        CUTEST_ccfsg( &status, &CUTEst_nvar, &CUTEst_ncon, x, c, &CUTEst_nnzj,
              &CUTEst_nnzj0, J, jcol, irow, &somethingTrue);
        if (status != 0) {
            sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
            mexErrMsgTxt(msgBuf);
          }

        /* Convert sparse matrix to Matlab format */
        plhs[1] = coordToMatlabSparse(CUTEst_ncon, CUTEst_nvar,
                                      CUTEst_nnzj, irow,
                                      jcol, (double *)J);

        mxFree(jcol);
        mxFree(irow);
        mxFree(J);

      } else {

        /* Single constraint and sparse gradient were requested */
        plhs[0] = mxCreateDoubleMatrix(1, 1, mxREAL);
        c  = (doublereal *)mxGetData(plhs[0]);

        /* This must be improved ! */
/*  nnzgci = CUTEst_nnzj > CUTEst_nvar ? CUTEst_nvar : CUTEst_nnzj; */
        if (CUTEst_nnzj > CUTEst_nvar)
          nnzgci = CUTEst_nvar;
        else
          nnzgci = CUTEst_nnzj;

#ifdef MXDEBUG
        mexPrintf("iscons:: Before: nnzgci = %-d icon = %-d\n", nnzgci, icon);
        mexPrintf("iscons::         nvar   = %-d\n",  CUTEst_nvar);
#endif

        ir = (integer *)mxCalloc(nnzgci, sizeof(integer));
        g = (doublereal *)mxCalloc(nnzgci, sizeof(doublereal));
        integer nnzgci0 = nnzgci;

        CUTEST_ccifsg( &status, &CUTEst_nvar, &icon, x, c, &nnzgci, &nnzgci0, g,
                       ir, &somethingTrue);
        if (status != 0) {
            sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
            mexErrMsgTxt(msgBuf);
          }

        /*
        mexPrintf("iir[0] = %-d %f\n", iir[0], g[0]);
        mexPrintf("iir[1] = %-d %f\n", iir[1], g[1]);
        */
        plhs[1] = SparseVector(CUTEst_nvar, nnzgci, (integer *)ir, (double *)g);

        mxFree(ir);
        mxFree(g);

        /*        plhs[1] = mxCreateSparse((mwSize)CUTEst_nvar, (mwSize)1,
                                 (mwSize)nnzgci, mxREAL);
        ir   = mxGetIr(plhs[1]);
        jptr = mxGetJc(plhs[1]);
        g    = (doublereal *)mxGetPr(plhs[1]);

        CUTEST_ccifsg( &status, &CUTEst_nvar, &icon, x, c, &nnzgci, &nnzgci, g,
               (integer *)ir, &somethingTrue);
        if (status != 0) {
            sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
            mexErrMsgTxt(msgBuf);
          }

        mexPrintf("ir[0] = %-d %f\n", ir[0], g[0]);
        mexPrintf("ir[1] = %-d %f\n", ir[1], g[1]);
        mexPrintf("ir[2] = %-d %f\n", ir[2], g[2]);

        /* Finalize sparse data structure for sparse gradient */
        /* for (i = 0; i < nnzgci; i++) ir[i]--;    /* 0-based indexing */
        /* jptr[0] = 0;
        jptr[1] = nnzgci;

        */

#ifdef MXDEBUG
        mexPrintf("iscons:: After: nnzgci = %-d\n", nnzgci);
#endif
      }

      mxFree((void *)toolName);
      return;
    }

    /* ------------------------------------------------------------------ */

    /* Return the sparse Jacobian and gradient of either the objective
     * function or the Lagrangian.
     * Usage:  [g,J] = cutest_slagjac(x) or [g,J] = cutest_slagjac(x,v)
     */
    if (strcmp(toolName, "slagjac") == 0) {

      if (CUTEst_ncon == 0) {
        sprintf(msgBuf, onlyConst, toolName);
        mexWarnMsgTxt(msgBuf);
      }
      if (nrhs < 2 || nrhs > 3)
        mexErrMsgTxt("cutest_slagjac: Please specify x and possibly v\n");
      if (nlhs != 2)
        mexErrMsgTxt("cutest_slagjac: Two output arguments returned\n");

      if (! mxIsDouble(prhs[1]))
        mexErrMsgTxt("cutest_slagjac: Input array must be of type double\n");

      if (nrhs == 3)
        if (! mxIsDouble(prhs[2]))
          mexErrMsgTxt("cutest_slagjac: Input array must be of type double\n");

      if (CUTEst_dertype < 1) mexErrMsgTxt("cutest_slagjac: cutest_setup must be called with derivative type set to 1 or 2\n");


      x = (doublereal *)mxGetData(prhs[1]);
      if (nrhs == 3)
        v = (doublereal *)mxGetData(prhs[2]);

      /* Reserve room to hold J and one gradient. Here, CUTEst_nnzj
       * contains the number of nonzeros in J, __not counting__ the extra
       * gradient, as in some CUTEst subroutines.
       */
      /* This must be improved ! */
      nnzjplusn = CUTEst_nnzj + CUTEst_nvar;
      lj = nnzjplusn ;
      J = (doublereal *)mxCalloc(nnzjplusn, sizeof(doublereal));
      irow = (integer *)mxCalloc(nnzjplusn, sizeof(integer));
      jcol = (integer *)mxCalloc(nnzjplusn, sizeof(integer));

      if (nrhs == 2)
        CUTEST_csgr( &status, &CUTEst_nvar, &CUTEst_ncon, x, NULL,
          &somethingFalse, &nnzjplusn, &lj, J, jcol, irow);
      else
        CUTEST_csgr( &status, &CUTEst_nvar, &CUTEst_ncon, x, v,
          &somethingTrue, &nnzjplusn, &lj, J, jcol, irow);
      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }

      /* Extract the gradient from J. Its components have irow[i]=0 */
      plhs[0] = extractSparseVector(CUTEst_ncon, CUTEst_nvar,
                                    nnzjplusn, nnzjplusn - CUTEst_nnzj,
                                    irow, jcol, (double *)J);

      /* nnzjplusn was overwritten with the actual number of nonzeros
       * in the "augmented" matrix [J' g].
       * Extract Jacobian matrix from J.
       */
      plhs[1] = coordToMatlabSparse(CUTEst_ncon, CUTEst_nvar, CUTEst_nnzj,
                                    irow, jcol, (double *)J);

      mxFree(jcol);
      mxFree(irow);
      mxFree(J);

      mxFree((void *)toolName);
      return;
    }

    /* ------------------------------------------------------------------ */

    /* Return the product of the Jacobian at x with a vector p.
     * Usage:  r = cutest_Jprod(x, p)  recomputes J(x)
     *         r = cutest_Jprod(p)     assumes J(x) has been computed previously.
     */

    if (strcmp(toolName, "Jprod") == 0) {

      if (CUTEst_ncon == 0) {
        sprintf(msgBuf, onlyConst, toolName);
        mexWarnMsgTxt(msgBuf);
      }
      if (nrhs < 2 || nrhs > 3)
        mexErrMsgTxt("cutest_Jprod: Please specify x if J(x) should be recomputed and vector p\n");

      if (nlhs != 1)
        mexErrMsgTxt("cutest_Jprod: A single output argument is returned\n");

      if (! mxIsDouble(prhs[1]))
        mexErrMsgTxt("cutest_Jprod: Input array must be of type double\n");

      if (mxGetN(prhs[1]) != 1 || mxGetM(prhs[1]) != CUTEst_nvar) {
        sprintf(msgBuf, "cutest_Jprod: input must be %-d X 1", CUTEst_nvar);
        mexErrMsgTxt(msgBuf);
      }

      if (nrhs == 2)    /* p is the only input argument */
        p = (doublereal *)mxGetData(prhs[1]);
      else {
        if (! mxIsDouble(prhs[2]))
          mexErrMsgTxt("cutest_Jprod: Input array must be of type double\n");
        if (mxGetN(prhs[2]) != 1 || mxGetM(prhs[2]) != CUTEst_nvar) {
          sprintf(msgBuf, "cutest_Jprod: input must be %-d X 1", CUTEst_nvar);
          mexErrMsgTxt(msgBuf);
        }

        x = (doublereal *)mxGetData(prhs[1]);
        p = (doublereal *)mxGetData(prhs[2]);
      }

      plhs[0] = mxCreateDoubleMatrix(CUTEst_ncon, 1, mxREAL);
      r = (doublereal *)mxGetData(plhs[0]);

      if (nrhs == 2)    /* Assume J(x) has been computed previously */
        CUTEST_cjprod( &status, &CUTEst_nvar, &CUTEst_ncon, &somethingTrue,
                &somethingFalse, NULL, p, &CUTEst_nvar, r, &CUTEst_ncon);
      else               /* Recompute J(x) */
        CUTEST_cjprod( &status, &CUTEst_nvar, &CUTEst_ncon, &somethingFalse,
                &somethingFalse, x, p, &CUTEst_nvar, r, &CUTEst_ncon);
      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }

      mxFree((void *)toolName);
      return;
    }

    /* ------------------------------------------------------------------ */

    /* Return the product of the transpose Jacobian at x with a vector p.
     * Usage:  r = cutest_Jtprod(x, p) recomputes J(x)
     *         r = cutest_Jtprod(p) assumes J(x) has been computed previously.
     */

    if (strcmp(toolName, "Jtprod") == 0) {

      if (CUTEst_ncon == 0) {
        sprintf(msgBuf, onlyConst, toolName);
        mexWarnMsgTxt(msgBuf);
      }
      if (nrhs < 2 || nrhs > 3)
        mexErrMsgTxt("cutest_Jtprod: Please specify x if J(x) should be recomputed and vector p\n");

      if (nlhs != 1)
        mexErrMsgTxt("cutest_Jtprod: A single output argument is returned\n");

      if (! mxIsDouble(prhs[1]))
        mexErrMsgTxt("cutest_Jtprod: Input array must be of type double\n");

      if (nrhs == 2) {   /* p is the only input argument */
        if (mxGetN(prhs[1]) != 1 || mxGetM(prhs[1]) != CUTEst_ncon) {
          sprintf(msgBuf,"cutest_Jtprod: input must be %-d X 1", CUTEst_ncon);
          mexErrMsgTxt(msgBuf);
        }
        p = (doublereal *)mxGetData(prhs[1]);
      } else {
        if (! mxIsDouble(prhs[2]))
          mexErrMsgTxt("cutest_Jtprod: Input array must be of type double\n");
        if (mxGetN(prhs[1]) != 1 || mxGetM(prhs[1]) != CUTEst_nvar) {
          sprintf(msgBuf, "cutest_Jtprod: input must be %-d X 1", CUTEst_nvar);
          mexErrMsgTxt(msgBuf);
        }
        if (mxGetN(prhs[2]) != 1 || mxGetM(prhs[2]) != CUTEst_ncon) {
          sprintf(msgBuf, "cutest_Jtprod: input must be %-d X 1", CUTEst_ncon);
          mexErrMsgTxt(msgBuf);
        }

        x = (doublereal *)mxGetData(prhs[1]);
        p = (doublereal *)mxGetData(prhs[2]);
      }

      plhs[0] = mxCreateDoubleMatrix(CUTEst_nvar, 1, mxREAL);
      r = (doublereal *)mxGetData(plhs[0]);

      if (nrhs == 2)    /* Assume J(x) has been computed previously */
        CUTEST_cjprod( &status, &CUTEst_nvar, &CUTEst_ncon, &somethingTrue,
                &somethingTrue, NULL, p, &CUTEst_ncon, r, &CUTEst_nvar);
      else               /* Recompute J(x) */
        CUTEST_cjprod( &status, &CUTEst_nvar, &CUTEst_ncon, &somethingFalse,
                &somethingTrue, x, p, &CUTEst_ncon, r, &CUTEst_nvar);
      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }

      mxFree((void *)toolName);
      return;
    }

    /* ============== Dense second derivative tools ===================== */

    /* Return the dense Hessian of the objective function if the problem is
     * unconstrained or of the Lagrangian if the problem is constrained.
     * If the problem is constrained and the user wants the Hessian of the
     * objective, they should call (sp)hess().
     * Usage:  H = cutest_hess(x) if the problem has no general constraints, or
     *         H = cutest_hess(x, v) otherwise.
     */
    if (strcmp(toolName, "hess") == 0) {

      if (CUTEst_ncon > 0) {
        if (nrhs != 3)
          mexErrMsgTxt("cutest_hess: Specify primal and dual variables\n");
        if (! mxIsDouble(prhs[1]))
          mexErrMsgTxt("cutest_hess: Input array must have type double\n");
        if (mxGetNumberOfElements(prhs[1]) != CUTEst_nvar)
          mexErrMsgTxt("cutest_hess: Input array has erroneous size\n");
        x = (doublereal *)mxGetData(prhs[1]);

        if (mxGetNumberOfElements(prhs[2]) != CUTEst_ncon)
          mexErrMsgTxt("cutest_hess: Input array has erroneous size\n");
        v = (doublereal *)mxGetData(prhs[2]);
      } else {
        if (nrhs != 2)
          mexErrMsgTxt("cutest_hess: Specify primal variables only\n");
        if (! mxIsDouble(prhs[1]))
          mexErrMsgTxt("cutest_hess: Input array must have type double\n");
        if (mxGetNumberOfElements(prhs[1]) != CUTEst_nvar)
          mexErrMsgTxt("cutest_hess: Input array has erroneous size\n");
        x = (doublereal *)mxGetData(prhs[1]);
      }

      if (nlhs != 1) mexErrMsgTxt("cutest_hess: Need single output argument\n");

      plhs[0] = mxCreateDoubleMatrix(CUTEst_nvar, CUTEst_nvar, mxREAL);
      H = (doublereal *)mxGetData(plhs[0]);

      if (CUTEst_ncon > 0)
        CUTEST_cdh( &status, &CUTEst_nvar, &CUTEst_ncon, x, v,
             &CUTEst_nvar, H);
      else
        CUTEST_udh( &status, &CUTEst_nvar, x, &CUTEst_nvar, H);

      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }

      mxFree((void *)toolName);
      return;
    }

    /* ------------------------------------------------------------------ */

    /* Return the dense Hessian of the objective or of a constraint. The
     * function index is ignored if the problem is unconstrained.
     * Usage:  Hi = cutest_ihess(x, i).
     */
    if (strcmp(toolName, "ihess") == 0) {

      if (nrhs != 3)
        mexErrMsgTxt("cutest_ihess: Specify x and index\n");
      if (! mxIsDouble(prhs[1]))
        mexErrMsgTxt("cutest_ihess: Input array must have type double\n");

      if (! isInteger(prhs[2]) && ! mxIsDouble(prhs[2]))
        mexErrMsgTxt("cutest_ihess: Index must be integer\n");

      if (isInteger(prhs[2])) {
        icon_ptr = (integer *)mxGetData(prhs[2]);
        icon = icon_ptr[0];
      } else
        icon = (integer)*mxGetPr(prhs[2]);

      if (CUTEst_ncon > 0 && (icon < 0 || icon > CUTEst_ncon))
        mexErrMsgTxt("cutest_ihess: Index out of range\n");

      if (nlhs != 1) mexErrMsgTxt("cutest_ihess: Need single output argument\n");

      x = (doublereal *)mxGetData(prhs[1]);
      plhs[0] = mxCreateDoubleMatrix(CUTEst_nvar, CUTEst_nvar, mxREAL);
      H = (doublereal *)mxGetData(plhs[0]);

      if (CUTEst_ncon > 0)
        CUTEST_cidh( &status, &CUTEst_nvar, x, &icon, &CUTEst_nvar, H);
      else
        CUTEST_udh( &status, &CUTEst_nvar, x, &CUTEst_nvar, H);

      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }

      mxFree((void *)toolName);
      return;
    }

    /* ------------------------------------------------------------------ */

    /* Return the matrix-vector product between the Hessian of the
     * Lagrangian (or of the objective if problem is unconstrained) and a
     * given vector p
     * Usage:  r = cutest_hprod(x, v, p)   (Re)computes the Hessian at (x,v)
     *         r = cutest_hprod(x, p)    Same, for unconstrained problems
     *         r = cutest_hprod(p)       assumes H(x,v) was computed previously
     */
    if (strcmp(toolName, "hprod") == 0) {

      if (nrhs > 4)
        mexErrMsgTxt("cutest_hprod: Too many arguments\n");
      for (i = 1; i < nrhs; i++)
        if (! mxIsDouble(prhs[i]))
          mexErrMsgTxt("cutest_hprod: Input array must have type double\n");
      if (nrhs == 2)   /* Only p is given as argument */
        p = (doublereal *)mxGetData(prhs[1]);
      else if (nrhs == 3) { /* Arguments are (x,p) and ncon = 0 */
        if (CUTEst_ncon > 0)
          mexErrMsgTxt("hprod: Please specify multiplierss\n");
        x = (doublereal *)mxGetData(prhs[1]);
        p = (doublereal *)mxGetData(prhs[2]);
      } else {               /* Arguments are (x,v,p) and ncon > 0 */
        if (CUTEst_ncon == 0)
          mexErrMsgTxt("hprod: Problem is unconstrained and you specified multipliers\n");
        x = (doublereal *)mxGetData(prhs[1]);
        v = (doublereal *)mxGetData(prhs[2]);
        p = (doublereal *)mxGetData(prhs[3]);
      }
      if (nlhs != 1) mexErrMsgTxt("cutest_hprod: Need single output argument\n");

      plhs[0] = mxCreateDoubleMatrix(CUTEst_nvar, 1, mxREAL);
      r = (doublereal *)mxGetData(plhs[0]);

      /* Call the appropriate matrix-vector product subroutine */
      if (nrhs == 2) {
        if (CUTEst_ncon > 0)
          CUTEST_chprod( &status, &CUTEst_nvar, &CUTEst_ncon, &somethingTrue, NULL,
                 NULL, p, r);
        else
          CUTEST_uhprod( &status, &CUTEst_nvar, &somethingTrue, NULL, p, r);
      } else if (nrhs == 3)
        CUTEST_uhprod( &status, &CUTEst_nvar, &somethingFalse, x, p, r);
      else
        CUTEST_chprod( &status, &CUTEst_nvar, &CUTEst_ncon, &somethingFalse, x,
               v, p, r);

      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }

      mxFree((void *)toolName);
      return;
    }

    /* ------------------------------------------------------------------ */

    /* Return the Hessian of the Lagrangian, the Jacobian of the constraints
     * and the gradient of either the objective function or the Lagrangian
     * Usage:  [g,H] = cutest_gradhess(x)   if the problem is unconstrained, or
     *       [g,J,H] = cutest_gradHess(x, v, gradf, jtrans) if it is constrained
     */
    if (strcmp(toolName, "gradhess") == 0) {

      if (nrhs > 5)
        mexErrMsgTxt("cutest_gradhess: Expected at most 4 arguments\n");

      if (!mxIsDouble(prhs[1]))
        mexErrMsgTxt("cutest_gradhess: Input array x must be double\n");

      /* If problem is unconstrained, ignore arguments v, gradf, jtrans */
      x = (doublereal *)mxGetData(prhs[1]);

      if (CUTEst_ncon > 0) {
        if (nrhs != 5)
          mexErrMsgTxt("cutest_gradhess: Specify x, v, gradf, jtrans\n");
        if (!mxIsDouble(prhs[2]))
          mexErrMsgTxt("cutest_gradhess: Input array v must be double\n");
        v = (doublereal *)mxGetData(prhs[2]);

        if (!mxIsLogical(prhs[3]))
          mexErrMsgTxt("cutest_gradhess: Input gradf must be logical\n");
        gradfptr = mxGetLogicals(prhs[3]);
        gradf = (logical)gradfptr[0];

        if (!mxIsLogical(prhs[4]))
          mexErrMsgTxt("cutest_gradhess: Input jtrans must be logical\n");
        jtransptr = mxGetLogicals(prhs[4]);
        jtrans = (logical)jtransptr[0];

      if (nlhs < 1 || nlhs > 3)
          mexErrMsgTxt("cutest_gradhess: Need 2 or 3 output arguments\n");

        plhs[0] = mxCreateDoubleMatrix(CUTEst_nvar, 1, mxREAL);
        g = (doublereal *)mxGetData(plhs[0]);
        if (jtrans)
          plhs[1] = mxCreateDoubleMatrix(CUTEst_nvar, CUTEst_ncon,
                                          mxREAL);
        else
          plhs[1] = mxCreateDoubleMatrix(CUTEst_ncon, CUTEst_nvar,
                                          mxREAL);
        J = (doublereal *)mxGetData(plhs[1]);
        plhs[2] = mxCreateDoubleMatrix(CUTEst_nvar, CUTEst_nvar, mxREAL);
        H = (doublereal *)mxGetData(plhs[2]);

#ifdef MXDEBUG
        mexPrintf("cutest_gradhess: using gradf=%-d, jtrans=%-d\n",
                   gradf, jtrans);
#endif

        if (jtrans)
          CUTEST_cgrdh( &status, &CUTEst_nvar, &CUTEst_ncon, x, v,
                        &gradf, g, &jtrans, &CUTEst_nvar, &CUTEst_ncon, J,
                        &CUTEst_nvar, H);
        else
          CUTEST_cgrdh( &status, &CUTEst_nvar, &CUTEst_ncon, x, v,
                        &gradf, g, &jtrans, &CUTEst_ncon, &CUTEst_nvar, J,
                        &CUTEst_nvar, H);
      } else {
        plhs[0] = mxCreateDoubleMatrix(CUTEst_nvar, 1, mxREAL);
        g = (doublereal *)mxGetData(plhs[0]);
        plhs[1] = mxCreateDoubleMatrix(CUTEst_nvar, CUTEst_nvar, mxREAL);
        H = (doublereal *)mxGetData(plhs[1]);

        CUTEST_ugrdh( &status, &CUTEst_nvar, x, g, &CUTEst_nvar, H);
      }

      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }

      mxFree((void *)toolName);
      return;
    }

    /* ============== Sparse second derivative tools ==================== */

    /* Return the sparse Hessian of the objective function if the problem is
     * unconstrained or of the Lagrangian if the problem is constrained.
     * If the problem is constrained and the user wants the Hessian of the
     * objective, they should call cutest_(sp)ihess().
     * Usage:  H = cutest_sphess(x) if the problem has no general constraints, or
     *         H = cutest_sphess(x, v) otherwise.
     */
    if (strcmp(toolName, "sphess") == 0) {

      if (CUTEst_ncon > 0) {
        if (nrhs != 3)
          mexErrMsgTxt("cutest_hess: Specify primal and dual variables\n");
        if (! mxIsDouble(prhs[1]) || ! mxIsDouble(prhs[2]))
          mexErrMsgTxt("cutest_hess: Input arrays must have type double\n");
        x = (doublereal *)mxGetData(prhs[1]);
        v = (doublereal *)mxGetData(prhs[2]);
      } else {
        /* If dual variables are specified, ignore them. */
        if (! mxIsDouble(prhs[1]))
          mexErrMsgTxt("cutest_hess: Input array must have type double\n");
        x = (doublereal *)mxGetData(prhs[1]);
      }
      if (nlhs != 1) mexErrMsgTxt("cutest_sphess: Need single output argument\n");

      if (CUTEst_dertype < 2) mexErrMsgTxt("cutest_sphess: cutest_setup must be called with derivative type set to 2\n");

      /* Make enough room for the full Hessian (both triangles) */
      H = (doublereal *)mxCalloc(2*CUTEst_nnzh, sizeof(doublereal));
      irow = (integer *)mxCalloc(2*CUTEst_nnzh, sizeof(integer));
      jcol = (integer *)mxCalloc(2*CUTEst_nnzh, sizeof(integer));
      /*      H = (doublereal *)mxCalloc(16, sizeof(doublereal));
      irow = (integer *)mxCalloc(16, sizeof(integer));
      jcol = (integer *)mxCalloc(16, sizeof(integer)); */

      /* Pretend only one triangle was allocated */
      if (CUTEst_ncon > 0)
        CUTEST_csh( &status, &CUTEst_nvar, &CUTEst_ncon, x, v,
             &CUTEst_nnzh, &CUTEst_nnzh, H, irow, jcol);
      else
        CUTEST_ush( &status, &CUTEst_nvar, x,
             &CUTEst_nnzh, &CUTEst_nnzh, H, irow, jcol);

      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }

      /* Expand missing triangle ; do not duplicate diagonal */
      offdiag_nnzh = 0;
      for (i = 0; i < CUTEst_nnzh; i++)
        if (irow[i] != jcol[i]) {
          irow[CUTEst_nnzh + offdiag_nnzh] = jcol[i];
          jcol[CUTEst_nnzh + offdiag_nnzh] = irow[i];
          H[CUTEst_nnzh + offdiag_nnzh] = H[i];
          offdiag_nnzh++;
        }

      /* i =  CUTEst_nnzh + offdiag_nnzh ; */
      /* sprintf(msgBuf,"** nnzh = %d", CUTEst_nnzh + offdiag_nnzh);*/

      /* mexPrintf(" nnzh total %-d allocated %-d\n", i, 2*CUTEst_nnzh);
for (i = 0; i < CUTEst_nnzh +  offdiag_nnzh ; i++)
mexPrintf("%-2d row,col,val %-d %-d %f \n ", i+1,irow[i],jcol[i],H[i]); */

/*  this fails if 13 is changed to 14 ... or CUTEst_nnzh + offdiag_nnzh = 16 */
/*      plhs[0] = coordToMatlabSparse(CUTEst_nvar, CUTEst_nvar, */
/*      matrix = coordToMatlabSparse(CUTEst_nvar, CUTEst_nvar,
                                    16,
                                    irow, jcol,
                                    (double *)H);
mexErrMsgTxt("stop\n");
*/

      plhs[0] = coordToMatlabSparse(CUTEst_nvar, CUTEst_nvar,
                                    CUTEst_nnzh + offdiag_nnzh,
                                    irow, jcol,
                                    (double *)H);
      mxFree(jcol);
      mxFree(irow);
      mxFree(H);

      mxFree((void *)toolName);
      return;
    }

    /* ------------------------------------------------------------------ */

    /* Return the sparse Hessian of the objective or of a constraint. The
     * function index is ignored if the problem is unconstrained.
     * Usage:  Hi = cutest_isphess(x, i).
     */
    if (strcmp(toolName, "isphess") == 0) {

      if (nrhs != 3)
        mexErrMsgTxt("cutest_isphess: Specify x and index\n");
      if (! mxIsDouble(prhs[1]))
        mexErrMsgTxt("cutest_isphess: Input array must have type double\n");

      if (! isInteger(prhs[2]) && ! mxIsDouble(prhs[2]))
        mexErrMsgTxt("cutest_isphess: Index must be integer\n");

      if (isInteger(prhs[2])) {
        icon_ptr = (integer *)mxGetData(prhs[2]);
        icon = icon_ptr[0];
      } else
        icon = (integer)*mxGetPr(prhs[2]);

      if (nlhs != 1) mexErrMsgTxt("cutest_isphess: Need single output argument\n");

      if (CUTEst_ncon > 0 && (icon < 0 || icon > CUTEst_ncon))
        mexErrMsgTxt("cutest_isphess: Index out of range\n");

      if (CUTEst_dertype < 2) mexErrMsgTxt("cutest_isphess: setup must be called with derivative type set to 2\n");

      x = (doublereal *)mxGetData(prhs[1]);

      /* Make enough room for the full Hessian (both triangles) */
      /* This must be improved by computing nnzhi ! */
      nnzh2 = 2*CUTEst_nnzh;
      H = (doublereal *)mxCalloc(nnzh2, sizeof(doublereal));
      irow = (integer *)mxCalloc(nnzh2, sizeof(integer));
      jcol = (integer *)mxCalloc(nnzh2, sizeof(integer));

      if (CUTEst_ncon > 0)
        CUTEST_cish( &status, &CUTEst_nvar, x, &icon,
                     &nnzhi, &CUTEst_nnzh, H, irow, jcol);
      else
        CUTEST_ush( &status, &CUTEst_nvar, x,
                    &nnzhi, &CUTEst_nnzh, H, irow, jcol);

      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
        }

      /* Expand missing triangle ; do not duplicate diagonal */
      offdiag_nnzh = 0;
      for (i = 0; i < nnzhi; i++)
        if (irow[i] != jcol[i]) {
          irow[nnzhi + offdiag_nnzh] = jcol[i];
          jcol[nnzhi + offdiag_nnzh] = irow[i];
          H[nnzhi + offdiag_nnzh] = H[i];
          offdiag_nnzh++;
        }

      /* Convert to Matlab Sparse format */
      plhs[0] = coordToMatlabSparse(CUTEst_nvar, CUTEst_nvar,
                                    nnzhi + offdiag_nnzh,
                                    irow, jcol,
                                    (double *)H);

      mxFree(jcol);
      mxFree(irow);
      mxFree(H);

      mxFree((void *)toolName);
      return;
    }

    /* ------------------------------------------------------------------ */

    /* Return the sparse Hessian of the Lagrangian, the sparse Jacobian of
     * the constraints and the gradient of either the objective function or
     * the Lagrangian
     * Usage: [g,H] = cutest_gradsphess(x)   if the problem is unconstrained, or
     *      [g,J,H] = cutest_gradsphess(x, v, gradf)  if it is constrained
     */
    if (strcmp(toolName, "gradsphess") == 0) {

      if (nrhs > 4)
        mexErrMsgTxt("cutest_gradsphess: Expected at most 3 arguments\n");

      if (!mxIsDouble(prhs[1]))
        mexErrMsgTxt("cutest_gradsphess: Input array x must be double\n");

      if (nlhs < 1 || nlhs > 3)
          mexErrMsgTxt("cutest_gradhess: Need 2 or 3 output arguments\n");

      if (CUTEst_dertype < 1) mexErrMsgTxt("cutest_gradsphess: cutest_setup must be called with derivative type set to 1 or 2\n");

      /* If problem is unconstrained, ignore arguments v, gradf, jtrans */
      x = (doublereal *)mxGetData(prhs[1]);

      if (CUTEst_ncon > 0) {

        /* Constrained problems */
        if (nrhs != 4)
          mexErrMsgTxt("cutest_gradsphess: Specify x, v, gradf\n");
        if (!mxIsDouble(prhs[2]))
          mexErrMsgTxt("cutest_gradsphess: Input array v must be double\n");
        v = (doublereal *)mxGetData(prhs[2]);

        if (!mxIsLogical(prhs[3]))
          mexErrMsgTxt("cutest_gradsphess: Input gradf must be logical\n");
        gradfptr = mxGetLogicals(prhs[3]);
        gradf = (logical)gradfptr[0];

#ifdef MXDEBUG
        mexPrintf("cutest_gradsphess: using gradf=%-d\n", gradf);
#endif

        /* Make enough room for full Hessian (both triangles) */
        irow = (integer *)mxCalloc(2*CUTEst_nnzh, sizeof(integer));
        jcol = (integer *)mxCalloc(2*CUTEst_nnzh, sizeof(integer));
        H = (doublereal *)mxCalloc(2*CUTEst_nnzh, sizeof(doublereal));

        /* Make room for Jacobian and sparse vector */
        /* This must be improved ! */
        nnzjplusn = CUTEst_nnzj + CUTEst_nvar;
        lj = nnzjplusn ;
        irow2 = (integer *)mxCalloc(nnzjplusn, sizeof(integer));
        jcol2 = (integer *)mxCalloc(nnzjplusn, sizeof(integer));
        J = (doublereal *)mxCalloc(nnzjplusn, sizeof(doublereal));

        /* Pretend only one triangle of H was allocated */
        CUTEST_csgrsh( &status, &CUTEst_nvar, &CUTEst_ncon, x, v, &gradf,
                &nnzjplusn, &lj, J, jcol2, irow2, &CUTEst_nnzh,
                &CUTEst_nnzh, H, irow, jcol);
        if (status != 0) {
            sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
            mexErrMsgTxt(msgBuf);
          }

        /* nnzjplusn was overwritten with the actual number of nonzeros
         * in the "augmented" matrix [J' g].
         * Extract Jacobian matrix from J.
         */
        plhs[1] = coordToMatlabSparse(CUTEst_ncon, CUTEst_nvar,
                                      CUTEst_nnzj, irow2,
                                      jcol2, (double *)J);

        /* Extract the gradient from J. Its components have irow[i]=0 */
        plhs[0] = extractSparseVector(CUTEst_ncon, CUTEst_nvar,
                                      nnzjplusn, nnzjplusn-CUTEst_nnzj,
                                      irow2, jcol2,
                                      (double *)J);

        mxFree(jcol2);
        mxFree(irow2);
        mxFree(J);

      } else {

        /* Unconstrained problems */
        plhs[0] = mxCreateDoubleMatrix(CUTEst_nvar, 1, mxREAL);
        g = (doublereal *)mxGetData(plhs[0]);

        /* Make enough room for full Hessian (both triangles) */
        irow = (integer *)mxCalloc(2*CUTEst_nnzh, sizeof(integer));
        jcol = (integer *)mxCalloc(2*CUTEst_nnzh, sizeof(integer));
        H = (doublereal *)mxCalloc(2*CUTEst_nnzh, sizeof(doublereal));

        /* Pretend only one triangle was allocated */
        CUTEST_ugrsh( &status, &CUTEst_nvar, x, g, &CUTEst_nnzh, &CUTEst_nnzh,
               H, irow, jcol);
        if (status != 0) {
            sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
            mexErrMsgTxt(msgBuf);
          }
      }

      /* Expand missing triangle of H ; do not duplicate diagonal */
      offdiag_nnzh = 0;
      for (i = 0; i < CUTEst_nnzh; i++)
        if (irow[i] != jcol[i]) {
          irow[CUTEst_nnzh + offdiag_nnzh] = jcol[i];
          jcol[CUTEst_nnzh + offdiag_nnzh] = irow[i];
          H[CUTEst_nnzh + offdiag_nnzh] = H[i];
          offdiag_nnzh++;
        }

      /* Convert to Matlab Sparse format */
      if (CUTEst_ncon > 0)
        plhs[2] = coordToMatlabSparse(CUTEst_nvar, CUTEst_nvar,
                                      CUTEst_nnzh + offdiag_nnzh,
                                      irow, jcol,
                                      (double *)H);
      else
        plhs[1] = coordToMatlabSparse(CUTEst_nvar, CUTEst_nvar,
                                      CUTEst_nnzh + offdiag_nnzh,
                                      irow, jcol,
                                      (double *)H);

      mxFree(jcol);
      mxFree(irow);
      mxFree(H);

      mxFree((void *)toolName);
      return;
    }

    if (strcmp(toolName, "terminate") == 0) {

      if (nlhs != 0) mexErrMsgTxt("terminate returns no output\n");
      if (nrhs > 1)
        mexWarnMsgTxt("terminate does not take input arguments\n");

      if (CUTEst_ncon > 0)
        CUTEST_cterminate( &status );
      else
        CUTEST_uterminate( &status );

      if (status != 0) {
          sprintf(msgBuf,"** CUTEst error, status = %d, aborting\n", status);
          mexErrMsgTxt(msgBuf);
      }
      setupCalled = 0;
      mxFree((void *)toolName);
      return;
    }

    sprintf(msgBuf, "Tool name %-s not recognized\n", toolName);
    mexErrMsgTxt(msgBuf);
    mxFree((void *)toolName);
  }

  /* -------------------------------------------------------------------------- */
  /* Helper functions */

  /* Convert a sparse vector to sparse matlab format.
   */
  mxArray *SparseVector(int n, int nnz, integer *index, double *val) {

    mxArray *vector;    /* Output sparse vector as Matlab sparse matrix */
    mwIndex *ir, *jptr; /* Index arrays of output vector */
    double  *pr;        /* Value array of output vector */

    int i, nnzActual = 0;

    if (nnz < 0) return NULL;

    vector = mxCreateSparse(n, 1, nnz, mxREAL);
    if (vector == NULL) return NULL;
    ir = (mwIndex *)mxGetIr(vector);
    jptr = (mwIndex *)mxGetJc(vector);
    pr = mxGetPr(vector);

    for (i = 0; i < nnz; i++) {
        ir[nnzActual] = (mwIndex)(index[i] - 1);  /* Indices are 0-based */
        pr[nnzActual] = val[i];
        nnzActual++;
    }
    jptr[0] = (mwIndex)0;
    jptr[1] = (mwIndex)nnzActual;

#ifdef MXDEBUG
    mexPrintf("Sparse vector has %-d nonzeros\n", nnzActual);
#endif

    return vector;
  }

  /* Extract a sparse vector from a CUTEst sparse matrix. The components
   * of the sparse vector are such that irow[i]=0. The rest of the
   * sparse matrix should be extracted with coordToMatlabSparse().
   */
  mxArray *extractSparseVector(int nrow, int ncol, int nnz, int nnzV,
                               integer *irow, integer *jcol, double *val) {

    mxArray *vector;    /* Output sparse vector as Matlab sparse matrix */
    mwIndex *ir, *jptr; /* Index arrays of output vector */
    double  *pr;        /* Value array of output vector */

    int i, nnzActual = 0;

    if (nnz < 0 || nnzV < 0) return NULL;

    /* The nnzV given by the user may be an overestimate */
    vector = mxCreateSparse(ncol, 1, nnzV, mxREAL);
    if (vector == NULL) return NULL;
    ir = (mwIndex *)mxGetIr(vector);
    jptr = (mwIndex *)mxGetJc(vector);
    pr = mxGetPr(vector);

    for (i = 0; i < nnz; i++)
      if (irow[i] == 0) {      /* Component belongs to sparse vector */
        ir[nnzActual] = (mwIndex)(jcol[i] - 1);  /* Indices are 0-based */
        pr[nnzActual] = val[i];
        nnzActual++;
      }
    jptr[0] = (mwIndex)0;
    jptr[1] = (mwIndex)nnzActual;

    /* Sort entries */
    if (jptr[0] > jptr[1] - 1)
      quicksort_cutest(ir, (double *)pr, jptr[0], jptr[1] - 1);

#ifdef MXDEBUG
    mexPrintf("Sparse vector has %-d nonzeros\n", nnzActual);
#endif

    return vector;
  }

  /* Convert a sparse matrix in coordinate format to Matlab format.
   * Some CUTEst sparse matrices contain a matrix and a sparse vector.
   * Components of the sparse vector have irow[i]=0. This function
   * ignores the sparse vector, which should be extracted by calling
   * extractSparseVector().
   */
  mxArray *coordToMatlabSparse(int nrow, int ncol, int nnz,
                               integer *irow, integer *jcol, double *val) {

    mxArray *matrix;    /* Output Matlab sparse matrix */
    mwIndex *ir, *jptr; /* Index arrays of output matrix */
    double *pr;         /* Value array of output matrix */

    double zero = (double)0.0;
    int i, j, k, elem;

    /*        mexPrintf("in %-d\n", nnz); */

    if (nnz < 0) return NULL;

    matrix = mxCreateSparse((mwSize)nrow, (mwSize)ncol, (mwSize)nnz, mxREAL);
    if (matrix == NULL) return NULL;

    ir   = (mwIndex *)mxGetIr(matrix);  /* Array of length nnz    */
    jptr = (mwIndex *)mxGetJc(matrix);  /* Array of length ncol+1 */
    pr   = mxGetPr(matrix);  /* Array of length nnz    */

    /* Store the number of nonzeros in each column */
    for (k = 0; k < nnz; k++)
      if (irow[k] > 0) {    /* Ignore sparse vector */
        j = jcol[k] - 1;     /* There is a nonzero in column j */
        jptr[j]++;           /* Keep track of it */
      }
    jptr[ncol] = (mwIndex)nnz;

    /* Go backwards through jptr to find the row index of the first
     * nonzero in each column. */
    for (j = ncol-1; j >= 0; j--)
      jptr[j] = jptr[j+1] - jptr[j];

    /* Copy entries. Make everything 0-based. */
    for (k = 0; k < nnz; k++)
      if (irow[k] > 0) {    /* Ignore sparse vector */
        j = jcol[k] - 1;
        elem = jptr[j];
        pr[elem] = val[k];
        ir[elem] = (mwIndex)(irow[k] - 1);
        jptr[j] = (mwIndex)(elem + 1);
      }

    /* Restore jptr */
    for (j = ncol; j >= 1; j--) jptr[j] = jptr[j-1];
    jptr[0] = (mwIndex)0;

    /* Sort each segment of ir in ascending order (a silly Matlab thing).
     * Keep each segment of pr synchronized. Not sorting row indices
     * causes bugs and eventually deadly crashes in Matlab. */

    for (j = 0; j < ncol; j++)
      /* sort row indices in column j if it is nonempty */
      if ( jptr[j]+1 < jptr[j+1])
        quicksort_cutest(ir, (double*)pr, jptr[j], jptr[j+1]-1);

    return matrix;
  }

  /* Sorting function, used to create Matlab sparse matrices */

  void quicksortFollow(mwIndex x[], double follower[],
                       mwIndex first, mwIndex last) {
    int pivIndex = 0, i;
    if (first < last) {
      pivIndex = partition(x, follower, first, last);
      quicksortFollow(x, follower, first, pivIndex-1);
      quicksortFollow(x, follower, pivIndex+1, last);
    }
  }

  int partition(mwIndex y[], double follower[], mwIndex f, mwIndex l) {
    mwIndex up, down;
    mwIndex piv = y[f];
    double dpiv = follower[f];
    up = f;
    down = l;
    goto partLS;
    do {
      swap(y, follower, up, down);
    partLS:
      while(y[up] <= piv && up < l) up++;
      while(y[down] > piv  && down > f) down--;
    } while(down > up);
    y[f] = y[down];   follower[f] = follower[down];
    y[down] = piv;    follower[down] = dpiv;
    return down;
  }

  void swap(mwIndex y[], double follower[], mwIndex el1, mwIndex el2) {
    mwIndex tmp = y[el1];
    double dtmp = follower[el1];
    y[el1] = y[el2];
    y[el2] = tmp;
    follower[el1] = follower[el2];
    follower[el2] = dtmp;
    return;
  }

  void quicksort_cutest(mwIndex numbers[], double values[],
                        mwIndex low, mwIndex up) {
    int current, low_current, up_current;
    double dcurrent;

    low_current = low;
    up_current = up;
    current = numbers[low];
    dcurrent = values[low];
    while (low < up)
    {
      while ((numbers[up] >= current) && (low < up))
        up--;
      if (low != up)
      {
        numbers[low] = numbers[up];
        values[low] = values[up];
        low++;
      }
      while ((numbers[low] <= current) && (low < up))
        low++;
      if (low != up)
      {
        numbers[up] = numbers[low];
        values[up] = values[low];
        up--;
      }
    }
    numbers[low] = current;
    values[low] = dcurrent;
    current = low;
    low = low_current;
    up = up_current;
    if (low < current)
      quicksort_cutest(numbers, values, low, current-1);
    if (up > current)
      quicksort_cutest(numbers, values, current+1, up);
  }

#ifdef __cplusplus
}
#endif

