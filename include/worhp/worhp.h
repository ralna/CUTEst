/*
 * This is the the central WORHP C/C++ header.
 * Include this to make all documented WORHP functions visible to your code;
 * you should not need to (explicitly) include any other WORHP header.
 */

#ifndef WORHP_H
#define WORHP_H

#ifdef __cplusplus
extern "C" {
#endif

#include "C_worhp_aux.h"
#include "C_worhp_setup.h"
#include "C_Worhp_Members.h"
#include "C_worhp_io.h"
#include "worhp_version.h"
#include "C_cs.h"

/* function pointer types for the non-RC interface */
typedef void (*fp_obj)
  (const int *const N, const double *const X, double *const F);
typedef void (*fp_con)
  (const int *const N, const int *const M, const double *const X, double *const G);
typedef void (*fp_dobj)
  (const int *const N, const int *const DFnnz, const int *const DFrow,
   const double *const X, double *const DF);
typedef void (*fp_dcon)
  (const int *const N, const int *const M, const int *const DGnnz,
   const int *const DGrow, const int *const DGcol, const double *const X,
   double *const DG);
typedef void (*fp_hess)
  (const int *const N, const int *const M, const int *const HMnnz,
   const int *const HMrow, const int *const HMcol, const double *const X,
   const double *const Mu, double *const HM);

/*
  Traditional Interface

  Mode: Control operation mode
  0: Set Dparam and Iparam to standard values for the given inputs.
  1: Start optimisation with Worhp.

  N: Number of optimization variables.
  M: Number of constraints.
  X: Optimization variables, vector of size N.
  L: Lower bounds, vector of size N+M (N box constrains, M general constraints).
  U: Lower bounds, vector of size N+M (N box constrains, M general constraints).

  Dparam: Double parameter array of size 10 [defaults in brackets].
  Dparam(1): Optimality Tolerance.  [1d-7]
  Dparam(2): Feasibility Tolerance. [1d-7]
  Dparam(3): Large value to regard as infinity. [1d+20]
  Dparam(4): Finite difference epsilon. [1d-5]
  Dparam(5:10): Currently unused.

  Iparam: Integer parameter array of size 10.
  Iparam(1): Control gradient computation.
      0: Finite difference gradient computation.
         DFnnz and DFrow are used but DOBJ is not called.
      1: User-supplied gradient.
         DFnnz and DFrow are used and DOBJ is called to update values.
  Iparam(2): Control Jacobian computation.
      0: Finite difference Jacobian computation.
         DGnnz, DGrow and DGcol are used but DCON is not called.
      1: User-supplied gradient.
         DFnnz and DFrow are used and DCON is called to update values.
  Iparam(3): Control second order computation.
     -1: Use BFGS update.
         HMnnz, HMrow and HMcol are ignored, HESS is not called.
      0: Finite difference Hessian matrix whose structure is approximated.
         HMnnz, HMrow and HMcol are ignored, HESS is not called.
      1: Finite difference Hessian matrix whose structure is user-defined.
         HMnnz, HMrow and HMcol are used, HESS is not called.
      2: User defined Hessian, structure specified by HMnnz, HMrow and HMcol,
         structure is checked once on entry, HESS is called to update values.
      3: Like 2, but structure is not checked on entry.
  Iparam(4): Maximum number of major iterations.
  Iparam(5): Maximum number of minor iterations.
  Iparam(6:10): Currently unused.

  DFnnz: Number of nonzero entries of the gradient of the objective function.
  DFrow: Vector of size DFnnz, which contains the row indices of the entries
         of DF in CS (coordinate storage) format.

  DGnnz: Number of nonzero entries of the Jacobian of the constraints.
  DGrow: Vector of size DGnnz, which contains the row indices of the entries
         of DG in CS (coordinate storage) format.
  DGcol: Vector of size DGnnz, which contains the column indices in CS (coordinate
         storage) format.

  HMnnz: Number of nonzero entries of the Hessian of the Lagrange function.
  HMrow: Vector of size HMnnz, which contains the row indices of the entries
         of HM in CS (coordinate storage) format.
  HMcol: Vector of size HMnnz, which contains the column indices in CS (coordinate
         storage) format.
         NOTE: Please confer the WORHP user manual for the ordering assumptions
               on the Hessian storage format by WORHP.

  fp_obj: Function pointer for the objective function.
  fp_con: Function pointer for the constraints.

  fp_dobj: Function pointer for the gradient of the objective function.
           If Iparam[1] = 0, you can pass a NULL-pointer instead (though we
           recommend you provide a dummy routine for safety's sake).
  fp_dcon: Function pointer for the gradient of the constraints.
           If Iparam[2] = 0, you can pass a NULL-pointer instead (though we
           recommend you provide a dummy routine for safety's sake).
  fp_hess: Function pointer for the Hessian of the Lagrange function.
           If Iparam[3] < 2, you can pass a NULL-pointer instead (though we
           recommend you provide a dummy routine for safety's sake).
*/
void WorhpSimple
(int *const Mode, const int *const N, const int *const M, double *const X,
 double *const L, double *const U, double Dparam[10], int Iparam[10],
 const int *const DFnnz, const int *const DFrow,
 const int *const DGnnz, const int *const DGrow, const int *const DGcol,
 const int *const HMnnz, const int *const HMrow, const int *const HMcol,
 fp_obj, fp_con, fp_dobj, fp_dcon, fp_hess);

/*
 * Unified Solver Interface function prototype:
 */
typedef void USI(OptVar *const opt, Workspace *const wsp,
           const Params *const par, Control   *const cnt);

/*
 *  Prototpyes for Worhp's solver and auxiliary routines.
 */
void Worhp(OptVar*, Workspace*, Params*, Control*);
void WorhpBasic(OptVar*, Workspace*, Params*, Control*,
                USI *F, USI *G, USI *DF, USI *DG, USI *HM);
void WorhpFree(OptVar*, Workspace*, Params*, Control*);
void WorhpFidif(OptVar*, Workspace*, Params*, Control*);
void WorhpInit(OptVar*, Workspace*, Params*, Control*);
void WorhpRestart(OptVar*, Workspace*, Params*, Control*);
void WorhpCrossover(OptVar*, Workspace*, Params*, Control*);
void ReadParams(int*, const char[], Params*);

/*
 *  Worhp Zen
 */
void ZenGetD(OptVar*, Workspace*, Params*, Control*,
             const char *var_pert, const char *pert, int *dim, double *d);
void ZenGetDMatrix(OptVar*, Workspace*, Params*, Control*,
                   const char *var_pert, const char *pert, double *val);
void ZenGetMaxPert(OptVar*, Workspace*, Params*, Control*,
                   double *max_dp, double *max_dr, double *max_dq,
                   double *max_db);
void ZenCalcD(OptVar*, Workspace*, Params*, Control*,
              const char *var_pert, const char *pert, int *dim);
void ZenUpdate(OptVar*, Workspace*, Params*, Control*,
               const char *var_pert, double *varnew, const double *dp,
               const double *dr, const double *dq,
               const double *db, const int *order);
void ZenQP2Diff(OptVar*, Workspace*, Params*, Control*);
int  ZenN(const OptVar*, const char *var_pert);
bool ZenIsVar(const char *var_pert);
bool ZenIsPert(const char *var_pert);

/*
 * These two need to be used together:
 * InitParams will set all parameters to default values
 * and also set the 'initialised' flag.
 * ReadParamsNoInit will then read a parameter file without
 * overwriting all parameters by their default values.
 *
 * Using ReadParamsNoInit alone will result in undefined behaviour!
 */
void InitParams(int*, Params*);
void ReadParamsNoInit(int*, const char*, Params*);


/*
 * Save solver data to xml file to be used for hot start.
 *
 * file: Name of the xml file (suffix will be added, if necessary).
 * cmpr: Control compression of the xml file (0/1).
 */
void HotStartSave(int *status, char file[], OptVar*, Workspace*,
		  Params*, Control*, int cmpr);
/*
 * Load solver data from xml file for hot start.
 *
 * file: Name of the xml file (suffix will be added, if necessary).
 * cmpr: Control compression of the xml file (0/1), which should
 *       match the value used for creating the file.
 * verb: Control verbosity of the hot start function (0-3).
 */
void HotStartLoad(int *status, char file[], OptVar*, Workspace*,
		  Params*, Control*, int cmpr, int verb);

/*
 * Macros for secure access to workspace partitions
 * via Workspace Management Tables
 */

/* Get size of the specified slice */
#define IWS_SIZE(WSP,ENTRY) WSP->IWMT[4][WSP->ENTRY-1]
#define RWS_SIZE(WSP,ENTRY) WSP->RWMT[4][WSP->ENTRY-1]


/*
 * Macros for C for-loop access to specified slice.
 * Use them like this:
 *
 *  for(i = xWS_LO(wsp, mySlice); i < xWS_UP(wsp, mySlice); ++i) {
 *    wsp->Xws[i] = something;
 *  }
 *
 * or like this:
 *
 *  for(i = 0; i < xWS_SIZE(wsp, mySlice); ++i) {
 *    wsp->Xws[i + xWS_LO(wsp, mySlice)] = something;
 *  }
 */
#define IWS_LO(WSP,ENTRY) WSP->IWMT[0][WSP->ENTRY-1]
#define IWS_UP(WSP,ENTRY) WSP->IWMT[3][WSP->ENTRY-1]
#define RWS_LO(WSP,ENTRY) WSP->RWMT[0][WSP->ENTRY-1]
#define RWS_UP(WSP,ENTRY) WSP->RWMT[3][WSP->ENTRY-1]

#define IWS_PTR(WSP,ENTRY) WSP->iws+WSP->IWMT[0][ENTRY-1]
#define RWS_PTR(WSP,ENTRY) WSP->rws+WSP->RWMT[0][ENTRY-1]


/*
 * Macros for custom access with 1-indexing: 1...N
 */

/* Get index in WS */
/*#define IWS_IDX_1(ENTRY) work%IWMT(ENTRY,1)*/
/*#define RWS_IDX_1(ENTRY) work%RWMT(ENTRY,1)*/

/* Get single element in WS */
/*#define IWS_ELEM_1(ENTRY,IDX) iws(work%IWMT(ENTRY,1)+IDX)*/
/*#define RWS_ELEM_1(ENTRY,IDX) rws(work%RWMT(ENTRY,1)+IDX)*/


/*
 * Macros for custom access with 0-indexing: 0...N-1
 */

/* Get index in WS */
/*#define IWS_IDX_0(ENTRY) work%IWMT(ENTRY,3)*/
/*#define RWS_IDX_0(ENTRY) work%RWMT(ENTRY,3)*/

/* Get single element in WS */
/*#define IWS_ELEM_0(ENTRY,IDX) iws(work%IWMT(ENTRY,3)+IDX)*/
/*#define RWS_ELEM_0(ENTRY,IDX) rws(work%RWMT(ENTRY,3)+IDX)*/

#ifdef __cplusplus
}
#endif

#endif
