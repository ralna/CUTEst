#ifndef C_LEQSOL_DATA_H
#define C_LEQSOL_DATA_H

#ifdef __cplusplus
extern "C" {
#endif

#include "C_std.h"
#include "C_cs.h"

/**
 *  This struct needs to precisely mirror the
 *  LeqsolWorkspace type in leqsol_data.F90.
 *  To make manual and semi-automatic handling easier
 *  + Keep it sorted
 *    1. By type, descending size (double > [type]* > int > Bool)
 *    2. Inside each type, alphabetically by name.
 *  + Exactly one member declaration per line.
 */
typedef struct LeqsolWorkspaceStruct {
  double EPS;
  double ITREF_TOL;
  double ITSOL_TOL;

  double smallval;
  double u;

  int    *ITSOL_PCOL;
  int    *ITSOL_PROW;
  double *ITSOL_PVAL;
  double *ITSOL_X0;

  int ITSOL_PCOL_ALLOCATED;
  int ITSOL_PROW_ALLOCATED;
  int ITSOL_PVAL_ALLOCATED;
  int ITSOL_X0_ALLOCATED;

  int DIRECT_METHOD;
  int IPRINT;
  int ITREF_MAXITER;
  int ITSOL_MAXITER;
  int ITSOL_METHOD;
  int ITSOL_PNZ;

  int ordering;
  int print_level;
  int nemin;
  int scaling;
  int factor_min;

  bool ITSOL_PERFORMPRECOND;
  bool ITSOL_PSYMMETRIC;
  bool SCAL;
  bool TRYSIMPLE;

  bool solve_blas3;
  bool solve_mf;
  bool action;

  bool initsymb;
  bool initlu;

  double *valsave;
  double *diag;
  double *perm;
  double *buf;
  double *d;

  void *akeep;
  void *fkeep;
  void *control;
  void *info;
} LeqsolWorkspace;

#ifdef __cplusplus
}
#endif

#endif
