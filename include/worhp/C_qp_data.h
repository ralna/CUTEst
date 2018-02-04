#ifndef C_QP_DATA_H
#define C_QP_DATA_H

#ifdef __cplusplus
extern "C" {
#endif

#include "C_std.h"
#include "C_cs.h"
#include "C_leqsol_data.h"

  enum {
   QPActionSolve = 1,
   QPActionSolveWithWarmstart = 2,
   QPActionInit = 3,
   QPActionInitWithWarmstart = 4,
   QPActionContinue = 5,
   QPActionCleanup = 6
  };

  enum {
      QPStatusSuccess                 =     0,
      QPStatusNotFinished             =     1,
      QPStatusMethodNotImplemented    =    -1,
      QPIpStatusInfeasible            =    -2,
      QPIpStatusSlowProgressRes       =    -3,
      QPIpStatusSlowProgressPhi       =    -4,
      QPIpStatusMinStepSize           =    -5,
      QPIpStatusMaxIter               =    -6,
      QPNsnStatusNoProgress           =    -7,
      QPNsnStatusMinStepSize          =    -8,
      QPNsnStatusMaxIter              =    -9,
      QPNsnStatusNaN                  =   -10,
      QPStatusTimeout                 =   -11,
      QPLgsSolverErrorCodeStart       = -1000,
      QPCgnrStatusMaxiter             = -101,
      QPCgnrStatusDivisionByZero      = -102
  };

/**
 *  This struct needs to precisely mirror the
 *  QPWorkspace type in qp_data.F90.
 *  To make manual and semi-automatic handling easier
 *  + Keep it sorted
 *    1. By type, descending size (double > [type]* > int > Bool)
 *    2. Inside each type, alphabetically by name.
 *  + Exactly one member declaration per line.
 */
extern const int nMemberQPWorkspace; /* in C_qp_data.c */
typedef struct QPWorkspaceStruct {

  LeqsolWorkspace leqsol;

  double   IP_ALPHA;
  double   IP_GAP;
  double   IP_MINPHI;
  double   IP_MINPHI2;
  double   IP_MU;
  double   IP_MU0;
  double   IP_NORMLGSRES;
  double   IP_NORMRES;
  double   IP_NORMRES0;
  double   IP_NORMRESQ;
  double   IP_NORMRESQ0;
  double   IP_NORMSYSMAT;
  double   IP_NORMSYSMATQ;
  double   IP_REGDELTA;
  double   ipComTol;
  double   ipResTol;
  double   lsTol;
  double   SCALEOBJ;

  int     *ACOL;
  int     *AROW;
  double  *ASCAL;
  double  *AVAL;
  double  *B;
  double  *C;
  int     *CCOL;
  int     *CROW;
  double  *CSCAL;
  double  *CVAL;
  double  *CX;
  double  *D;
  double  *IP_ATY;
  double  *IP_AX;
  int     *IP_COL;
  double  *IP_CTZ;
  double  *IP_CX;
  double  *IP_DELTA;
  int     *IP_DIAGAIND;
  int     *IP_DIAGCIND;
  int     *IP_DIAGQIND;
  double  *IP_DS;
  double  *IP_DSAFF;
  double  *IP_DSL;
  double  *IP_DSLAFF;
  double  *IP_DSU;
  double  *IP_DSUAFF;
  double  *IP_DZ;
  double  *IP_DZAFF;
  double  *IP_DZL;
  double  *IP_DZLAFF;
  double  *IP_DZU;
  double  *IP_DZUAFF;
  double  *IP_PHI;
  double  *IP_QDIAG;
  double  *IP_QX;
  double  *IP_RA;
  double  *IP_RC;
  double  *IP_RHS;
  double  *IP_RL;
  int     *IP_ROW;
  double  *IP_RQ;
  double  *IP_RU;
  double  *IP_S;
  double  *IP_SL;
  double  *IP_STARTS;
  double  *IP_STARTSL;
  double  *IP_STARTSU;
  double  *IP_SU;
  double  *IP_VAL;
  double  *IP_WARMS;
  double  *IP_WARMSL;
  double  *IP_WARMSU;
  int     *IXL;
  int     *IXU;
  int     *QCOL;
  int     *QROW;
  double  *QVAL;
  double  *SCALEEQ;
  double  *SCALEIEQ;
  double  *STARTX;
  double  *STARTY;
  double  *STARTZ;
  double  *STARTZL;
  double  *STARTZU;
  double  *WARMX;
  double  *WARMY;
  double  *WARMZ;
  double  *WARMZL;
  double  *WARMZU;
  double  *X;
  double  *XL;
  double  *XSCAL;
  double  *XU;
  double  *Y;
  double  *Z;
  double  *ZL;
  double  *ZU;

  int      ACOL_ALLOCATED;
  int      AROW_ALLOCATED;
  int      ASCAL_ALLOCATED;
  int      AVAL_ALLOCATED;
  int      B_ALLOCATED;
  int      C_ALLOCATED;
  int      CCOL_ALLOCATED;
  int      CROW_ALLOCATED;
  int      CSCAL_ALLOCATED;
  int      CVAL_ALLOCATED;
  int      CX_ALLOCATED;
  int      D_ALLOCATED;
  int      IP_ATY_ALLOCATED;
  int      IP_AX_ALLOCATED;
  int      IP_COL_ALLOCATED;
  int      IP_CTZ_ALLOCATED;
  int      IP_CX_ALLOCATED;
  int      IP_DELTA_ALLOCATED;
  int      IP_DIAGAIND_ALLOCATED;
  int      IP_DIAGCIND_ALLOCATED;
  int      IP_DIAGQIND_ALLOCATED;
  int      IP_DS_ALLOCATED;
  int      IP_DSAFF_ALLOCATED;
  int      IP_DSL_ALLOCATED;
  int      IP_DSLAFF_ALLOCATED;
  int      IP_DSU_ALLOCATED;
  int      IP_DSUAFF_ALLOCATED;
  int      IP_DZ_ALLOCATED;
  int      IP_DZAFF_ALLOCATED;
  int      IP_DZL_ALLOCATED;
  int      IP_DZLAFF_ALLOCATED;
  int      IP_DZU_ALLOCATED;
  int      IP_DZUAFF_ALLOCATED;
  int      IP_PHI_ALLOCATED;
  int      IP_QDIAG_ALLOCATED;
  int      IP_QX_ALLOCATED;
  int      IP_RA_ALLOCATED;
  int      IP_RC_ALLOCATED;
  int      IP_RHS_ALLOCATED;
  int      IP_RL_ALLOCATED;
  int      IP_ROW_ALLOCATED;
  int      IP_RQ_ALLOCATED;
  int      IP_RU_ALLOCATED;
  int      IP_S_ALLOCATED;
  int      IP_SL_ALLOCATED;
  int      IP_STARTS_ALLOCATED;
  int      IP_STARTSL_ALLOCATED;
  int      IP_STARTSU_ALLOCATED;
  int      IP_SU_ALLOCATED;
  int      IP_VAL_ALLOCATED;
  int      IP_WARMS_ALLOCATED;
  int      IP_WARMSL_ALLOCATED;
  int      IP_WARMSU_ALLOCATED;
  int      IXL_ALLOCATED;
  int      IXU_ALLOCATED;
  int      QCOL_ALLOCATED;
  int      QROW_ALLOCATED;
  int      QVAL_ALLOCATED;
  int      SCALEEQ_ALLOCATED;
  int      SCALEIEQ_ALLOCATED;
  int      STARTX_ALLOCATED;
  int      STARTY_ALLOCATED;
  int      STARTZ_ALLOCATED;
  int      STARTZL_ALLOCATED;
  int      STARTZU_ALLOCATED;
  int      WARMX_ALLOCATED;
  int      WARMY_ALLOCATED;
  int      WARMZ_ALLOCATED;
  int      WARMZL_ALLOCATED;
  int      WARMZU_ALLOCATED;
  int      X_ALLOCATED;
  int      XL_ALLOCATED;
  int      XSCAL_ALLOCATED;
  int      XU_ALLOCATED;
  int      Y_ALLOCATED;
  int      Z_ALLOCATED;
  int      ZL_ALLOCATED;
  int      ZU_ALLOCATED;

  int      ACTION;
  int      ADIM;
  int      CDIM;
  int      IP_ITER;
  int      IP_NDIM;
  int      IP_NNZ;
  int      LDIM;
  int      N;
  int      NEGEV;
  int      NZA;
  int      NZA_MAX;
  int      NZC;
  int      NZC_MAX;
  int      NZQ;
  int      NZQ_MAX;
  int      POSEV;
  int      STATUS;
  int      UDIM;
  int      ZEROEV;

  bool    CALCWARM;
  bool    CHECKZ;
  bool    INITS;
  bool    ipRelax;
  bool    strict;
  bool    SCALEDQP;
  bool    WarmstartInitDone;
} QPWorkspace;

/**
 *  This struct needs to precisely mirror the
 *  QPParams type in qp_data.F90.
 *  To make manual and semi-automatic handling easier
 *  + Keep it sorted
 *    1. By type, descending size (double > [type]* > int > Bool)
 *    2. Inside each type, alphabetically by name.
 *  + Exactly one member declaration per line.
 */
extern const int nMemberQPParams; /* in C_qp_data.c */
typedef struct QPParamsStruct {
  double ipBarrier;
  double ipComTol;
  double ipFracBound;
  double ipMinAlpha;
  double ipRelaxDiv;
  double ipRelaxMax;
  double ipRelaxMin;
  double ipRelaxMult;
  double ipResTol;
  double lsTol;
  double nsnBeta;
  double nsnKKT;
  double nsnMinAlpha;
  double nsnSigma;

  int ipLsMethod;
  int lsItMaxIter;
  int lsItMethod;
  int lsItPrecondMethod;
  int lsRefineMaxIter;
  int maxIter;
  int method;
  int nsnLsMethod;
  int printLevel;

  bool ipTryRelax;
  bool lsScale;
  bool lsTrySimple;
  bool nsnGradStep;
  bool scaleIntern;
  bool strict;
} QPParams;

#ifdef __cplusplus
}
#endif

#endif
