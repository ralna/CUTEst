
#ifndef C_CS_H
#define C_CS_H

#ifdef __cplusplus
extern "C" {
#endif

#include "C_std.h"

typedef enum {
  Matrix_Kind_Undefined = 0,
  Matrix_Kind_General   = 1,
  Matrix_Kind_ComCol    = Matrix_Kind_General,
  Matrix_Kind_LowTri    = 2,
  Matrix_Kind_Diagonal  = 3,
  Matrix_Kind_Identity  = 4,
  Matrix_Kind_Vector    = 5,
  Matrix_Kind_Struct    = 6
} WorhpMatrix_Kind;

typedef enum {
  WorhpMatrix_OK         =  OK,
  WorhpMatrix_Dim_Error  = -1,
  WorhpMatrix_Kind_Error = -2,
  WorhpMatrix_Init_Error = -3
} WorhpMatrix_Status;

typedef enum {
  WorhpMatrix_Dont_Allocate       = -1,
  WorhpMatrix_Init_Dense          = -2
} WorhpMatrix_Init_Flags;

enum {
  WorhpMatrix_Name_Length = 10
};

/* Threshold used for printing values by PrintWorhpMatrix */
static const mat_int PrintWorhpMatrix_Threshold = 100;

typedef struct WorhpMatrix {

  /* Vectors */
  double* val;
  mat_int* row;
  mat_int* col;
  mat_int* slc;
  mat_int* CCrow;
  mat_int* CCcol;
  mat_int* CCper;
  mat_int* nGroups;
  mat_int* Groups;
  mat_int* nSubGroups;
  mat_int* SubGroups;
  mat_int* perm;

  /*
   * if (dim_foo > 0)
   * WORHP allocated memory for foo and is responsible for freeing it.
   *
   * if (dim_foo == 0 && foo != NULL)
   * foo belongs to someone else and WORHP must not attempt to free it.
   */

  /* Vector dimensions */
  mat_int dim_val;
  mat_int dim_row;
  mat_int dim_col;
  mat_int dim_slc;
  mat_int dim_CCrow;
  mat_int dim_CCcol;
  mat_int dim_CCper;
  mat_int dim_nGroups;
  mat_int dim_Groups;
  mat_int dim_nSubGroups;
  mat_int dim_SubGroups;
  mat_int dim_perm;

  /* Current and default dimensions */
  mat_int nnz;
  mat_int nnzDefault;
  mat_int nRow;
  mat_int nRowDefault;
  mat_int nCol;
  mat_int nColDefault;

  /* Scalar group counter (used in Worhp_Groups) */
  mat_int nGrp;

  int kind;

  char name[WorhpMatrix_Name_Length];
  bool NeedStructure;
  bool Dense;
} WorhpMatrix;

DLL_PUBLIC void PrintWorhpMatrix(const WorhpMatrix *const WM);
DLL_PUBLIC int InitWorhpMatrix(WorhpMatrix* WM, const char* name, int extend, bool CCwithRow,
                               bool CCwithCol);
DLL_PUBLIC void FreeWorhpMatrix(WorhpMatrix *WM);
DLL_PUBLIC void ZeroWorhpMatrix(WorhpMatrix *WM);
DLL_PUBLIC void CopyWorhpMatrix(WorhpMatrix *const to, const WorhpMatrix *const from);

DLL_PUBLIC void SortWorhpMatrix(WorhpMatrix *WM);

#ifdef __cplusplus
}
#endif

#endif
