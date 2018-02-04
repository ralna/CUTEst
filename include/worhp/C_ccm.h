
#ifndef C_CCM_H
#define C_CCM_H

#ifdef __cplusplus
extern "C" {
#endif

#include "C_std.h"
#include "C_cs.h"

typedef enum {
  CCMatrixOK           =  OK,
  CCMatrixDimError     = -1,
  CCMatrixKindError    = -2,
  CCMatrixInitError    = -3,
  CCMatrixNotEnoughRWS = notEnoughRWS,
  CCMatrixNotEnoughIWS = notEnoughIWS
} CCMatrixStatus;

#ifdef __cplusplus
}
#endif

#endif
