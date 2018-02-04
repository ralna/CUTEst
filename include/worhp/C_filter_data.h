#ifndef C_FILTER_DATA_H
#define C_FILTER_DATA_H

#ifdef __cplusplus
extern "C" {
#endif

#include "C_std.h"

/**
 *  This struct needs to precisely mirror the
 *  FilterNode type in filter_data.F90.
 *  To make manual and semi-automatic handling easier
 *  + Keep it sorted
 *    1. By type, descending size (double > [type]* > int > Bool)
 *    2. Inside each type, alphabetically by name.
 *  + Exactly one member declaration per line.
 */
struct FltNode{
  double CV;
  double F;
  /* int n; */
  bool initialised;
  struct FltNode *next;
  struct FltNode *prev;
};

typedef struct FltNode FilterNode;

#ifdef __cplusplus
}
#endif

#endif
