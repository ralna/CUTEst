#ifndef C_WORHP_AUX_H
#define C_WORHP_AUX_H

#ifdef __cplusplus
extern "C" {
#endif

#include "C_Worhp_Data.h"

DLL_PUBLIC void WorhpDiag(OptVar*, Workspace*, Params*, Control*);
DLL_PUBLIC void PrintIWMT(Workspace*);
DLL_PUBLIC void PrintRWMT(Workspace*);

#ifdef __cplusplus
}
#endif

#endif
