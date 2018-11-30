#pragma once

#ifdef __cplusplus
extern "C" {
#endif

#include "C_Worhp_Data.h"

void WorhpMonitorInit();

void WorhpMonitorClear();

void WorhpMonitorIter(OptVar *opt, Workspace *wsp, Params *par, Control *cnt);

void WorhpMonitorText();

#ifdef __cplusplus
}
#endif
