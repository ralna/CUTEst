#pragma once

#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
#include <windows.h>
#endif

#include "C_Worhp_Data.h"

void WorhpMonitorInit();

void WorhpMonitorClear();

void WorhpMonitorIter(OptVar *opt, Workspace *wsp, Params *par, Control *cnt);

void WorhpMonitorText();

#ifdef __cplusplus
}
#endif
