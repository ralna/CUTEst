#ifndef C_TIMER_H
#define C_TIMER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "C_std.h"

enum {
  TIMER_STOPPED = 0,
  TIMER_STARTED = 1
};

typedef struct {
  double start;
  double total;
  int status;
  int _shim;
} TimerType;

typedef struct {
  int year;
  int month;
  int day;
  int hour;
  int minute;
  int second;
} DateType;

DLL_PUBLIC void StartTimer(TimerType *T);
DLL_PUBLIC void ResetTimer(TimerType *T);
DLL_PUBLIC void  StopTimer(TimerType *T);
DLL_PUBLIC double GetTimer(TimerType const *T);
DLL_PUBLIC double GetTimerCont(TimerType const *T);

void GetDate(DateType *D);

#ifdef __cplusplus
}
#endif

#endif
