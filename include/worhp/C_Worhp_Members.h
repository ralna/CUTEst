#ifndef C_WORHP_MEMBERS_H_GUARD
#define C_WORHP_MEMBERS_H_GUARD 1

#ifdef __cplusplus
extern "C" {
#endif

/*----------------------------------------------------------
 *
 * This header defines the runtime parameter information and
 * modification functions.
 *
 * WOHRP allows to poll/collect parameter names, types and
 * descriptions by index, and name-based setters and getters.
 *
 * Note that indexing is 1-based and that name-based access
 * is case sensitive. String arguments are C-style strings,
 * and must be null-terminated.
 *
 * The const char* functions return pointers to local static
 * memory, which may be invalidated by any other operation
 * into the WORHP shared library. You should therefore make
 * deep copies of these 'strings' to control their lifetime,
 * before performing operations on them.
 *
 * Note that the getter functions do not provide deep copies
 * of vector-valued parameters, but point to the corresponding
 * Params component instead. The setter functions therefore
 * do not operate on vector-valued parameters.
 *
 * These functions are neither guaranteed to be thread-safe,
 * nor particularly tuned for performance. You should refrain
 * from calling them from different threads or from inside
 * performance-critical loops.
 *
 *----------------------------------------------------------*/

typedef enum {
  WORHP_BOOL_T,
  WORHP_CUSTOM_T,  /* custom type, i.e. structs */
  WORHP_DOUBLE_T,
  WORHP_INT_T,
  WORHP_NO_T,      /* no type (default or invalid answer) */
  WORHP_SINGLE_T,
  WORHP_SIZE_T,
  WORHP_CONST_C_STRING_T
} WorhpType;

#include "C_std.h"
#include "C_Worhp_Data.h"

/* Get total number of parameters */
DLL_PUBLIC int WorhpGetParamCount(void);

/* Get parameter name by index in {1, ..., WorhpGetParamCount()}
 * Returns empty string for invalid indices. */
DLL_PUBLIC const char* WorhpGetParamName(int i);

/* Get parameter type by index in {1, ..., WorhpGetParamCount()}
 * Returns WORHP_NO_T for invalid indices. */
DLL_PUBLIC WorhpType WorhpGetParamType(int i);

/* Get short parameter description by index in {1, ..., WorhpGetParamCount()}
 * Returns empty string for invalid indices. */
DLL_PUBLIC const char* WorhpGetParamDescription(int i);

/* Get parameter by case-sensitive name. Return value indicates
 * whether name was found; if name was not found, value is undefined. */
DLL_PUBLIC bool WorhpGetBoolParam  (Params *par, const char* name, bool   *value);
DLL_PUBLIC bool WorhpGetIntParam   (Params *par, const char* name, int    *value);
DLL_PUBLIC bool WorhpGetDoubleParam(Params *par, const char* name, double *value);

/* Set parameter by case-sensitive name. Return value indicates
 * whether name was found; does not operate on vector-valued parameters. */
DLL_PUBLIC bool WorhpSetBoolParam  (Params *par, const char* name, bool   value);
DLL_PUBLIC bool WorhpSetIntParam   (Params *par, const char* name, int    value);
DLL_PUBLIC bool WorhpSetDoubleParam(Params *par, const char* name, double value);

#ifdef __cplusplus
}
#endif

#endif
