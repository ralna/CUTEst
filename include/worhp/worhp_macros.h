#ifndef WORHP_MACROS_H
#define WORHP_MACROS_H

#include "worhp_version.h"

/*
 * Extra visibility control for Linux: Actual exports are controlled by
 * the map-files, but this is supposed to generate more efficient code
 */
#ifndef _WIN32
# define DLL_PUBLIC  __attribute__ ((visibility("default")))
# define DLL_PRIVATE __attribute__ ((visibility("hidden")))
#else
# define DLL_PUBLIC
# define DLL_PRIVATE
#endif

/*
 * This macro makes the executing code check for matching library version
 */
#define CHECK_WORHP_VERSION if(CheckWorhpVersion(WORHP_MAJOR, WORHP_MINOR, WORHP_PATCH)) {exit(EXIT_FAILURE);}

#endif
