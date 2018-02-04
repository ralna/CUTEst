#ifndef C_WORHP_IO_H
#define C_WORHP_IO_H

#ifdef __cplusplus
extern "C" {
#endif

#include "worhp_macros.h"
#include "C_Worhp_Data.h"

enum {
  WORHP_PRINT_MESSAGE         = 1,
  WORHP_PRINT_WARNING         = 2,
  WORHP_PRINT_ERROR           = 4,
  WORHP_PRINT_BOLD            = 8,
  WORHP_PRINT_GREEN           = 16,
  WORHP_PRINT_BLUE            = 32,
  WORHP_PRINT_CONTINUED_START = 64,
  WORHP_PRINT_CONTINUED_END   = 128,
  WORHP_PRINT_CONTINUED       = 192
};

typedef void (*worhp_print_t) (int mode, const char s[]);

/**
 * Print function used internally by WORHP. Call it to print
 * your message the same way WORHP currently would.
 *
 * @note WorhpPrint is a wrapper around the actual print function
 * to ensure the function-pointer is non-null.
 * Do not pass WorhpPrint to SetWorhpPrint, since this will cause
 * an infinite recursion error (i.e. hangs or segfaults).
 * SetWorhpPrint detects this and falls back to the default print
 * function.
 */
DLL_PUBLIC void WorhpPrint(const int mode, const char message[]);

/**
 * The actual function that does the printing by default.
 * Pass this function to SetWorhpPrint to restore the default
 * printing behaviour.
 */
DLL_PUBLIC void WorhpDefaultPrintFunction(int mode, const char *message);

/**
 * Function for defining the low-level print function to be used by WORHP
 */
DLL_PUBLIC void SetWorhpPrint(worhp_print_t f);

/**
 * Prints an informative message including its origin, or continues a message.
 * @note Fortran-independent C implementation, since string interoperability
 * is inconvenient.
 * @see WorhpMessage
 */
DLL_PUBLIC void WorhpMessage(const char *message, const char *source, int prn);


/**
 * Prints a warning message including its origin, or continues a warning
 * message.
 * @note Fortran-independent C implementation, since string interoperability
 * is inconvenient.
 * @see WorhpError
 */
DLL_PUBLIC void WorhpWarning(const char *message, const char *source, int prn);


/**
 * Prints an error message including its origin, or continues an error
 * message.
 * @note Fortran-independent C implementation, since string interoperability
 * is inconvenient.
 * @see WorhpError
 */
DLL_PUBLIC void WorhpError(const char *message, const char *source, int prn);


/**
 * Constant for the maximum length of a description of a status that
 * that can be returned by Status2String.
 */
static size_t const WORHP_MAX_STATUS_LENGTH = 90;

/**
 * Prints a status message with information about the current solver status
 * to the standard output. Typically used to print the solver result.
 */
DLL_PUBLIC void StatusMsg(OptVar* o, Workspace* w, Params* p, Control* c);

/**
 * Writes a status message with information about the current solver status
 * into the given C-string. The 'message' parameter has to be allocated and large enough.
 */
DLL_PUBLIC void StatusMsgString(OptVar* o, Workspace* w, Params* p, Control* c, char message[]);

/**
 * Writes a string describing the given solver status into the given C-string.
 * The 'message' parameter has to be allocated and large enough, use the
 * constant 'MAX_STATUS_LENGTH' for this.
 */
DLL_PUBLIC void Status2String(int status, char str[]);


/**
 * Prints the iteration output.
 */
DLL_PUBLIC void IterationOutput(OptVar*, Workspace*, Params*, Control*);


/**
 * Prints the stage message.
 */
DLL_PUBLIC void StageMsg(OptVar*, Workspace*, Params*, Control*);


#ifdef __cplusplus
}
#endif

#endif
