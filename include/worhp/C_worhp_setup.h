#ifndef C_WORHP_SETUP_H
#define C_WORHP_SETUP_H

#ifdef __cplusplus
extern "C" {
#endif

/* Pre-Init routine to be called by library users. Resets all
 * values to safe defaults, so WorhpInit works correctly. */
DLL_PUBLIC
void
WorhpPreInit(OptVar *opt, Workspace *wsp, Params *par, Control *cnt);

#ifdef __cplusplus
}
#endif

#endif
