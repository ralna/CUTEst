
/* dummy interfaces to OSQP functions to
 * allow for testing of CUTEst interface
 *
 * Nick Gould Dec 20 2017
 *
 */

#include "osqp.h"
#include "auxil.h"
#include "util.h"
#include "scaling.h"
#include "glob_opts.h"

#ifndef EMBEDDED
#include "polish.h"
#endif

#ifdef CTRLC
#include "ctrlc.h"
#endif

#ifndef EMBEDDED
#include "lin_sys.h"
#endif

/* set default settings */

void osqp_set_default_settings(OSQPSettings * settings) {
    settings->scaling = SCALING;
#if EMBEDDED != 1
    settings->adaptive_rho = ADAPTIVE_RHO;
    settings->adaptive_rho_interval = ADAPTIVE_RHO_INTERVAL;
    settings->adaptive_rho_tolerance = (c_float) ADAPTIVE_RHO_TOLERANCE;
#ifdef PROFILING
    settings->adaptive_rho_fraction = (c_float) ADAPTIVE_RHO_FRACTION;
#endif
#endif
    settings->rho = (c_float) RHO;
    settings->sigma = (c_float) SIGMA;
    settings->max_iter = MAX_ITER;
    settings->eps_abs = (c_float) EPS_ABS;
    settings->eps_rel = (c_float) EPS_REL;
    settings->eps_prim_inf = (c_float) EPS_PRIM_INF;
    settings->eps_dual_inf = (c_float) EPS_DUAL_INF;
    settings->alpha = (c_float) ALPHA;
    settings->linsys_solver = LINSYS_SOLVER;
#ifndef EMBEDDED
    settings->delta = DELTA;
    settings->polish = POLISH;
    settings->polish_refine_iter = POLISH_REFINE_ITER;
    settings->verbose = VERBOSE;
#endif
    settings->scaled_termination = SCALED_TERMINATION;
    settings->check_termination = CHECK_TERMINATION;
    settings->warm_start = WARM_START;
#ifdef PROFILING
    settings->time_limit = TIME_LIMIT;
#endif
}

/* dummy setup */

OSQPWorkspace * osqp_setup(const OSQPData * data, OSQPSettings *settings){
  OSQPWorkspace * work; // Workspace

  /* Allocate empty workspace */

  work = c_calloc(1, sizeof(OSQPWorkspace));
  if (!work){
#ifdef PRINTING
    c_print("ERROR: allocating work failure!\n");
#endif
    return OSQP_NULL;
  }

  /* Copy problem data into workspace */

  work->data = c_malloc(sizeof(OSQPData));
  work->data->n = data->n;    // Number of variables
  work->data->m = data->m;    // Number of linear constraints

  /* Allocate solution */

  work->solution = c_calloc(1, sizeof(OSQPSolution));
  work->solution->x = c_calloc(1, work->data->n * sizeof(c_float));
  work->solution->y = c_calloc(1, work->data->m * sizeof(c_float));

  /* allocate and initialize information */

  work->info = c_calloc(1, sizeof(OSQPInfo));
  work->info->status_val = OSQP_UNSOLVED;
  c_strcpy(work->info->status, "unsolved");

  return work;
}

/* dummy solve */

c_int osqp_solve(OSQPWorkspace * work){
  c_int exitflag = 0;
  int i;

  /* set dummy return values */

  work->info->status_val = OSQP_UNSOLVED;
  c_strcpy(work->info->status, "unsolved");
  work->info->obj_val = OSQP_INFTY;
  for (i = 0 ; i < work->data->n ; i++) work->solution->x[i] = 0.0;
  for (i = 0 ; i < work->data->m ; i++) work->solution->y[i] = 0.0;
  return exitflag;
}

/* dummy cleanup */

c_int osqp_cleanup(OSQPWorkspace * work){
  c_int exitflag=0;

  // Free solution

  if (work->solution) {
    if (work->solution->x) c_free(work->solution->x);
    if (work->solution->y) c_free(work->solution->y);
    c_free(work->solution);
  }

  // Free information
  if (work->info) c_free(work->info);

  return exitflag;
}

void c_strcpy(char dest[], const char source[]){
  int i = 0;
  while (1) {
     dest[i] = source[i];
     if (dest[i] == '\0') break;
     i++;
  }
}

csc* csc_matrix(c_int m, c_int n, c_int nzmax, c_float* x, c_int* i, c_int* p)
{
    csc* M = (csc *)c_malloc(sizeof(csc));
    M->m = m;
    M->n = n;
    M->nz = -1;
    M->nzmax = nzmax;
    M->x = x;
    M->i = i;
    M->p = p;
    return M;
}
