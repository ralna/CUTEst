#ifndef THIS_PROBLEM_NAME
#define THIS_PROBLEM_NAME "WORHP/CUTEst"
#endif

#ifndef OUTSDIFD_FILENAME
#define OUTSDIFD_FILENAME "OUTSDIF.d"
#endif

#include "cutest.h"
#include "worhp.h"
#include <stdio.h>
#include <string.h>

/* Declare user functions, implementation later */
/* Objective function */
void UserF(OptVar *opt, Workspace *wsp, Params *par, Control *cnt);
/* Function of constraints */
void UserG(OptVar *opt, Workspace *wsp, Params *par, Control *cnt);
/* Gradient of objective function */
void UserDF(OptVar *opt, Workspace *wsp, Params *par, Control *cnt);
/* Jacobian of constraints */
void UserDG(OptVar *opt, Workspace *wsp, Params *par, Control *cnt);
/* Hessian of Lagrangian */
void UserHM(OptVar *opt, Workspace *wsp, Params *par, Control *cnt,
            integer hm_nnz, integer hm_nnz_init, integer HMdimMiss,
            integer hm_extended_size, integer *HMpermOldIndices);


int MAINENTRY() {
    /*
     * WORHP data structures
     *
     * OptVar contains the variables, constraint values and multipliers.
     * Workspace encapsulates all workspace and working variables for WORHP.
     * Params contains all WORHP parameters.
     * Control contains things for reverse communication flow control.
     */
    OptVar    opt;
    Workspace wsp;
    Params    par;
    Control   cnt;

    int status, i, j;
    logical fortran_true = TRUE_;
    integer order_none = 0;

    /* CUTEst related stuff */
    integer const cutest_input = 60;
    integer const io_buffer = 11;
    integer const out = 6;
    integer cutest_status, indexCount, changeIndex,
            jac_nnz, jac_nnz_init;
    logical *equatn = NULL;
    logical *linear = NULL;

    doublereal *j_val_init = NULL;
    doublereal *h_val_init = NULL;
    integer *j_fun_init = NULL;
    integer *j_var_init = NULL;
    integer *h_row_init = NULL;
    integer *h_col_init = NULL;

    integer hm_nnz, hm_nnz_init, HMdimMiss, hm_extended_size;
    integer *HMpermOldIndices = NULL;

    /* Counter */
    integer neval_F, neval_G, neval_DF, neval_DG, neval_HM;

    /* Adapt these as appropriate */
    char const cutest_problem[STRING_LENGTH] = THIS_PROBLEM_NAME;
    char const *outsdifd_filename = OUTSDIFD_FILENAME;
    char buffer[STRING_LENGTH];

    /* Check Version of library and header files */
    CHECK_WORHP_VERSION

    snprintf(buffer, sizeof(buffer), "CUTEst problem: %s", cutest_problem);
    WorhpPrint(WORHP_PRINT_MESSAGE, buffer);

    /*
     * Properly zeros everything, or else the following routines
     * could get confused
     */
    WorhpPreInit(&opt, &wsp, &par, &cnt);

    /*
     * Parameter initialisation routine that must be called
     * when using ReadParamsNoInit instead of ReadParams.
     */
    InitParams(&status, &par);

    /*
     * Parameter XML import routine that does not reset
     * all parameters to default values (InitParams does this)
     */
    ReadParamsNoInit(&status, "worhp.xml", &par);
    if (status == DataError || status == InitError) {
        return EXIT_FAILURE;
    }

    /*
     * WORHP data structure initialisation routine.
     * Calling this routine prior to WORHP is mandatory.
     * Before calling WorhpInit, set the problem and matrix dimensions as
     *
     * opt.n      = number of variables,
     * opt.m      = number of constraints (lin + nonlin, excluding box con's),
     * wsp.DF.nnz = nonzero entries of the objective function gradient,
     * wsp.DG.nnz = nonzero entries of the constraint Jacobian,
     * wsp.HM.nnz = nonzero entries of the Lagrange Hessian.
     *
     * Open CUTEst problem
     * cutest_input: unit number for the decoded data
     */
    FORTRAN_open(&cutest_input, outsdifd_filename, &cutest_status);
    if (cutest_status != 0) {
        snprintf(buffer, sizeof(buffer), "Coult not open %s",
                 outsdifd_filename);
        WorhpError(buffer, cutest_problem, par.NLPprint);
        return cutest_status;
    } else {
        snprintf(buffer, sizeof(buffer), "Opened %s", outsdifd_filename);
        WorhpMessage(buffer, cutest_problem, par.NLPprint);
    }

    /*
     * Getting problem dimensions
     * cutest_status: 0 - success, 1 - alloc/dealloc error,
     *                2 - array bound error, 3 - eval error
     */
    CUTEST_cdimen(&cutest_status, &cutest_input, &opt.n, &opt.m);
    if (cutest_status != 0) {
        return cutest_status;
    }

    if (par.UserDF) {
        wsp.DF.nnz = opt.n;
    } else {
        wsp.DF.nnz = WorhpMatrix_Init_Dense;
    }

    if (opt.m > 0) {
        /* If general constraints are present DG will be allocated further on */
        /* through the cutest sparse DG-functions */
        wsp.DG.nnz = WorhpMatrix_Dont_Allocate;
    } else {
        /* If the problem is generally unconstrained WORHP requires dg.nnz to */
        /* be WorhpMatrix_Init_Dense */
        wsp.DG.nnz = WorhpMatrix_Init_Dense;
    }

    wsp.HM.nnz = WorhpMatrix_Dont_Allocate;

    WorhpInit(&opt, &wsp, &par, &cnt);
    if (cnt.status != FirstCall) {
        WorhpError("Initialisation failed", cutest_problem, par.NLPprint);
        return EXIT_FAILURE;
    }

    /*
     * These pointers give access to the essential user data:
     *
     * opt.X[0] to opt.X[opt.n - 1]           : Optimisation variables
     * opt.Lambda[0] to opt.Lambda[opt.n - 1] : Multipliers for the constraints
     *                                          on X ("box constraints")
     * opt.G[0] to opt.G[opt.m - 1]           : Linear and nonlinear constraints
     * opt.Mu[0] to opt.Mu[opt.m - 1]         : Multipliers for the constraints
     *                                          on G
     *
     * Set initial values of X, Lambda and Mu here.
     * G need not be initialised.
     */

    if (opt.m > 0) {
        /* Constrained case */
        MALLOC(equatn, opt.m, logical);
        MALLOC(linear, opt.m, logical);

        CUTEST_csetup(&cutest_status, &cutest_input, &out, &io_buffer,
                      &opt.n, &opt.m, opt.X, opt.XL, opt.XU,
                      opt.Mu, opt.GL, opt.GU, equatn, linear,
                      &order_none, &order_none, &order_none);
#if (WORHP_MAJOR > 1 || WORHP_MINOR > 9)
        for (i = 0; i < opt.m; i += 1) {
            if (linear[i]) {
                opt.GType[i] = WORHP_LINEAR;
            }
        }
#endif
    } else {
        CUTEST_usetup(&cutest_status, &cutest_input, &out, &io_buffer,
                      &opt.n, opt.X, opt.XL, opt.XU);
    }
    FORTRAN_close(&cutest_input, &cutest_status);

    if (opt.m > 0) {
        CUTEST_cdimsj(&cutest_status, &jac_nnz_init);
        if (cutest_status != 0) {
            WorhpError("Error retrieving number of nonzeroes in DG from CUTEst",
                       cutest_problem, par.NLPprint);
        }

        /*
         * Initialise structure of Jacobian and split between objective gradient
         * and jacobian of the constraints as CUTEst does not provide a
         * structure giving method. We misuse evaluation of the Jacobian to gain
         * the structure.
         */
        MALLOC(j_val_init, jac_nnz_init, doublereal);
        MALLOC(j_var_init, jac_nnz_init, integer);
        MALLOC(j_fun_init, jac_nnz_init, integer);

        CUTEST_ccfsg(&cutest_status, &opt.n, &opt.m, opt.X, opt.G, &jac_nnz,
                     &jac_nnz_init, j_val_init, j_var_init, j_fun_init,
                     &fortran_true);
        wsp.DG.nnz = jac_nnz;
        /* Initialise dimension of permutation vector for sorting */
        wsp.DG.dim_perm = jac_nnz;
        wsp.DG.perm = NULL;

        InitWorhpMatrix(&wsp.DG, "DG", 0, par.MatrixCC, par.MatrixCC);
        if (status != OK) {
            WorhpError("Could not allocate DG structure", cutest_problem,
                       par.NLPprint);
            cnt.status = status;
        }

        memcpy(wsp.DG.row, j_fun_init, wsp.DG.nnz * sizeof(wsp.DG.row[0]));
        memcpy(wsp.DG.col, j_var_init, wsp.DG.nnz * sizeof(wsp.DG.col[0]));
        /* Tell Worhp to sort DG */
        SortWorhpMatrix(&wsp.DG);
    }

    /* To avoid invalid frees we initialise HMdimMiss in any case to zero */
    HMdimMiss = 0;
    /* Now the hessian */
    if (par.UserHM || par.FidifHM || par.BFGSmethod > 1) {
        /* Retrieve number of nonzeros in hessian from SIF */
        CUTEST_cdimsh(&cutest_status, &hm_nnz_init);
        if (cutest_status != 0) {
            WorhpError("Error retrieving number of nonzeroes in HM from CUTEst",
                       cutest_problem, par.NLPprint);
        }
        /* Allocate corresponding vectors for first initilization */
        MALLOC(h_val_init, hm_nnz_init, doublereal);
        MALLOC(h_row_init, hm_nnz_init, integer);
        MALLOC(h_col_init, hm_nnz_init, integer);

        /* Row is used as col and col as row, because sif gives */
        /* upper triangular part. Transposition is performed */
        /* by changing row and col. */
        if (opt.m > 0) {
            CUTEST_csh(&cutest_status, &opt.n, &opt.m, opt.X, opt.Mu, &hm_nnz,
                       &hm_nnz_init, h_val_init, h_col_init, h_row_init);
        } else {
            CUTEST_ush(&cutest_status, &opt.n, opt.X, &hm_nnz, &hm_nnz_init,
                       h_val_init, h_col_init, h_row_init);
        }
        wsp.HM.nnz = hm_nnz;
        /* Tell Worhp to initialise the permutation vector for hessian */
        wsp.HM.dim_perm = hm_nnz;
        wsp.HM.perm = NULL;

        /* Initialising the hessian, while taking care of our relaxation */
        /* variables. Extend must be specified in this case. */
        InitWorhpMatrix(&wsp.HM, "HM", wsp.RelaxNvar,
                        par.MatrixCC, par.MatrixCC);
        if (status != OK) {
            WorhpError("Could not allocate HM structure", cutest_problem,
                       par.NLPprint);
            cnt.status = status;
        }

        memcpy(wsp.HM.row, h_row_init, wsp.HM.nnz * sizeof(wsp.HM.row[0]));
        memcpy(wsp.HM.col, h_col_init, wsp.HM.nnz * sizeof(wsp.HM.col[0]));
        /* Tell Worhp to sort the hessian */
        SortWorhpMatrix(&wsp.HM);

        if (wsp.HM.nnz > opt.n) {
            if (wsp.HM.row[wsp.HM.nnz - opt.n] != 1 ||
                wsp.HM.col[wsp.HM.nnz - opt.n] != 1) {
                /* Determine number of missing elements, worst case O(opt.n) */
                /* linear effort is fine */
                for (i = wsp.HM.nnz - opt.n + 1; i < wsp.HM.nnz; i += 1) {
                    if (wsp.HM.row[i] == wsp.HM.col[i]) {
                        break;
                    }
                }
                /* Number of missing elements is: */
                HMdimMiss = opt.n - (wsp.HM.nnz - i);
            }
        } else {
            if (wsp.HM.nnz == 0) {
                HMdimMiss = opt.n;
            } else {
                /* Number of nonzeroes in HM is lower than opt.n. First step, */
                /* find index of first diagonal entry in this hessian */
                for (i = 0; i < wsp.HM.nnz + 1; i += 1) {
                    /* We need +1 because in case of no diag entries at all, */
                    /* otherwise loop would not help */
                    if (!(i > wsp.HM.nnz)) {
                        if (wsp.HM.row[i] == wsp.HM.col[i]) {
                            break;
                        }
                    }
                }
                /* Number of missing elements is: */
                HMdimMiss = opt.n - (wsp.HM.nnz - i);
            }
        }
        if (HMdimMiss > 0) {
            /* Permutation vector is only correct for elements before */
            /* diagonal last */
            MALLOC(HMpermOldIndices, opt.n - HMdimMiss, integer);

            wsp.HM.nnz += HMdimMiss;
            hm_extended_size = wsp.HM.nnz + wsp.RelaxNvar;
            wsp.HM.val = wRealloc(wsp.HM.val, hm_extended_size *
                                              sizeof(wsp.HM.val[0]));
            wsp.HM.row = wRealloc(wsp.HM.row, hm_extended_size *
                                              sizeof(wsp.HM.row[0]));
            wsp.HM.col = wRealloc(wsp.HM.col, hm_extended_size *
                                              sizeof(wsp.HM.col[0]));
            wsp.HM.dim_val = hm_extended_size;
            wsp.HM.dim_row = hm_extended_size;
            wsp.HM.dim_col = hm_extended_size;

            indexCount = 1;
            changeIndex = 0;
            for (i = wsp.HM.nnz - opt.n; i < wsp.HM.nnz; i += 1) {
                if (wsp.HM.row[i] != indexCount ||
                    wsp.HM.col[i] != indexCount) {
                    for (j = wsp.HM.nnz - 1; j > i; j -= 1) {
                        wsp.HM.row[j] = wsp.HM.row[j - 1];
                        wsp.HM.col[j] = wsp.HM.col[j - 1];
                    }
                    wsp.HM.row[i] = indexCount;
                    wsp.HM.col[i] = indexCount;
                    wsp.HM.val[i] = 0.0;
                } else {
                    /* Permutation vector HMperm now is not correct anymore */
                    /* because HM.val changed so saving the permutated */
                    /* indices  will help */
                    HMpermOldIndices[changeIndex] = i;
                    changeIndex += 1;
                }
                indexCount += 1;
            }

            /* Worhp will resize hessian during optimisation, we need to set */
            /* default again (happened in WorhpInit) */
            wsp.HM.nnzDefault = wsp.HM.nnz;
            wsp.HM.nRowDefault = wsp.HM.nRow;
            wsp.HM.nColDefault = wsp.HM.nCol;
        }
    }

    if (par.UserHM) {
        FREE(h_val_init);
        FREE(h_row_init);
        FREE(h_col_init);
    }
    if (par.UserDG && opt.m > 0) {
        FREE(j_val_init);
        FREE(j_var_init);
        FREE(j_fun_init);
        FREE(equatn);
        FREE(linear);
    }

    /* X, Mu and bounds are initialised by CUTEst setup */
    memset(opt.Lambda, 0, opt.n * sizeof(opt.Lambda[0]));

    neval_F = 0;
    neval_G = 0;
    neval_DF = 0;
    neval_DG = 0;
    neval_HM = 0;

    /*
     * WORHP Reverse Communication loop.
     * In every iteration poll GetUserAction for the requested action, i.e. one
     * of {callWorhp, iterOutput, evalF, evalG, evalDF, evalDG, evalHM, fidif}.
     *
     * Make sure to reset the requested user action afterwards by calling
     * DoneUserAction, except for 'callWorhp' and 'fidif'.
     */
    while (cnt.status < TerminateSuccess && cnt.status > TerminateError) {
        /*
         * WORHP's main routine.
         * Do not manually reset callWorhp, this is only done by the FD routines.
         */
        if (GetUserAction(&cnt, callWorhp)) {
            Worhp(&opt, &wsp, &par, &cnt);
            /* No DoneUserAction! */
        }

        /*
         * Show iteration output.
         * The call to IterationOutput() may be replaced by user-defined code.
         */
        if (GetUserAction(&cnt, iterOutput)) {
            IterationOutput(&opt, &wsp, &par, &cnt);
            DoneUserAction(&cnt, iterOutput);
        }

        /*
         * Evaluate the objective function.
         * The call to UserF may be replaced by user-defined code.
         */
        if (GetUserAction(&cnt, evalF)) {
            UserF(&opt, &wsp, &par, &cnt);
            neval_F += 1;
            DoneUserAction(&cnt, evalF);
        }

        /*
         * Evaluate the constraints.
         * The call to UserG may be replaced by user-defined code.
         */
        if (GetUserAction(&cnt, evalG)) {
            UserG(&opt, &wsp, &par, &cnt);
            neval_G += 1;
            DoneUserAction(&cnt, evalG);
        }

        /*
         * Evaluate the gradient of the objective function.
         * The call to UserDF may be replaced by user-defined code.
         */
        if (GetUserAction(&cnt, evalDF)) {
            UserDF(&opt, &wsp, &par, &cnt);
            neval_DF += 1;
            DoneUserAction(&cnt, evalDF);
        }

        /*
         * Evaluate the Jacobian of the constraints.
         * The call to UserDG may be replaced by user-defined code.
         */
        if (GetUserAction(&cnt, evalDG)) {
            UserDG(&opt, &wsp, &par, &cnt);
            neval_DG += 1;
            DoneUserAction(&cnt, evalDG);
        }

        /*
         * Evaluate the Hessian matrix of the Lagrange function (L = f + mu*g)
         * The call to UserHM may be replaced by user-defined code.
         */
        if (GetUserAction(&cnt, evalHM)) {
            UserHM(&opt, &wsp, &par, &cnt,
                   hm_nnz, hm_nnz_init, HMdimMiss, hm_extended_size,
                   HMpermOldIndices);
            neval_HM += 1;
            DoneUserAction(&cnt, evalHM);
        }

        /*
         * Use finite differences with RC to determine derivatives
         * Do not reset fidif, this is done by the FD routine.
         */
        if (GetUserAction(&cnt, fidif)) {
            WorhpFidif(&opt, &wsp, &par, &cnt);
            /* No DoneUserAction! */
        }
    }

    /*
     * Translate the WORHP status flag into a meaningful message.
     */
    StatusMsg(&opt, &wsp, &par, &cnt);

    if (par.NLPprint > -1) {
        snprintf(buffer, sizeof(buffer),
                 "Objective function evaluations .... %8i", neval_F);
        WorhpPrint(WORHP_PRINT_MESSAGE, buffer);
        snprintf(buffer, sizeof(buffer),
                 "Constraint function evaluations ... %8i", neval_G);
        WorhpPrint(WORHP_PRINT_MESSAGE, buffer);
        snprintf(buffer, sizeof(buffer),
                 "Gradient evaluations .............. %8i", neval_DF);
        WorhpPrint(WORHP_PRINT_MESSAGE, buffer);
        snprintf(buffer, sizeof(buffer),
                 "Jacobian evaluations .............. %8i", neval_DG);
        WorhpPrint(WORHP_PRINT_MESSAGE, buffer);
        snprintf(buffer, sizeof(buffer),
                 "Hessian evaluations ............... %8i", neval_HM);
        WorhpPrint(WORHP_PRINT_MESSAGE, buffer);
        snprintf(buffer, sizeof(buffer),
                 "Overall minor (QP) iterations ..... %8i",
                 wsp.MinorIterOverall);
        WorhpPrint(WORHP_PRINT_MESSAGE, buffer);
        snprintf(buffer, sizeof(buffer),
                 "Overall feas. refinement iter. .... %8i",
                 wsp.RefineFeasIterOverall);
        WorhpPrint(WORHP_PRINT_MESSAGE, buffer);
    }

    /* Deallocate SIF / Cutest memory */
    if (opt.m > 0) {
        CUTEST_cterminate(&cutest_status);
    } else {
        CUTEST_uterminate(&cutest_status);
    }
    if (HMdimMiss > 0) {
        FREE(HMpermOldIndices);
    }

    /*
     * Deallocate all data structures.
     * Data structures must not be accessed after this call.
     */
    WorhpFree(&opt, &wsp, &par, &cnt);
}


void UserF(OptVar *opt, Workspace *wsp, Params *par, Control *cnt) {
    integer cutest_status;
    logical fortran_false = FALSE_;
    doublereal *df_dummy;

    MALLOC(df_dummy, opt->n, doublereal);

    if (opt->m > 0) {
       CUTEST_cofg(&cutest_status, &opt->n, opt->X, &opt->F, df_dummy,
                   &fortran_false);
    } else {
       CUTEST_ufn(&cutest_status, &opt->n, opt->X, &opt->F);
    }
    if (cutest_status != 0) {
        WorhpError("Error evaluating objective function.", "CUTEst",
                   par->NLPprint);
    }
    opt->F = wsp->ScaleObj * opt->F;

    FREE(df_dummy);
}

void UserG(OptVar *opt, Workspace *wsp, Params *par, Control *cnt) {
    integer cutest_status, nnzj, nnzjN;
    logical fortran_false = FALSE_;
    doublereal *j_val;
    integer *j_var;
    integer *j_fun;

    MALLOC(j_val, wsp->DG.nnz, doublereal);
    MALLOC(j_var, wsp->DG.nnz, integer);
    MALLOC(j_fun, wsp->DG.nnz, integer);

    CUTEST_ccfsg(&cutest_status, &opt->n, &opt->m, opt->X, opt->G,
                 &nnzjN, &nnzj, j_val, j_var, j_fun, &fortran_false);
    if (cutest_status != 0) {
       WorhpError("Error evaluating objective function.", "CUTEst",
                  par->NLPprint);
    }

    FREE(j_val);
    FREE(j_var);
    FREE(j_fun);
}

void UserDF(OptVar *opt, Workspace *wsp, Params *par, Control *cnt) {
    integer cutest_status;
    logical fortran_true = TRUE_;
    doublereal f_dummy;
    int i;

    if (opt->m > 0) {
       CUTEST_cofg(&cutest_status, &opt->n, opt->X, &f_dummy, wsp->DF.val,
                   &fortran_true);
    } else {
       CUTEST_ugr(&cutest_status, &opt->n, opt->X, wsp->DF.val);
    }

    for (i = 0; i < wsp->DF.nnz; i += 1) {
        wsp->DF.val[i] = wsp->ScaleObj * wsp->DF.val[i];
    }
}

void UserDG(OptVar *opt, Workspace *wsp, Params *par, Control *cnt) {
    integer cutest_status, nnzj, nnzjN;
    logical fortran_true = TRUE_;
    doublereal *g_dummy;
    doublereal *DGaux;
    doublereal *j_val;
    integer *j_var;
    integer *j_fun;
    int i;

    MALLOC(g_dummy, opt->m, doublereal);
    MALLOC(DGaux, wsp->DG.nnz, doublereal);
    MALLOC(j_val, wsp->DG.nnz, doublereal);
    MALLOC(j_var, wsp->DG.nnz, integer);
    MALLOC(j_fun, wsp->DG.nnz, integer);

    nnzj = wsp->DG.nnz;
    CUTEST_ccfsg(&cutest_status, &opt->n, &opt->m, opt->X, g_dummy,
                 &nnzjN, &nnzj, DGaux, j_var, j_fun, &fortran_true);
    if (cutest_status != 0) {
       WorhpError("Failed to evaluate DG.", "CUTEST_ccfsg", par->NLPprint);
    }

    for (i = 0; i < wsp->DG.nnz; i += 1) {
        wsp->DG.val[i] = DGaux[wsp->DG.perm[i] - 1];
    }

    FREE(g_dummy);
    FREE(DGaux);
    FREE(j_val);
    FREE(j_var);
    FREE(j_fun);
}

void UserHM(OptVar *opt, Workspace *wsp, Params *par, Control *cnt,
            integer hm_nnz, integer hm_nnz_init, integer HMdimMiss,
            integer hm_extended_size, integer *HMpermOldIndices) {
    integer cutest_status;
    integer objective_part = 0;
    doublereal *h_val_init = NULL;
    integer *h_row_init = NULL;
    integer *h_col_init = NULL;
    int kk, jj;

    MALLOC(h_val_init, hm_nnz, doublereal);
    MALLOC(h_row_init, hm_nnz, integer);
    MALLOC(h_col_init, hm_nnz, integer);

    /* reset everything to zero cause we cannot be sure that the zeros on the */
    /* diagonal are still zero, cause worhp regularizes the hessian */
    memset(wsp->HM.val, 0, wsp->HM.nnz * sizeof(wsp->HM.val[0]));

    /* Evaluate F-Part of hessian of the lagrangian */
    if (opt->m > 0) {
       CUTEST_cish(&cutest_status, &opt->n, opt->X, &objective_part,
                   &hm_nnz, &hm_nnz_init, h_val_init, h_col_init, h_row_init);
    } else {
       CUTEST_ush(&cutest_status, &opt->n, opt->X, &hm_nnz, &hm_nnz_init,
                  h_val_init, h_col_init, h_row_init);
    }

    if (hm_nnz < wsp->HM.nnz) {
        /* In this case we found missing diagonal entries */
        /* Insane permutation changes require this stuff */
        for (kk = 0; kk < (hm_nnz - (opt->n - HMdimMiss)); kk += 1) {
            wsp->HM.val[kk] = wsp->ScaleObj * h_val_init[wsp->HM.perm[kk] - 1];
        }
        jj = 0;
        for (kk = (hm_nnz - (opt->n - HMdimMiss)); kk < hm_nnz; kk += 1) {
            wsp->HM.val[HMpermOldIndices[jj]] =
                wsp->ScaleObj * h_val_init[wsp->HM.perm[kk] - 1];
            jj += 1;
        }
    } else {
        for (kk = 0; kk < hm_nnz; kk += 1) {
            wsp->HM.val[kk] = wsp->ScaleObj * h_val_init[wsp->HM.perm[kk] - 1];
        }
    }

    if (opt->m > 0) {
        /* Evaluate G-Part of hessian of the lagrangian */
        CUTEST_cshc(&cutest_status, &opt->n, &opt->m, opt->X, opt->Mu,
                    &hm_nnz, &hm_nnz_init, h_val_init, h_col_init, h_row_init);
        if (hm_nnz < wsp->HM.nnz) {
            /* In this case we found missing diagonal entries */
            /* Insane permutation changes require this stuff */
            for (kk = 0; kk < (hm_nnz - (opt->n - HMdimMiss)); kk += 1) {
                wsp->HM.val[kk] += h_val_init[wsp->HM.perm[kk] - 1];
            }
            jj = 0;
            for (kk = (hm_nnz - (opt->n - HMdimMiss)); kk < hm_nnz; kk += 1) {
                wsp->HM.val[HMpermOldIndices[jj]] +=
                    h_val_init[wsp->HM.perm[kk] - 1];
                jj += 1;
            }
        } else {
            for (kk = 0; kk < hm_nnz; kk += 1) {
                wsp->HM.val[kk] += h_val_init[wsp->HM.perm[kk] - 1];
            }
        }
    }

    FREE(h_val_init);
    FREE(h_row_init);
    FREE(h_col_init);
}
