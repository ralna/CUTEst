/* THIS VERSION: CUTEST 2.3 - 2024-10-11 AT 09:00 GMT */

#ifndef THIS_PROBLEM_NAME
#define THIS_PROBLEM_NAME "WORHP/CUTEst"
#endif

#ifndef OUTSDIFD_FILENAME
#define OUTSDIFD_FILENAME "OUTSDIF.d"
#endif

/* comment out to enable assertions */
#define NDEBUG

#include "cutest.h"
#include "cutest_routines.h"
#include "worhp.h"
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Declare user functions, implementation later */
/* Objective function */
void UserF(OptVar *opt, Workspace *wsp, Params *par, Control *cnt,
           rp_ *g_val_dummy);
/* Function of constraints */
void UserG(OptVar *opt, Workspace *wsp, Params *par, Control *cnt,
           rp_ *j_val, integer *j_var, integer *j_fun);
/* Objective function and function of constraints simultaneously */
void UserF_G(OptVar *opt, Workspace *wsp, Params *par, Control *cnt);
/* Gradient of objective function */
void UserDF(OptVar *opt, Workspace *wsp, Params *par, Control *cnt,
            rp_ *g_val, integer *g_var);
/* Jacobian of constraints */
void UserDG(OptVar *opt, Workspace *wsp, Params *par, Control *cnt,
            rp_ *j_val, integer *j_var, integer *j_fun,
            rp_ *c_dummy);
/* Gradient of objective function and Jacobian of constraints simultaneously */
void UserDF_DG(OptVar *opt, Workspace *wsp, Params *par, Control *cnt,
               rp_ *j_val, integer *j_var, integer *j_fun);
/* Hessian of Lagrangian */
void UserHM(OptVar *opt, Workspace *wsp, Params *par, Control *cnt,
            rp_ *h_val, integer *h_row, integer *h_col,
            integer hm_nnz, integer hm_nnz_init, integer *hm_perm_inverse);


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

    int status, i;

    /* CUTEst related stuff */
    const logical evaluate_derivative = TRUE_;
    const integer order_none = 0;
    const integer cutest_input = 60;
    const integer io_buffer = 11;
    const integer out = 6;
    integer cutest_status, cutest_m, jac_nnz, jac_nnz_init, g_nnz;
    logical *equatn = NULL;
    logical *linear = NULL;

    rp_ *j_val = NULL;
    rp_ *h_val = NULL;
    rp_ *g_val = NULL;
    rp_ *c_dummy = NULL;
    integer *j_fun = NULL;
    integer *j_var = NULL;
    integer *h_row = NULL;
    integer *h_col = NULL;
    integer *g_var = NULL;

    integer hm_nnz, hm_nnz_init;
    integer *hm_perm_inverse;
    bool *is_diag_entry_present;
    integer num_missing_diagonal;
    integer i_diagonal;

    /* Counter */
    integer neval_F, neval_G, neval_DF, neval_DG, neval_HM;

    /* Adapt these as appropriate */
    const char cutest_problem[STRING_LENGTH] = THIS_PROBLEM_NAME;
    const char *outsdifd_filename = OUTSDIFD_FILENAME;
    /* char buffer[STRING_LENGTH]; */
    char buffer[STRING_LENGTH+20];

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
    CUTEST_cdimen(&cutest_status, &cutest_input, &opt.n, &cutest_m);
    if (cutest_status != 0) {
        return cutest_status;
    }
    opt.m = cutest_m;

    if (opt.m > 0) {
        /* If general constraints are present DG will be allocated further on */
        /* through the cutest sparse DG-functions */
        wsp.DG.nnz = WorhpMatrix_Dont_Allocate;
        wsp.DF.nnz = WorhpMatrix_Dont_Allocate;
    } else {
        /* If the problem is generally unconstrained WORHP requires dg.nnz to */
        /* be WorhpMatrix_Init_Dense */
        wsp.DG.nnz = WorhpMatrix_Init_Dense;
        wsp.DF.nnz = WorhpMatrix_Init_Dense;
    }

    wsp.HM.nnz = WorhpMatrix_Dont_Allocate;

    WorhpInit(&opt, &wsp, &par, &cnt);
    if (cnt.status != FirstCall) {
        WorhpError("Initialisation failed", cutest_problem, par.NLPprint);
        return EXIT_FAILURE;
    }

    if (opt.m > 0) {
        /* Constrained case */
        MALLOC(equatn, cutest_m, logical);
        MALLOC(linear, cutest_m, logical);
        MALLOC(c_dummy, cutest_m, rp_);

        CUTEST_csetup(&cutest_status, &cutest_input, &out, &io_buffer,
                        &opt.n, &cutest_m, opt.X, opt.XL, opt.XU,
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
        /*
         * Initialise structure of gradient of objective function
         */
        MALLOC(g_var, opt.n, integer);
        MALLOC(g_val, opt.n, rp_);

        CUTEST_cofsg(&cutest_status, &opt.n, opt.X, &opt.F, &g_nnz, &opt.n,
                       g_val, g_var, &evaluate_derivative);

        wsp.DF.nnz = g_nnz;
        wsp.DF.dim_perm = g_nnz;
        wsp.DF.perm = NULL;

        status = InitWorhpMatrix(&wsp.DF, "DF", 0, par.MatrixCC, par.MatrixCC);
        if (status != OK) {
            WorhpError("Could not allocate DF structure", cutest_problem,
                       par.NLPprint);
            cnt.status = status;
        }

        memcpy(wsp.DF.row, g_var, wsp.DF.nnz * sizeof(wsp.DF.row[0]));
        /* Tell Worhp to sort DG */
        SortWorhpMatrix(&wsp.DF);


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
        MALLOC(j_val, jac_nnz_init, rp_);
        MALLOC(j_var, jac_nnz_init, integer);
        MALLOC(j_fun, jac_nnz_init, integer);

        CUTEST_ccfsg(&cutest_status, &opt.n, &cutest_m, opt.X, opt.G,
                       &jac_nnz, &jac_nnz_init, j_val, j_var, j_fun,
                       &evaluate_derivative);
        assert(jac_nnz + opt.n == jac_nnz_init);
        wsp.DG.nnz = jac_nnz;
        /* Initialise dimension of permutation vector for sorting */
        wsp.DG.dim_perm = jac_nnz;
        wsp.DG.perm = NULL;

        status = InitWorhpMatrix(&wsp.DG, "DG", 0, par.MatrixCC, par.MatrixCC);
        if (status != OK) {
            WorhpError("Could not allocate DG structure", cutest_problem,
                       par.NLPprint);
            cnt.status = status;
        }

        memcpy(wsp.DG.row, j_fun, wsp.DG.nnz * sizeof(wsp.DG.row[0]));
        memcpy(wsp.DG.col, j_var, wsp.DG.nnz * sizeof(wsp.DG.col[0]));
        /* Tell Worhp to sort DG */
        SortWorhpMatrix(&wsp.DG);
    }

    /* Now the hessian */
    if (par.UserHM || par.FidifHM || par.BFGSmethod > 1) {
        /* Retrieve number of nonzeros in hessian from SIF */
        if (opt.m > 0) {
            CUTEST_cdimsh(&cutest_status, &hm_nnz_init);
        } else {
            CUTEST_udimsh(&cutest_status, &hm_nnz_init);
        }
        if (cutest_status != 0) {
            WorhpError("Error retrieving number of nonzeroes in HM from CUTEst",
                       cutest_problem, par.NLPprint);
        }
        /* Allocate corresponding vectors for first initilization */
        MALLOC(h_val, hm_nnz_init, rp_);
        MALLOC(h_row, hm_nnz_init, integer);
        MALLOC(h_col, hm_nnz_init, integer);

        /* Row is used as col and col as row, because sif gives */
        /* upper triangular part. Transposition is performed */
        /* by changing row and col. */
        if (opt.m > 0) {
            CUTEST_cshp(&cutest_status, &opt.n,
                          &hm_nnz, &hm_nnz_init, h_col, h_row);
        } else {
            CUTEST_ushp(&cutest_status, &opt.n,
                          &hm_nnz, &hm_nnz_init, h_col, h_row);
        }
        is_diag_entry_present = calloc(opt.n, sizeof(bool));
        num_missing_diagonal = opt.n;
        for (i = 0; i < hm_nnz; i += 1) {
            if (h_row[i] == h_col[i]) {
                is_diag_entry_present[h_row[i] - 1] = true;
                num_missing_diagonal -= 1;
            }
        }
        wsp.HM.nnz = hm_nnz + num_missing_diagonal;
        wsp.HM.dim_perm = wsp.HM.nnz;
        wsp.HM.perm = NULL;

        /* Initialising the hessian, while taking care of our relaxation */
        /* variables. Extend must be specified in this case. */
        status = InitWorhpMatrix(&wsp.HM, "HM", wsp.RelaxNvar,
                                 par.MatrixCC, par.MatrixCC);
        if (status != OK) {
            WorhpError("Could not allocate HM structure", cutest_problem,
                       par.NLPprint);
            cnt.status = status;
        }

        memcpy(wsp.HM.row, h_row, hm_nnz * sizeof(wsp.HM.row[0]));
        memcpy(wsp.HM.col, h_col, hm_nnz * sizeof(wsp.HM.col[0]));
        i_diagonal = 0;
        for (i = 0; i < num_missing_diagonal; i += 1) {
            while (is_diag_entry_present[i_diagonal]) {
                i_diagonal += 1;
            }
            wsp.HM.row[hm_nnz + i] = (i_diagonal + 1);
            wsp.HM.col[hm_nnz + i] = (i_diagonal + 1);
            i_diagonal += 1;
        }
        free(is_diag_entry_present);
        /* Tell Worhp to sort the hessian */
        SortWorhpMatrix(&wsp.HM);
        /* Create inverse permutation */
        hm_perm_inverse = malloc(wsp.HM.dim_perm * sizeof(hm_perm_inverse[0]));
        for (i = 0; i < wsp.HM.dim_perm; i += 1) {
            hm_perm_inverse[wsp.HM.perm[i] - 1] = i;
        }
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
     */
    while (cnt.status < TerminateSuccess && cnt.status > TerminateError) {
        if (GetUserAction(&cnt, callWorhp)) {
            Worhp(&opt, &wsp, &par, &cnt);
            /* No DoneUserAction! */
        }

        if (GetUserAction(&cnt, iterOutput)) {
            IterationOutput(&opt, &wsp, &par, &cnt);
            DoneUserAction(&cnt, iterOutput);
        }

        if (GetUserAction(&cnt, evalF) && GetUserAction(&cnt, evalG)) {
            UserF_G(&opt, &wsp, &par, &cnt);
            neval_F += 1;
            neval_G += 1;
            DoneUserAction(&cnt, evalF);
            DoneUserAction(&cnt, evalG);
        }

        if (GetUserAction(&cnt, evalF)) {
            UserF(&opt, &wsp, &par, &cnt, g_val);
            neval_F += 1;
            DoneUserAction(&cnt, evalF);
        }

        if (GetUserAction(&cnt, evalG)) {
            UserG(&opt, &wsp, &par, &cnt, j_val, j_var, j_fun);
            neval_G += 1;
            DoneUserAction(&cnt, evalG);
        }

        if (GetUserAction(&cnt, evalDF) && GetUserAction(&cnt, evalDG)) {
            UserDF_DG(&opt, &wsp, &par, &cnt, j_val, j_var, j_fun);
            neval_DF += 1;
            neval_DG += 1;
            DoneUserAction(&cnt, evalDF);
            DoneUserAction(&cnt, evalDG);
        }

        if (GetUserAction(&cnt, evalDF)) {
            UserDF(&opt, &wsp, &par, &cnt, g_val, g_var);
            neval_DF += 1;
            DoneUserAction(&cnt, evalDF);
        }

        if (GetUserAction(&cnt, evalDG)) {
            UserDG(&opt, &wsp, &par, &cnt, j_val, j_var, j_fun, c_dummy);
            neval_DG += 1;
            DoneUserAction(&cnt, evalDG);
        }

        if (GetUserAction(&cnt, evalHM)) {
            UserHM(&opt, &wsp, &par, &cnt, h_val, h_row, h_col,
                   hm_nnz, hm_nnz_init, hm_perm_inverse);
            neval_HM += 1;
            DoneUserAction(&cnt, evalHM);
        }

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
    if (par.UserHM || par.FidifHM || par.BFGSmethod > 1) {
        FREE(h_val);
        FREE(h_row);
        FREE(h_col);
        free(hm_perm_inverse);
    }
    if (opt.m > 0) {
        FREE(equatn);
        FREE(linear);
        FREE(c_dummy);
        FREE(g_var);
        FREE(g_val);
        FREE(j_val);
        FREE(j_var);
        FREE(j_fun);
    }

    /*
     * Deallocate all data structures.
     * Data structures must not be accessed after this call.
     */
    WorhpFree(&opt, &wsp, &par, &cnt);

    return 0;
}


void UserF(OptVar *opt, Workspace *wsp, Params *par, Control *cnt,
           rp_ *g_val_dummy) {
    const logical evaluate_gradient = FALSE_;
    integer cutest_status;

    if (opt->m > 0) {
        CUTEST_cofg(&cutest_status, &opt->n, opt->X, &opt->F, g_val_dummy,
                      &evaluate_gradient);
    } else {
        CUTEST_ufn(&cutest_status, &opt->n, opt->X, &opt->F);
    }
    if (cutest_status != 0) {
        WorhpError("Error evaluating objective function.", "CUTEst",
                   par->NLPprint);
    }
    opt->F = wsp->ScaleObj * opt->F;
}

void UserG(OptVar *opt, Workspace *wsp, Params *par, Control *cnt,
           rp_ *j_val, integer *j_var, integer *j_fun) {
    const logical evaluate_jacobian = FALSE_;
    const integer cutest_m = opt->m;
    integer cutest_status, nnzj, nnzjN;

    CUTEST_ccfsg(&cutest_status, &opt->n, &cutest_m, opt->X, opt->G,
                   &nnzjN, &nnzj, j_val, j_var, j_fun, &evaluate_jacobian);
    if (cutest_status != 0) {
        WorhpError("Error evaluating constraint functions.", "CUTEst",
                   par->NLPprint);
    }
}

void UserF_G(OptVar *opt, Workspace *wsp, Params *par, Control *cnt) {
    const integer cutest_m = opt->m;
    integer cutest_status;

    assert(opt->m > 0);
    CUTEST_cfn(&cutest_status, &opt->n, &cutest_m, opt->X, &opt->F, opt->G);
    if (cutest_status != 0) {
        WorhpError("Error evaluating objective and constraint functions "
                   "simultaneously.", "CUTEst", par->NLPprint);
    }
    opt->F = wsp->ScaleObj * opt->F;
}

void UserDF(OptVar *opt, Workspace *wsp, Params *par, Control *cnt,
            rp_ *g_val, integer *g_var) {
    const logical evaluate_gradient = TRUE_;
    integer cutest_status;
    rp_ f_dummy;
    int i, g_nnz;

    if (opt->m > 0) {
        CUTEST_cofsg(&cutest_status, &opt->n, opt->X, &f_dummy,
                       &g_nnz, &wsp->DF.nnz, g_val, g_var, &evaluate_gradient);
        assert(g_nnz == wsp->DF.nnz);
        for (i = 0; i < wsp->DF.nnz; i += 1) {
            assert(wsp->DF.row[i] == g_var[wsp->DF.perm[i] - 1]);
            wsp->DF.val[i] = g_val[wsp->DF.perm[i] - 1];
        }
    } else {
        CUTEST_ugr(&cutest_status, &opt->n, opt->X, wsp->DF.val);
    }
    if (cutest_status != 0) {
        WorhpError("Error evaluating gradient of objective function", "CUTEst",
                   par->NLPprint);
    }

    for (i = 0; i < wsp->DF.nnz; i += 1) {
        wsp->DF.val[i] = wsp->ScaleObj * wsp->DF.val[i];
    }
}

void UserDG(OptVar *opt, Workspace *wsp, Params *par, Control *cnt,
            rp_ *j_val, integer *j_var, integer *j_fun,
            rp_ *c_dummy) {
    const logical evaluate_jacobian = TRUE_;
    const integer cutest_m = opt->m;
    integer cutest_status, allocated_nnz, written_nnz;
    int i;

    allocated_nnz = wsp->DG.nnz + opt->n;
    CUTEST_ccfsg(&cutest_status, &opt->n, &cutest_m, opt->X, c_dummy,
                   &written_nnz, &allocated_nnz, j_val, j_var, j_fun,
                   &evaluate_jacobian);
    if (cutest_status != 0) {
        WorhpError("Failed to evaluate DG.", "CUTEST_ccfsg", par->NLPprint);
    }
    assert(written_nnz == wsp->DG.nnz);

    for (i = 0; i < wsp->DG.nnz; i += 1) {
        assert(wsp->DG.row[i] == j_fun[wsp->DG.perm[i] - 1]);
        assert(wsp->DG.col[i] == j_var[wsp->DG.perm[i] - 1]);
        wsp->DG.val[i] = j_val[wsp->DG.perm[i] - 1];
    }
}

void UserDF_DG(OptVar *opt, Workspace *wsp, Params *par, Control *cnt,
               rp_ *j_val, integer *j_var, integer *j_fun) {
    const logical gradient_of_lagrangian = FALSE_;
    const integer objective_part = 0;
    const integer cutest_m = opt->m;
    integer cutest_status, allocated_nnz, written_nnz;
    int i;
    int i_DF;

    assert(opt->m > 0);
    allocated_nnz = wsp->DG.nnz + opt->n;
    CUTEST_csgr(&cutest_status, &opt->n, &cutest_m, opt->X, opt->Mu,
                  &gradient_of_lagrangian, &written_nnz, &allocated_nnz,
                  j_val, j_var, j_fun);
    if (cutest_status != 0) {
        WorhpError("Failed to evaluate DF and DG simultaneously.",
                   "CUTEST_csgr", par->NLPprint);
    }
    assert(written_nnz == allocated_nnz);

    for (i = 0; i < wsp->DG.nnz; i += 1) {
        assert(wsp->DG.row[i] == j_fun[wsp->DG.perm[i] - 1]);
        assert(wsp->DG.col[i] == j_var[wsp->DG.perm[i] - 1]);
        wsp->DG.val[i] = j_val[wsp->DG.perm[i] - 1];
    }

    /* The gradient of the objective function is stored dense at the end of
     * j_val, j_var and j_fun when using csgr. Copy it into WORHP's wsp->DF. */
    i_DF = 0;
    for (i = wsp->DG.nnz; i < written_nnz; i += 1) {
        assert(j_fun[i] == objective_part);
        assert(j_var[i] == i - wsp->DG.nnz + 1);
        if (i_DF >= wsp->DF.nnz) {
            /* All nnz of wsp->DF have been set, so we can exit */
            break;
        }
        if (wsp->DF.row[i_DF] == j_var[i]) {
            wsp->DF.val[i_DF] = wsp->ScaleObj * j_val[i];
            i_DF += 1;
        }
    }
}

void UserHM(OptVar *opt, Workspace *wsp, Params *par, Control *cnt,
            rp_ *h_val, integer *h_row, integer *h_col,
            integer hm_nnz, integer hm_nnz_init, integer *hm_perm_inverse) {
    const integer objective_part = 0;
    const integer cutest_m = opt->m;
    integer cutest_status;
    int i;

    /* reset everything to zero cause we cannot be sure that the zeros on the */
    /* diagonal are still zero, cause worhp regularizes the hessian */
    memset(wsp->HM.val, 0, wsp->HM.nnz * sizeof(wsp->HM.val[0]));

    /* Evaluate F-Part of hessian of the lagrangian */
    if (opt->m > 0) {
        CUTEST_cish(&cutest_status, &opt->n, opt->X, &objective_part,
                    &hm_nnz, &hm_nnz_init, h_val, h_col, h_row);
    } else {
        CUTEST_ush(&cutest_status, &opt->n, opt->X, &hm_nnz, &hm_nnz_init,
                     h_val, h_col, h_row);
    }

    for (i = 0; i < hm_nnz; i += 1) {
        assert(wsp->HM.row[hm_perm_inverse[i]] == h_row[i]);
        assert(wsp->HM.col[hm_perm_inverse[i]] == h_col[i]);
        wsp->HM.val[hm_perm_inverse[i]] = wsp->ScaleObj * h_val[i];
    }

    if (opt->m > 0) {
        /* Evaluate G-Part of hessian of the lagrangian */
        CUTEST_cshc(&cutest_status, &opt->n, &cutest_m, opt->X, opt->Mu,
                      &hm_nnz, &hm_nnz_init, h_val, h_col, h_row);
        for (i = 0; i < hm_nnz; i += 1) {
            assert(wsp->HM.row[hm_perm_inverse[i]] == h_row[i]);
            assert(wsp->HM.col[hm_perm_inverse[i]] == h_col[i]);
            wsp->HM.val[hm_perm_inverse[i]] += h_val[i];
        }
    }
}
