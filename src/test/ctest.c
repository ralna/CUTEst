// THIS VERSION: CUTEST 2.3 - 2024-10-17 AT 14:00 GMT.

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "cutest_routines.h"
#include "cutest.h"

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

// Function prototypes
void write_x(ipc_ n, rpc_ *X, rpc_ *X_l, rpc_ *X_u);
void write_y(ipc_ m, rpc_ *Y, rpc_ *C_l, rpc_ *C_u, 
             logical *equation, logical *linear);
void write_x_type(ipc_ n, ipc_ *X_type);
void write_f(rpc_ f);
void write_g(ipc_ n, rpc_ *G);
void write_g_sparsity_pattern(ipc_ g_ne, ipc_ *g_var);
void write_sg(ipc_ g_ne, ipc_ *g_var, rpc_ *g_val);
void write_c(ipc_ m, rpc_ *C);
void write_ci(ipc_ icon, rpc_ ci);
void write_ji(ipc_ n, ipc_ icon, rpc_ *ji);
void write_sji(ipc_ icon, ipc_ ji_ne, rpc_ *ji, ipc_ *j_var);
void write_j_dense(ipc_ n, ipc_ m, ipc_ l_j2_1, ipc_ l_j2_2, rpc_ *J2_val);
void write_jt_dense(ipc_ n, ipc_ m, ipc_ l_j2_1, ipc_ l_j2_2, rpc_ *J2_val);
void write_j_sparsity_pattern(ipc_ J_ne, ipc_ *J_row, ipc_ *J_col);
void write_j_sparse(ipc_ J_ne, rpc_ *J_val, ipc_ *J_row, ipc_ *J_col);
void write_h_dense(ipc_ n, ipc_ l_h2_1, rpc_ *H2_val);
void write_h_sparsity_pattern(ipc_ H_ne, ipc_ *H_row, ipc_ *H_col);
void write_h_sparse(ipc_ H_ne, rpc_ *H_val, ipc_ *H_row, ipc_ *H_col);
void write_ohp_sparsity(ipc_ OHP_ne, ipc_ *OHP_ind);
void write_ohp(ipc_ OHP_ne, rpc_ *OHP_val, ipc_ *OHP_ind);
void write_chp_sparsity(ipc_ m, ipc_ *OHP_ind, ipc_ *OHP_ptr);
void write_chp(ipc_ m, rpc_ *CHP_val, ipc_ *OHP_ind, ipc_ *OHP_ptr);
void write_h_element(ipc_ ne, ipc_ *HE_row_ptr, ipc_ *HE_val_ptr, 
                     ipc_ *HE_row, rpc_ *HE_val);
void write_result(ipc_ n, rpc_ *vector, rpc_ *result);
void write_result2(ipc_ n_vector, rpc_ *vector, ipc_ n_result, rpc_ *result);
void write_sresult(ipc_ nnz_vector, ipc_ *INDEX_nz_vector, rpc_ *vector, 
                   ipc_ nnz_result, ipc_ *INDEX_nz_result, rpc_ *result);

#include "cutest_modules.h"
#include "cutest_routines.h"

int main() {
    // CUTEst data file
    char *fname = "c_OUTSDIF.d\0";

    // Parameters
    ipc_ input = 55;
    ipc_ out = 6;
    ipc_ buffer = 77;
    ipc_ i1 = 1;

    // Local Variables
    ipc_ i, j, n, m, HE_nel, HE_val_ne, HE_row_ne, J_ne, Ji_ne, status;
    ipc_ G_ne, l_g, l_h2_1, l_h, lhe_ptr, H_ne, lhe_val, lhe_row, l_ohp;
    ipc_ CHP_ne, l_chp, OHP_ne, nnz_vector, nnz_result;
    ipc_ l_j2_1, l_j2_2, l_j, icon, iprob;
    ipc_ nonlinear_variables_objective, nonlinear_variables_constraints;
    ipc_ equality_constraints, linear_constraints;

    rpc_ f, ci, y0;
    logical byrows;
    logical grad;
    logical *equation, *linear;
    bool goth, gotj, grlagf, jtrans, noobj;
    ipc_ *X_type, *G_var, *J_var, *J_fun;
    ipc_ *H_row, *H_col, *HE_row, *HE_row_ptr, *HE_val_ptr;
    ipc_ *OHP_ind, *CHP_ind, *CHP_ptr;
    ipc_ *INDEX_nz_vector, *INDEX_nz_result;

    rpc_ *X, *X_l, *X_u, *G, *C, *C_l, *C_u, *Y, *G_val, *Ji;
    rpc_ *H_val, *HE_val, *J_val, *CHP_val, *OHP_val, *H2_val, *J2_val;
    rpc_ *vector, *result;
    char *classification, *p_name, *cptr;
    char *X_names_fortran, *C_names_fortran;
    char **X_names, **C_names;
    rpc_ CPU[4], CALLS[7];

    printf("in\n");
    // Open the problem data file
    FORTRAN_open_r(&input, fname, &status);

    // Determine problem dimensions
    printf("CALL CUTEST_cdimen\n");
    CUTEST_cdimen_r( &status, &input, &n, &m );
    printf("* n = %d, m = %d\n", n, m );
    printf("CALL CUTEST_cnoobj\n");
    CUTEST_cnoobj_r( &status, &input, &noobj );
    if (noobj) {
      printf("there is no objective function\n");
    }
    else {
      printf("there is an objective function\n");
    }

    // allocate basic arrays
    MALLOC(p_name, FSTRING_LEN + 1, char);
    MALLOC(X_names_fortran, n * ( FSTRING_LEN + 1 ), char); /* For Fortran */
    MALLOC(X_names, n, char *);
    for (i = 0; i < n; i++)
        MALLOC(X_names[i], FSTRING_LEN + 1, char);
    MALLOC(C_names_fortran, m * ( FSTRING_LEN + 1 ), char); /* For Fortran */
    MALLOC(C_names, m, char *);
    for (i = 0; i < m; i++)
        MALLOC(C_names[i], FSTRING_LEN + 1, char);
    if (p_name == NULL || X_names_fortran == NULL || X_names == NULL ||
        C_names_fortran == NULL || C_names == NULL) {
        perror("Error allocating memory for chars");
        return 1;
    }

    X = malloc(n * sizeof(rpc_));
    X_l = malloc(n * sizeof(rpc_));
    X_u = malloc(n * sizeof(rpc_));
    G = malloc(n * sizeof(rpc_));
    Ji = malloc(n * sizeof(rpc_));
    X_type = malloc(n * sizeof(ipc_));
    INDEX_nz_vector = malloc(n * sizeof(ipc_));
    INDEX_nz_result = malloc(n * sizeof(ipc_));

    C = malloc(m * sizeof(rpc_));
    C_l = malloc(m * sizeof(rpc_));
    C_u = malloc(m * sizeof(rpc_));
    Y = malloc(m * sizeof(rpc_));
    equation = malloc(m * sizeof(logical));
    linear = malloc(m * sizeof(logical));

    ipc_ maxmn = MAX( m, n );
    vector = malloc(maxmn * sizeof(rpc_));
    result = malloc(maxmn * sizeof(rpc_));

    l_h2_1 = n;
    H2_val = malloc( l_h2_1 * n * sizeof(rpc_));
    l_j2_1 = maxmn;
    l_j2_2 = l_j2_1;
    J2_val = malloc( l_j2_1 * l_j2_2 * sizeof(rpc_));
    if (X == NULL || X_l == NULL || X_u == NULL || G == NULL ||
        C == NULL || C_l == NULL || C_u == NULL || Y == NULL ||
        vector == NULL || result == NULL || X_names == NULL ||
        X_type == NULL || Ji == NULL || equation == NULL ||
        linear == NULL || INDEX_nz_vector == NULL || 
        INDEX_nz_result == NULL || H2_val == NULL ||
        H2_val == NULL || J2_val == NULL) {
        perror("Error allocating memory for ints and reals");
        return 1;
    }

    // obtain the classification
    printf("CALL CUTEST_classification \n");
    MALLOC(classification, FCSTRING_LEN + 1, char);
    CUTEST_classification_r( &status, &input, classification );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    printf("* classification: %-s\n", classification);

    // set up SIF data
    printf("CALL CUTEST_csetup\n");
    CUTEST_csetup_r( &status, &input, &out, &buffer, &n, &m, X, X_l, X_u,
                     Y, C_l, C_u, equation, linear, &i1, &i1, &i1 );
    if ( status != 0 ) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_x( n, X, X_l, X_u );
    write_y( m, Y, C_l, C_u, equation, linear );

    X[0] = 1.1, X[1] = 2.2;

    // obtain numbers of nonlinear variables, and equality and 
    // linear constraints
    printf("CALL CUTEST_cstats\n");
    CUTEST_cstats_r( &status, &nonlinear_variables_objective,
                     &nonlinear_variables_constraints,
                     &equality_constraints, &linear_constraints );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    printf(" * nonlinear_variables_objective = %d\n",
           nonlinear_variables_objective);
    printf(" * nonlinear_variables_constraints = %d\n",
           nonlinear_variables_constraints);
    printf(" * equality_constraints = %d\n", equality_constraints);
    printf(" * linear_constraints = %d\n", linear_constraints);

    // obtain variable and problem names
    printf("CALL CUTEST_cnames\n");
    CUTEST_cnames_r( &status, &n, &m, p_name, X_names_fortran, 
                     C_names_fortran );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }

    // Transfer variables and constraint names into arrays 
    // of null-terminated strings, and print them
    for (i = 0; i < n; i++) {
      cptr = X_names_fortran + i * ( FSTRING_LEN + 1 );
      for (j = 0; j < FSTRING_LEN; j++) {
        X_names[i][j] = *cptr;
        cptr++;
      }
      X_names[i][FSTRING_LEN] = '\0';
    }
    for (i = 0; i < m; i++) {
      cptr = C_names_fortran + i * ( FSTRING_LEN + 1 );
      for (j = 0; j < FSTRING_LEN; j++) {
        C_names[i][j] = *cptr;
        cptr++;
      }
      C_names[i][FSTRING_LEN] = '\0';
    }

    p_name[FSTRING_LEN] = '\0';
    printf(" * p_name: %-s\n", p_name);
    printf(" * Variable names:\n");
    for (i = 0; i < n; i++) printf(" *  %s\n", X_names[i]);
    printf(" * Constraint names:\n");
    for (i = 0; i < m; i++) printf(" *  %s\n", C_names[i]);

    // obtain constraint names
    printf("Call CUTEST_connames\n");
    CUTEST_connames_r( &status, &m, C_names_fortran );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }

    // Transfer constraint names into an array  of null-terminated strings, 
    //and print them
    for (i = 0; i < m; i++) {
      cptr = C_names_fortran + i * ( FSTRING_LEN + 1 );
      for (j = 0; j < FSTRING_LEN; j++) {
        C_names[i][j] = *cptr;
        cptr++;
      }
      C_names[i][FSTRING_LEN] = '\0';
    }
    printf(" * Constraint names:\n");
    for (i = 0; i < m; i++) printf(" *  %s\n", C_names[i]);

    // obtain variable types
    printf("CALL CUTEST_cvartype\n");
    CUTEST_cvartype_r( &status, &n, X_type );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_x_type( n, X_type );

    // compute the objective and constraint function values
    printf("CALL CUTEST_cfn\n");
    CUTEST_cfn_r( &status, &n, &m, X, &f, C );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_f( f );
    write_c( m, C );

    // compute the objective function value
    printf("CALL CUTEST_cifn for the objective function\n");
    icon = 0;
    CUTEST_cifn_r( &status, &n, &icon, X, &f );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_f( f );

    // compute a constraint value
    printf("CALL CUTEST_cifn for a constraint\n");
    icon = 1;
    CUTEST_cifn_r( &status, &n, &icon, X, &ci );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_ci( icon, ci );

    // compute the constraint function values alone
    printf("CALL CUTEST_ccf\n");

    CUTEST_ccf_r( &status, &n, &m, X, C );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_c( m, C );

    // compute the gradient and dense Jacobian values
    grlagf = true, jtrans = true;
    printf("CALL CUTEST_cgr with grlagf = true and jtrans = true\n");
    CUTEST_cgr_r( &status, &n, &m, X, Y, &grlagf, G, &jtrans,
                  &l_j2_1, &l_j2_2, J2_val );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_g( n, G );
    write_jt_dense( n, m, l_j2_1, l_j2_2, J2_val );
    grlagf = true , jtrans = false;
    printf("CALL CUTEST_cgr with grlagf = true and jtrans = false\n");
    CUTEST_cgr_r( &status, &n, &m, X, Y, &grlagf, G, &jtrans,
                  &l_j2_1, &l_j2_2, J2_val );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_g( n, G );
    write_j_dense( n, m, l_j2_1, l_j2_2, J2_val );
    grlagf = false, jtrans = true;
    printf("CALL CUTEST_cgr with grlagf = false and jtrans = true\n");
    CUTEST_cgr_r( &status, &n, &m, X, Y, &grlagf, G, &jtrans,
                  &l_j2_1, &l_j2_2, J2_val );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_g( n, G );
    write_jt_dense( n, m, l_j2_1, l_j2_2, J2_val );
    grlagf = false, jtrans = false;
    printf("CALL CUTEST_cgr with grlagf = false and jtrans = false\n");
    CUTEST_cgr_r( &status, &n, &m, X, Y, &grlagf, G, &jtrans,
                  &l_j2_1, &l_j2_2, J2_val );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_g( n, G );
    write_j_dense( n, m, l_j2_1, l_j2_2, J2_val );

    // compute the objective function and gradient values
    grad = false;
    printf("CALL CUTEST_cofg with grad = false\n");
    CUTEST_cofg_r( &status, &n, X, &f, G, &grad );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_f( f );
    grad = true;
    printf("CALL CUTEST_cofg with grad = true\n");
    CUTEST_cofg_r( &status, &n, X, &f, G, &grad );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_f( f );
    write_g( n, G );

    // compute just its gradient
    icon = 0;
    printf("CALL CUTEST_cigr for the objective function\n");
printf("hello\n");
    CUTEST_cigr_r( &status, &n, &icon, X, G );
printf("status %d\n", status);
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
printf("hello\n");
    write_g( n, G );

    // compute the objective function and sparse gradient values
    l_g = n;
    G_var = malloc(l_g * sizeof(ipc_));
    G_val = malloc(l_g * sizeof(rpc_));
    if (G_var == NULL || G_val == NULL) {
        perror("Error allocating memory");
        return 1;
    }
    grad = false;
    printf("CALL CUTEST_cofsg with grad = false\n");
    CUTEST_cofsg_r( &status, &n, X, &f, &G_ne, &l_g, G_val, G_var, &grad );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_f( f );
    grad = true;
    printf("CALL CUTEST_cofsg with grad = true\n");
    CUTEST_cofsg_r( &status, &n, X, &f, &G_ne, &l_g, G_val, G_var, &grad );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_f( f );
    write_sg( G_ne, G_var, G_val );

    // compute the number of nonzeros in the sparse gradient of the objective
    printf("CALL CUTEST_cdimsg\n");
    CUTEST_cdimsg_r( &status, &G_ne );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    printf("* G_ne = %d\n", G_ne);

    // compute its sparsity pattern
    icon = 0;
    printf("CALL CUTEST_cisgrp for the objective function\n");
    CUTEST_cisgrp_r( &status, &n, &icon, &G_ne, &l_g, G_var );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_g_sparsity_pattern( G_ne, G_var );

    // and its values
    printf("CALL CUTEST_cisgr for the objective function\n");
    CUTEST_cisgr_r( &status, &n, &icon, X, &G_ne, &l_g, G_val, G_var );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_sg( G_ne, G_var, G_val );

    // compute the number of nonzeros in the sparse Jacobian
    printf("CALL CUTEST_cdimsj\n");
    CUTEST_cdimsj_r( &status, &J_ne );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    printf("* J_ne = %d\n", J_ne);

    l_j = J_ne;
    J_val = malloc(l_j * sizeof(rpc_));
    J_fun = malloc(l_j * sizeof(ipc_));
    J_var = malloc(l_j * sizeof(ipc_));
    if (J_val == NULL || J_fun == NULL || J_var == NULL) {
        perror("Error allocating memory");
        return 1;
    }

    // compute the sparsity pattern of the Jacobian
    printf("Call CUTEST_csjp\n");
    CUTEST_csjp_r( &status, &J_ne, &l_j, J_var, J_fun );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_j_sparsity_pattern( J_ne, J_fun, J_var );

    // compute the sparsity pattern of the Jacobian and objective gradient
    printf("Call CUTEST_csgrp\n");
    CUTEST_csgrp_r( &status, &n, &J_ne, &l_j, J_var, J_fun );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_j_sparsity_pattern( J_ne, J_fun, J_var );

    // compute the gradient and sparse Jacobian values
    grlagf = true;
    printf("CALL CUTEST_csgr with grlagf = true\n");
    CUTEST_csgr_r( &status, &n, &m, X, Y, &grlagf,
                   &J_ne, &l_j, J_val, J_var, J_fun );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
 printf("lj, J_ne = %d %d\n", l_j, J_ne);
    write_j_sparse( J_ne, J_val, J_fun, J_var );
    grlagf = false;
    printf("CALL CUTEST_csgr with grlagf = false\n");
    CUTEST_csgr_r( &status, &n, &m, X, Y, &grlagf,
                   &J_ne, &l_j, J_val, J_var, J_fun );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_j_sparse( J_ne, J_val, J_fun, J_var );

    // compute the constraint and dense Jacobian values
    grad = true, jtrans = true;
    printf("CALL CUTEST_ccfg with grad = true and jtrans = true\n");
    CUTEST_ccfg_r( &status, &n, &m, X, C, &jtrans,
                   &l_j2_1, &l_j2_2, J2_val, &grad );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_c( m, C );
    write_jt_dense( n, m, l_j2_1, l_j2_2, J2_val );
    grad = true , jtrans = false;
    printf("CALL CUTEST_ccfg with grad = true and jtrans = false\n");
    CUTEST_ccfg_r( &status, &n, &m, X, C, &jtrans,
                   &l_j2_1, &l_j2_2, J2_val, &grad );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_c( m, C );
    write_j_dense( n, m, l_j2_1, l_j2_2, J2_val );
    grad = false, jtrans = true;
    printf("CALL CUTEST_ccfg with grad = false and jtrans = true\n");
    CUTEST_ccfg_r( &status, &n, &m, X, C, &jtrans,
                   &l_j2_1, &l_j2_2, J2_val, &grad );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_c( m, C );
    grad = false, jtrans = false;
    printf("CALL CUTEST_ccfg with grad = false and jtrans = false\n");
    CUTEST_ccfg_r( &status, &n, &m, X, C, &jtrans,
                   &l_j2_1, &l_j2_2, J2_val, &grad );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_c( m, C );

    // compute the constraint and sparse Jacobian values
    grad = true;
    printf("CALL CUTEST_ccfsg with grad = true\n");
    CUTEST_ccfsg_r( &status, &n, &m, X, C,
                    &J_ne, &l_j, J_val, J_var, J_fun, &grad );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_c( m, C );
    write_j_sparse( J_ne, J_val, J_fun, J_var );
    grad = false;
    printf("CALL CUTEST_ccfsg with grad = false\n");
    CUTEST_ccfsg_r( &status, &n, &m, X, C,
                    &J_ne, &l_j, J_val, J_var, J_fun, &grad );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_c( m, C );

    // compute the Lagrangian function and gradient values
    grad = true;
    printf("CALL CUTEST_clfg with grad = true\n");
    CUTEST_clfg_r( &status, &n, &m, X, Y, &f, G, &grad );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_f( f );
    write_g( n, G );
    grad = false;
    printf("CALL CUTEST_clfg with grad = false\n");
    CUTEST_clfg_r( &status, &n, &m, X, Y, &f, G, &grad );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_f( f );

    // compute an individual constraint and its dense gradient
    icon = 1;
    grad = false;
    printf("CALL CUTEST_ccifg with grad = false\n");
    CUTEST_ccifg_r( &status, &n, &icon, X, &ci, Ji, &grad );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_ci( icon, ci );
    grad = true;
    printf("CALL CUTEST_ccifg with grad = true\n");
    CUTEST_ccifg_r( &status, &n, &icon, X, &ci, Ji, &grad );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_ci( icon, ci );
    write_ji( n, icon, Ji );

    // compute just its dense gradient
    printf("CALL CUTEST_cigr for a constraint\n");
    CUTEST_cigr_r( &status, &n, &icon, X, Ji );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_ji( n, icon, Ji );

    // compute an individual constraint and its sparse gradient
    grad = false;
    printf("CALL CUTEST_ccifsg with grad = false\n");
    CUTEST_ccifsg_r( &status, &n, &icon, X, &ci,
                     &Ji_ne, &n, Ji, J_var, &grad );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_ci( icon, ci );
    grad = true;
    printf("CALL CUTEST_ccifsg with grad = true\n");
    CUTEST_ccifsg_r( &status, &n, &icon, X, &ci,
                     &Ji_ne, &n, Ji, J_var, &grad );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_ci( icon, ci );
    write_sji( icon, Ji_ne, Ji, J_var );

    // compute the sparsity pattern of the gradient of an individual constraint
    printf("CALL CUTEST_cisgrp for a constraint\n");
    CUTEST_cisgrp_r( &status, &n, &icon, &G_ne, &l_g, G_var );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_g_sparsity_pattern( G_ne, G_var );

    // and its values
    printf("CALL CUTEST_cisgr for a constraint\n");
    CUTEST_cisgr_r( &status, &n, &icon, X, &Ji_ne, &n, Ji, J_var );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_sji( icon, Ji_ne, Ji, J_var );

    // compute the dense Hessian value
    printf("CALL CUTEST_cdh\n");
    CUTEST_cdh_r( &status, &n, &m, X, Y, &l_h2_1, H2_val );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_h_dense( n, l_h2_1, H2_val );

    // compute the dense Hessian value of the John function
    y0 = 2.0;
    printf("CALL CUTEST_cdhj\n");
    CUTEST_cdhj_r( &status, &n, &m, X, &y0, Y, &l_h2_1, H2_val );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_h_dense( n, l_h2_1, H2_val );

    // compute the dense Hessian value without the objective function
    printf("CALL CUTEST_cdhc\n");
    CUTEST_cdhc_r( &status, &n, &m, X, Y, &l_h2_1, H2_val );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_h_dense( n, l_h2_1, H2_val );

    // compute the dense Hessian value of the objective or a constraint
    iprob = 0;
    printf("CALL CUTEST_cidh for objective\n");
    CUTEST_cidh_r( &status, &n, X, &iprob, &l_h2_1, H2_val );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_h_dense( n, l_h2_1, H2_val );
    iprob = 1;
    printf("CALL CUTEST_cidh for a constraint\n");
    CUTEST_cidh_r( &status, &n, X, &iprob, &l_h2_1, H2_val );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_h_dense( n, l_h2_1, H2_val );

    // compute the gradient and dense Hessian values
    grlagf = true, jtrans = true;
    printf("CALL CUTEST_cgrdh with grlagf = true and jtrans = true\n");
    CUTEST_cgrdh_r( &status, &n, &m, X, Y, &grlagf, G, &jtrans,
                    &l_j2_1, &l_j2_2, J2_val, &l_h2_1, H2_val );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_g( n, G );
    write_h_dense( n, l_h2_1, H2_val );
    grlagf = true, jtrans = false;
    printf("CALL CUTEST_cgrdh with grlagf = true and jtrans = false\n");
    CUTEST_cgrdh_r( &status, &n, &m, X, Y, &grlagf, G, &jtrans,
                    &l_j2_1, &l_j2_2, J2_val, &l_h2_1, H2_val );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_g( n, G );
    write_h_dense( n, l_h2_1, H2_val );
    grlagf = false, jtrans = true;
    printf("CALL CUTEST_cgrdh with grlagf = false and jtrans = true\n");
    CUTEST_cgrdh_r( &status, &n, &m, X, Y, &grlagf, G, &jtrans,
                    &l_j2_1, &l_j2_2, J2_val, &l_h2_1, H2_val );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_g( n, G );
    write_h_dense( n, l_h2_1, H2_val );
    grlagf = false, jtrans = false;
    printf("CALL CUTEST_cgrdh with grlagf = false and jtrans = false\n");
    CUTEST_cgrdh_r( &status, &n, &m, X, Y, &grlagf, G, &jtrans,
                    &l_j2_1, &l_j2_2, J2_val, &l_h2_1, H2_val );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_g( n, G );
    write_h_dense( n, l_h2_1, H2_val );

    // compute the number of nonzeros in the sparse Hessian
    printf("CALL CUTEST_cdimsh\n");
    CUTEST_cdimsh_r( &status, &H_ne );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    printf("* H_ne = %d\n", H_ne);

    l_h = H_ne;
    H_val = malloc(l_h * sizeof(rpc_));
    H_row = malloc(l_h * sizeof(ipc_));
    H_col = malloc(l_h * sizeof(ipc_));
    if (H_val == NULL || H_row == NULL || H_col == NULL) {
        perror("Error allocating memory");
        return 1;
    }

    // compute the sparsity pattern of the Hessian
    printf("Call CUTEST_cshp\n");
    CUTEST_cshp_r( &status, &n, &H_ne, &l_h, H_row, H_col );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_h_sparsity_pattern( H_ne, H_row, H_col );

    // compute the sparse Hessian value
    printf("CALL CUTEST_csh\n");
    CUTEST_csh_r( &status, &n, &m, X, Y,
                  &H_ne, &l_h, H_val, H_row, H_col );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_h_sparse( H_ne, H_val, H_row, H_col );

    // compute the sparse Hessian value without the objective
    printf("CALL CUTEST_cshc\n");
    CUTEST_cshc_r( &status, &n, &m, X, Y,
                   &H_ne, &l_h, H_val, H_row, H_col );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_h_sparse( H_ne, H_val, H_row, H_col );

    // compute the sparse Hessian of the John function
    printf("CALL CUTEST_cshj\n");
    CUTEST_cshj_r( &status, &n, &m, X, &y0, Y,
                   &H_ne, &l_h, H_val, H_row, H_col );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_h_sparse( H_ne, H_val, H_row, H_col );

    // compute the sparse Hessian value of the objective or a constraint
    iprob = 0;
    printf("CALL CUTEST_cish for objective\n");
    CUTEST_cish_r( &status, &n, X, &iprob,
                   &H_ne, &l_h, H_val, H_row, H_col );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_h_sparse( H_ne, H_val, H_row, H_col );
    iprob = 1;
    printf("CALL CUTEST_cish for a constraint\n");
    CUTEST_cish_r( &status, &n, X, &iprob,
                   &H_ne, &l_h, H_val, H_row, H_col );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_h_sparse( H_ne, H_val, H_row, H_col );

    // compute the sparsity pattern of the gradients and Hessian
    printf("Call CUTEST_csgrshp\n");
    CUTEST_csgrshp_r( &status, &n, &J_ne, &l_j, J_var, J_fun,
                      &H_ne, &l_h, H_row, H_col );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_j_sparsity_pattern( J_ne, J_fun, J_var );
    write_h_sparsity_pattern( H_ne, H_row, H_col );

    // compute the gradient and sparse Hessian values
    grlagf = true;
    printf("CALL CUTEST_csgrsh with grlagf = true\n");
    CUTEST_csgrsh_r( &status, &n, &m, X, Y, &grlagf, &J_ne, &l_j, J_val,
                     J_var, J_fun, &H_ne, &l_h, H_val, H_row, H_col );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_j_sparse( J_ne, J_val, J_fun, J_var );
    write_h_sparse( H_ne, H_val, H_row, H_col );
    grlagf = false;
    printf("CALL CUTEST_csgrsh with grlagf = false\n");
    CUTEST_csgrsh_r( &status, &n, &m, X, Y, &grlagf, &J_ne, &l_j, J_val,
                     J_var, J_fun, &H_ne, &l_h, H_val, H_row, H_col );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_j_sparse( J_ne, J_val, J_fun, J_var );
    write_h_sparse( H_ne, H_val, H_row, H_col );

    // compute the number of nonzeros in the element Hessian
    printf("CALL CUTEST_cdimse\n");
    CUTEST_cdimse_r( &status, &HE_nel, &HE_val_ne, &HE_row_ne );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    printf("* H_nel = %d, HE_val_ne = %d, HE_row_ne = %d\n", 
           HE_nel, HE_val_ne, HE_row_ne );

    lhe_ptr = HE_nel + 1;
    lhe_val = HE_val_ne;
    lhe_row = HE_row_ne;
    HE_row_ptr = malloc(lhe_ptr * sizeof(ipc_));
    HE_val_ptr = malloc(lhe_ptr * sizeof(ipc_));
    HE_row = malloc(lhe_row * sizeof(ipc_));
    HE_val = malloc(lhe_val * sizeof(rpc_));
    if (HE_row_ptr == NULL || HE_val_ptr == NULL || 
        HE_row == NULL || HE_val == NULL) {
        perror("Error allocating memory");
        return 1;
    }

    // compute the element Hessian value
    byrows = false;
    printf("CALL CUTEST_ceh with byrows = false\n");
    CUTEST_ceh_r( &status, &n, &m, X, Y, &HE_nel, &lhe_ptr, HE_row_ptr,
                  HE_val_ptr, &lhe_row, HE_row, &lhe_val, HE_val, &byrows );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_h_element( HE_nel, HE_row_ptr, HE_val_ptr, HE_row, HE_val );
    byrows = true;
    printf("CALL CUTEST_ceh with byrows = true\n");
    CUTEST_ceh_r( &status, &n, &m, X, Y, &HE_nel, &lhe_ptr, HE_row_ptr,
                  HE_val_ptr, &lhe_row, HE_row, &lhe_val, HE_val, &byrows );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_h_element( HE_nel, HE_row_ptr, HE_val_ptr, HE_row, HE_val );

    // compute the gradient and element Hessian values
    grlagf = true, byrows = true;
    printf("CALL CUTEST_csgreh with grlagf = true and byrows = true\n");
    CUTEST_csgreh_r( &status, &n, &m, X, Y, &grlagf, &J_ne, &l_j,
                     J_val, J_var, J_fun, &HE_nel, &lhe_ptr, 
                     HE_row_ptr, HE_val_ptr, &lhe_row, HE_row, 
                     &lhe_val, HE_val, &byrows );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_j_sparse( J_ne, J_val, J_fun, J_var );
    write_h_element( HE_nel, HE_row_ptr, HE_val_ptr, HE_row, HE_val );
    grlagf = true, byrows = false;
    printf("ALL CUTEST_csgreh with grlagf = true and byrows = false\n");
    CUTEST_csgreh_r( &status, &n, &m, X, Y, &grlagf, &J_ne, &l_j,
                     J_val, J_var, J_fun, &HE_nel, &lhe_ptr, HE_row_ptr,
                     HE_val_ptr, &lhe_row, HE_row, &lhe_val, HE_val, 
                     &byrows );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_j_sparse( J_ne, J_val, J_fun, J_var );
    write_h_element( HE_nel, HE_row_ptr, HE_val_ptr, HE_row, HE_val );
    grlagf = false, byrows = true;
    printf("CALL CUTEST_csgreh with grlagf = false and byrows = true\n");
    CUTEST_csgreh_r( &status, &n, &m, X, Y, &grlagf, &J_ne, &l_j,
                     J_val, J_var, J_fun, &HE_nel, &lhe_ptr, HE_row_ptr,
                     HE_val_ptr, &lhe_row, HE_row, &lhe_val, HE_val, 
                     &byrows );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_j_sparse( J_ne, J_val, J_fun, J_var );
    write_h_element( HE_nel, HE_row_ptr, HE_val_ptr, HE_row, HE_val );
    grlagf = false ; byrows = false;
    printf("ALL CUTEST_csgreh with grlagf = false and byrows = false\n");
    CUTEST_csgreh_r( &status, &n, &m, X, Y, &grlagf, &J_ne, &l_j,
                     J_val, J_var, J_fun, &HE_nel, &lhe_ptr, HE_row_ptr,
                     HE_val_ptr, &lhe_row, HE_row, &lhe_val, HE_val, 
                     &byrows );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_j_sparse( J_ne, J_val, J_fun, J_var );
    write_h_element( HE_nel, HE_row_ptr, HE_val_ptr, HE_row, HE_val );
    // compute a Hessian-vector product
    vector[0] = 1.0;
    for (int i = 1; i < n; i++) vector[i] = 0.0;
    goth = false;
    printf("Call CUTEST_chprod with goth = false\n");
    CUTEST_chprod_r( &status, &n, &m, &goth, X, Y, vector, result );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_result( n, vector, result );
    goth = true;
    printf("Call CUTEST_chprod with goth = true\n");
    CUTEST_chprod_r( &status, &n, &m, &goth, X, Y, vector, result );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_result( n, vector, result );

    // compute a sparse Hessian-vector product
    nnz_vector = 1 ; INDEX_nz_vector[0] = 1;
    goth = false;
    printf("Call CUTEST_cshprod with goth = false\n");
    CUTEST_cshprod_r( &status, &n, &m, &goth, X, Y,
                      &nnz_vector, INDEX_nz_vector, vector,
                      &nnz_result, INDEX_nz_result, result );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_sresult( nnz_vector, INDEX_nz_vector, vector,
                   nnz_result, INDEX_nz_result, result );

    goth = true;
    printf("Call CUTEST_cshprod with goth = true\n");
    CUTEST_cshprod_r( &status, &n, &m, &goth, X, Y,
                      &nnz_vector, INDEX_nz_vector, vector,
                      &nnz_result, INDEX_nz_result, result );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_sresult( nnz_vector, INDEX_nz_vector, vector,
                   nnz_result, INDEX_nz_result, result );

    // compute a Hessian-of-the-John-function-vector product
    goth = false;
    printf("Call CUTEST_chjprod with goth = false\n");
    CUTEST_chjprod_r( &status, &n, &m, &goth, X, &y0, Y, vector, result );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_result( n, vector, result );
    goth = true;
    printf("Call CUTEST_chpjrod with goth = true\n");
    CUTEST_chjprod_r( &status, &n, &m, &goth, X, &y0, Y, vector, result );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_result( n, vector, result );

    // compute a Hessian-vector product ignoring the objective
    goth = false;
    printf("Call CUTEST_chcprod with goth = false\n");
    CUTEST_chcprod_r( &status, &n, &m, &goth, X, Y, vector, result );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_result( n, vector, result );
    goth = true;
    printf("Call CUTEST_chcprod with goth = true\n");
    CUTEST_chcprod_r( &status, &n, &m, &goth, X, Y, vector, result );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_result( n, vector, result );

    // compute a sparse Hessian-vector product ignoring the objective
    goth = false;
    printf("Call CUTEST_cshprod with goth = false\n");
    CUTEST_cshcprod_r( &status, &n, &m, &goth, X, Y,
                       &nnz_vector, INDEX_nz_vector, vector,
                       &nnz_result, INDEX_nz_result, result );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_sresult( nnz_vector, INDEX_nz_vector, vector,
                   nnz_result, INDEX_nz_result, result );

    goth = true;
    printf("Call CUTEST_cshprod with goth = true\n");
    CUTEST_cshcprod_r( &status, &n, &m, &goth, X, Y,
                       &nnz_vector, INDEX_nz_vector, vector,
                       &nnz_result, INDEX_nz_result, result );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    write_sresult( nnz_vector, INDEX_nz_vector, vector,
                   nnz_result, INDEX_nz_result, result );

    // compute a Jacobian-vector product
    vector[0] = 1.0;
    for (int i = 1; i < n; i++) vector[i] = 0.0;
    gotj = false, jtrans = false;
    printf("CALL CJPROD with gotj = false and jtrans = false\n");
    CUTEST_cjprod_r( &status, &n, &m, &gotj, &jtrans, X, vector, &n,
                     result, &m );
    write_result2( n, vector, m, result );
    gotj = true, jtrans = false;
    printf("CALL CJPROD with gotj = true and jtrans = false\n");
    CUTEST_cjprod_r( &status, &n, &m, &gotj, &jtrans, X, vector, &n,
                     result, &m );
    write_result2( n, vector, m, result );
    gotj = false, jtrans = true;
    printf("CALL CJPROD with gotj = false and jtrans = true\n");
    CUTEST_cjprod_r( &status, &n, &m, &gotj, &jtrans, X, vector, &m,
                     result, &n );
    write_result2( m, vector, n, result );
    gotj = true, jtrans = true;
    printf("CALL CJPROD with gotj = true and jtrans = true\n");
    CUTEST_cjprod_r( &status, &n, &m, &gotj, &jtrans, X, vector, &m,
                     result, &n );
    write_result2( m, vector, n, result );

    // compute a sparse Jacobian-vector product
    gotj = false, jtrans = false;
    printf("CALL CSJPROD with gotj = false and jtrans = false\n");
    CUTEST_csjprod_r( &status, &n, &m, &gotj, &jtrans, X,
                      &nnz_vector, INDEX_nz_vector, vector, &n,
                      &nnz_result, INDEX_nz_result, result, &m );
    write_sresult( nnz_vector, INDEX_nz_vector, vector,
                   nnz_result, INDEX_nz_result, result );
    gotj = true, jtrans = false;
    printf("CALL CSJPROD with gotj = true and jtrans = false\n");
    CUTEST_csjprod_r( &status, &n, &m, &gotj, &jtrans, X,
                      &nnz_vector, INDEX_nz_vector, vector, &n,
                      &nnz_result, INDEX_nz_result, result, &m );
    write_sresult( nnz_vector, INDEX_nz_vector, vector,
                   nnz_result, INDEX_nz_result, result );
    gotj = false, jtrans = true;
    printf("CALL CSJPROD with gotj = false and jtrans = true\n");
    CUTEST_csjprod_r( &status, &n, &m, &gotj, &jtrans, X,
                      &nnz_vector, INDEX_nz_vector, vector, &m,
                      &nnz_result, INDEX_nz_result, result, &n );
    write_sresult( nnz_vector, INDEX_nz_vector, vector,
                   nnz_result, INDEX_nz_result, result );
    gotj = true, jtrans = true;
    printf("CALL CSJPROD with gotj = true and jtrans = true\n");
    CUTEST_csjprod_r( &status, &n, &m, &gotj, &jtrans, X,
                      &nnz_vector, INDEX_nz_vector, vector, &m,
                      &nnz_result, INDEX_nz_result, result, &n );
    write_sresult( nnz_vector, INDEX_nz_vector, vector,
                   nnz_result, INDEX_nz_result, result );

    // compute the number of nonzeros when forming the products of the 
    // constraint Hessians with a vector
    printf("CALL CUTEST_cdimchp\n");
    CUTEST_cdimchp_r( &status, &CHP_ne );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    printf("* CHP_ne = %d\n", CHP_ne);

    l_chp = CHP_ne;
    CHP_val = malloc(l_chp * sizeof(rpc_));
    CHP_ind = malloc(l_chp * sizeof(ipc_));
    CHP_ptr = malloc((m+1) * sizeof(ipc_));
    if (CHP_val == NULL || CHP_ind == NULL || CHP_ptr == NULL) {
        perror("Error allocating memory");
        return 1;
    }

    // compute the sparsity pattern needed for the matrix-vector products
    // between each constraint Hessian and a vector
    printf("Call CUTEST_cchprodsp\n");
    CUTEST_cchprodsp_r( &status, &m, &l_chp, CHP_ind, CHP_ptr );
    write_chp_sparsity( m, CHP_ind, CHP_ptr );

    // compute the matrix-vector products between each constraint Hessian
    // and a vector
    goth = false;
    printf("Call CUTEST_cchprods with goth = false\n");
     CUTEST_cchprods_r( &status, &n, &m, &goth, X, vector, &l_chp,
                        CHP_val, CHP_ind, CHP_ptr );
     write_chp( m, CHP_val, CHP_ind, CHP_ptr );

    goth = true;
    printf("Call CUTEST_cchprods with goth = true\n");
     CUTEST_cchprods_r( &status, &n, &m, &goth, X, vector, &l_chp,
                        CHP_val, CHP_ind, CHP_ptr );
     write_chp( m, CHP_val, CHP_ind, CHP_ptr );

    // compute the number of nonzeros when forming the products of the
    // objective Hessians with a vector
    printf("CALL CUTEST_cdimohp\n");
    CUTEST_cdimohp_r( &status, &OHP_ne );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }
    printf("* OHP_ne = %d\n", OHP_ne);

    l_ohp = OHP_ne;
    OHP_val = malloc(l_ohp * sizeof(rpc_));
    OHP_ind = malloc(l_ohp * sizeof(ipc_));
    if (OHP_val == NULL || OHP_ind == NULL) {
        perror("Error allocating memory");
        return 1;
    }

    // compute the sparsity pattern needed for the matrix-vector product
    // between the objective Hessian and a vector
    printf("Call CUTEST_cohprodsp\n");
    CUTEST_cohprodsp_r( &status, &OHP_ne, &l_ohp, OHP_ind );
    write_ohp_sparsity( OHP_ne, OHP_ind );
    // compute the matrix-vector product between the objective Hessian and a
    // vector
    goth = false;
    printf("Call CUTEST_cohprods with goth = false\n");
    CUTEST_cohprods_r( &status, &n, &goth, X, vector,
                       &OHP_ne, &l_ohp, OHP_val, OHP_ind );
    write_ohp( OHP_ne, OHP_val, OHP_ind );

    goth = true;
    printf("Call CUTEST_cohprods with goth = true\n");
    CUTEST_cohprods_r( &status, &n, &goth, X, vector,
                       &OHP_ne, &l_ohp, OHP_val, OHP_ind );
    write_ohp( OHP_ne, OHP_val, OHP_ind );

    // calls and time report
    printf("CALL CUTEST_creport\n");
    CUTEST_creport_r( &status, CALLS, CPU );
    printf("CALLS(1-7) = %.2f %.2f %.2f %.2f\n", 
           CALLS[0], CALLS[1], CALLS[2], CALLS[3]);
    printf("             %.2f %.2f %.2f\n", CALLS[4], CALLS[5], CALLS[6]);
    printf("CPU(1-4) = %.2f %.2f %.2f %.2f\n", CPU[0], CPU[1], CPU[2], CPU[3]);

    // terminal exit
    printf("Call CUTEST_cterminate\n");
    CUTEST_cterminate_r( &status );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }

    // one more setup ...
    printf("CALL CUTEST_csetup\n");
    CUTEST_csetup_r( &status, &input, &out, &buffer, &n, &m, X, X_l, X_u,
                     Y, C_l, C_u, equation, linear, &i1, &i1, &i1 );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }

    // ... and terminal exit
    printf("Call CUTEST_cterminate\n");
    CUTEST_cterminate_r( &status );
    if (status != 0) {
       printf("error status = %d\n", status);
       return 2;
    }

//here
    free(classification);
    free(p_name);
    free(X_type);
    free(H_row);
    free(H_col);
    free(HE_row);
    free(HE_row_ptr);
    free(HE_val_ptr);
    free(X);
    free(X_l);
    free(X_u);
    free(G);
    free(Ji);
    free(Y);
    free(C_l);
    free(C_u);
    free(C);
    free(H_val);
    free(HE_val);
    free(H2_val);
    free(J2_val);
    free(vector);
    free(result);
    free(G_val);
    free(G_var);
    for (i = 0; i < n; i++)
        free(X_names[i]);
    free(X_names);
    free(X_names_fortran);
    for (i = 0; i < m; i++)
        free(C_names[i]);
    free(C_names);
    free(C_names_fortran);
    free(equation);
    free(linear);
    free(INDEX_nz_vector);
    free(INDEX_nz_result);
    free(CHP_val);
    free(CHP_ind);
    free(CHP_ptr);
    free(OHP_val);
    free(OHP_ind);
    free(J_val);
    free(J_fun);
    free(J_var);

    FORTRAN_close_r(&input, &status);

    return 0;
}

//  data printing functions

void write_x(ipc_ n, rpc_ *X, rpc_ *X_l, rpc_ *X_u) {
    printf(" *       i      X_l           X           X_u\n");
    for (ipc_ i = 0; i < n; i++) {
        printf(" * %7d %12.4e %12.4e %12.4e\n", i + 1, X_l[i], X[i], X_u[i]);
    }
}

void write_y(ipc_ m, rpc_ *Y, rpc_ *C_l, rpc_ *C_u, 
             logical *equation, logical *linear) {
    printf(" *       i      C_l           C_u          Y");
    printf("     equation linear\n");
    for (ipc_ i = 0; i < m; i++) {
        printf(" * %7d %12.4e %12.4e %12.4e %s %s\n", i + 1, C_l[i], C_u[i], 
          Y[i], equation[i]?" true  ":" false ", linear[i]?" true  ":" false ");
    }
}

void write_x_type(ipc_ n, ipc_ *X_type) {
    printf(" *       i  X_type\n");
    for (ipc_ i = 0; i < n; i++) {
        printf(" * %7d %5d\n", i + 1, X_type[i]);
    }
}

void write_f(rpc_ f) {
    printf(" * f = %12.4e\n", f);
}

void write_c(ipc_ m, rpc_ *C) {
    printf(" *       i       C\n");
    for (ipc_ i = 0; i < m; i++) {
        printf(" * %7d %12.4e\n", i + 1, C[i]);
    }
}

void write_g(ipc_ n, rpc_ *G) {
    printf(" *       i       G\n");
    for (ipc_ i = 0; i < n; i++) {
        printf(" * %7d %12.4e\n", i + 1, G[i]);
    }
}

void write_g_sparsity_pattern(ipc_ g_ne, ipc_ *g_var) {
    printf(" * G(sparse)\n         i\n");
    for (ipc_ i = 0; i < g_ne; i++) {
        printf(" * %7d\n", g_var[i] );
    }
}

void write_sg(ipc_ g_ne, ipc_ *g_var, rpc_ *g_val) {
    printf(" *       i    G\n");
    for (ipc_ i = 0; i < g_ne; i++) {
        printf(" * %7d %12.4e\n", g_var[i], g_val[i]);
    }
}

void write_ci(ipc_ icon, rpc_ ci) {
    printf(" * c(%d) = %12.4e\n", icon, ci);
}

void write_ji(ipc_ n, ipc_ icon, rpc_ *ji) {
    printf(" *       i    J(%d)\n", icon);
    for (ipc_ i = 0; i < n; i++) {
        printf(" * %7d %12.4e\n", i+1, ji[i]);
    }
}

void write_sji(ipc_ icon, ipc_ ji_ne, rpc_ *ji, ipc_ *j_var) {
    printf(" *       i    J(%d)\n", icon);
    for (ipc_ i = 0; i < ji_ne; i++) {
        printf(" * %7d %12.4e\n", j_var[i], ji[i]);
    }
}

void write_j_dense(ipc_ n, ipc_ m, ipc_ l_j2_1, ipc_ l_j2_2, rpc_ *J2_val) {
    printf(" * J(dense)\n");
    for (ipc_ j = 0; j < n; j += 4) {
        // Print column headers based on how many columns are left
        printf(" *       i   j");
        for (ipc_ k = 0; k < 4 && j + k < n; k++) {
            printf("%12d ", j + k);
        }
        printf("\n");

        // Print matrix values
        for (ipc_ i = 0; i < m; i++) {
            printf(" * %7d      ", i + 1);
            for (ipc_ k = 0; k < 4 && j + k < n; k++) {
                printf(" %12.4e", J2_val[i+l_j2_1*(j+k)]);
            }
            printf("\n");
        }
    }
}

void write_jt_dense(ipc_ n, ipc_ m, ipc_ l_j2_1, ipc_ l_j2_2, rpc_ *J2_val) {
    printf(" * JT(dense)\n");
    for (ipc_ j = 0; j < m; j += 4) {
        // Print column headers based on how many columns are left
        printf(" *       i   j");
        for (ipc_ k = 0; k < 4 && j + k < m; k++) {
            printf("%12d ", j + k);
        }
        printf("\n");

        // Print matrix values
        for (ipc_ i = 0; i < n; i++) {
            printf(" * %7d      ", i + 1);
            for (ipc_ k = 0; k < 4 && j + k < m; k++) {
                printf(" %12.4e", J2_val[i+l_j2_1*(j+k)]);
            }
            printf("\n");
        }
    }
}

void write_h_dense(ipc_ n, ipc_ l_h2_1, rpc_ *H2_val) {
    printf(" * H(dense)\n");
    for (ipc_ j = 0; j < n; j += 4) {
        // Print column headers based on how many columns are left
        printf(" *       i   j");
        for (ipc_ k = 0; k < 4 && j + k < n; k++) {
            printf("%12d ", j + k);
        }
        printf("\n");

        // Print matrix values
        for (ipc_ i = 0; i < l_h2_1; i++) {
            printf(" * %7d      ", i + 1);
            for (ipc_ k = 0; k < 4 && j + k < n; k++) {
                printf(" %12.4e", H2_val[i+n*(j+k)]);
            }
            printf("\n");
        }
    }
}

void write_j_sparsity_pattern(ipc_ J_ne, ipc_ *J_row, ipc_ *J_col) {
    printf(" * J(sparse)\n *    row    col\n");
    for (ipc_ i = 0; i < J_ne; i++) {
        printf(" * %7d %7d\n", J_row[i], J_col[i]);
    }
}

void write_j_sparse(ipc_ J_ne, rpc_ *J_val, ipc_ *J_row, ipc_ *J_col) {
    printf(" * J_ne = %d\n", J_ne );
    printf(" * J(sparse)\n *    row    col     val\n");
    for (ipc_ i = 0; i < J_ne; i++) {
        printf(" * %7d %7d %12.4e\n", J_row[i], J_col[i], J_val[i]);
    }
}

void write_h_sparsity_pattern(ipc_ H_ne, ipc_ *H_row, ipc_ *H_col) {
    printf(" * H(sparse)\n *    row    col\n");
    for (ipc_ i = 0; i < H_ne; i++) {
        printf(" * %7d %7d\n", H_row[i], H_col[i]);
    }
}

void write_h_sparse(ipc_ H_ne, rpc_ *H_val, ipc_ *H_row, ipc_ *H_col) {
    printf(" * H(sparse)\n *    row    col     val\n");
    for (ipc_ i = 0; i < H_ne; i++) {
        printf(" * %7d %7d %12.4e\n", H_row[i], H_col[i], H_val[i]);
    }
}

void write_ohp_sparsity(ipc_ OHP_ne, ipc_ *OHP_ind) {
    printf(" * OH(product sparsity)\n * indices\n *");
    ipc_ k = 0;
    for (ipc_ i = 0; i < OHP_ne; i++) {
      printf(" %d", OHP_ind[i]);
      k++;
      if (k==10) {
        printf("\n *        ");
        k = 0;
      }
    }
    printf("\n");
}

void write_ohp(ipc_ OHP_ne, rpc_ *OHP_val, ipc_ *OHP_ind) {
    printf(" * OH(product)\n * (ind val)");
    ipc_ k = 0;
    for (ipc_ i = 0; i < OHP_ne; i++) {
      printf(" (%6d %12.4e)", OHP_ind[i], OHP_val[i]);
      k++;
      if (k==3) {
        printf("\n *          ");
        k = 0;
      }
    }
    printf("\n");
}

void write_chp_sparsity(ipc_ m, ipc_ *CHP_ind, ipc_ *CHP_ptr) {
    ipc_ k;
    printf(" * CH(product sparsity)\n");
    for (ipc_ i = 0; i < m; i++) {
      if (CHP_ptr[i + 1] > CHP_ptr[i]) {
        k = 0;
        printf(" * constraint Hessian %d\n * product indices", i + 1);
        for (ipc_ j = CHP_ptr[i]-1; j < CHP_ptr[i + 1]-1; j++) {
          printf(" %6d", CHP_ind[j]);
          k++;
          if (k==10 && j < CHP_ptr[i + 1]-2) {
            printf("\n *        ");
            k = 0;
          }
        }
        printf("\n");
      } else {
        printf(" * constraint Hessian %d has no indices\n", i + 1);
      }
    }
}

void write_chp(ipc_ m, rpc_ *CHP_val, ipc_ *CHP_ind, ipc_ *CHP_ptr) {
    ipc_ k;
    printf(" * CH(product)\n");
    for (ipc_ i = 0; i < m; i++) {
      if (CHP_ptr[i + 1] > CHP_ptr[i]) {
        k = 0;
        printf(" * constraint Hessian %d\n * (ind val)", i + 1);
        for (ipc_ j = CHP_ptr[i]-1; j < CHP_ptr[i + 1]-1; j++) {
          printf(" (%6d %12.4e)", CHP_ind[j], CHP_val[j]);
          k++;
          if (k==3 && j < CHP_ptr[i + 1]-2) {
            printf("\n *        ");
            k = 0;
          }
        }
        printf("\n");
      } else {
        printf(" * constraint Hessian %d has no indices\n", i + 1);
      }
    }
}

void write_h_element(ipc_ ne, ipc_ *HE_row_ptr, ipc_ *HE_val_ptr, 
                     ipc_ *HE_row, rpc_ *HE_val) {
    ipc_ k;
    printf(" * H(element)\n");
    for (ipc_ i = 0; i < ne; i++) {
      if (HE_row_ptr[i + 1] > HE_row_ptr[i]) {
        k = 0;
        printf(" * element %d\n * indices", i + 1);
        for (ipc_ j = HE_row_ptr[i]-1; j < HE_row_ptr[i + 1]-1; j++) {
          printf(" %12d", HE_row[j]);
          k++;
          if (k==5 && j < HE_row_ptr[i + 1]-2) {
            printf("\n *        ");
            k = 0;
          }
        }
        printf("\n * values ");
        k = 0;
        for (ipc_ j = HE_val_ptr[i]-1; j < HE_val_ptr[i + 1]-1; j++) {
          printf(" %12.4e", HE_val[j]);
          k++;
          if (k==5  && j < HE_val_ptr[i + 1]-2){
            printf("\n *        ");
            k = 0;
          }
        }
        printf("\n");
      } else {
        printf(" * element %d has no indices\n", i + 1);
      }
    }
}

void write_result(ipc_ n, rpc_ *vector, rpc_ *result) {
     printf(" *       i    vector       result\n");
    for (ipc_ i = 0; i < n; i++) {
      printf(" * %7d %12.4e %12.4e\n", i + 1, vector[i], result[i]);
    }
}

void write_result2(ipc_ n_vector, rpc_ *vector, ipc_ n_result, rpc_ *result) {
    ipc_ minnm = MIN( n_vector, n_result );
    printf(" *       i    vector       result\n");
    for (ipc_ i = 0; i < minnm; i++) {
        printf(" * %7d %12.4e %12.4e\n", i + 1, vector[i], result[i]);
    }
    if (n_vector > n_result) {
      for (ipc_ i = minnm; i < n_vector; i++)
          printf(" * %7d %12.4e       -\n", i + 1, vector[i]);
    } else {
      for (ipc_ i = minnm; i < n_result; i++)
          printf(" * %7d       -      %12.4e\n", i + 1, result[i]);
    }
}

void write_sresult(ipc_ nnz_vector, ipc_ *INDEX_nz_vector, rpc_ *vector, 
                   ipc_ nnz_result, ipc_ *INDEX_nz_result, rpc_ *result) {
    printf(" *       i    vector\n");
    for (ipc_ j = 0; j < nnz_vector; j++) {
        ipc_ i = INDEX_nz_vector[j];
        printf(" * %7d %12.4e\n", i + 1, vector[i]);
    }
    printf(" *       i    result\n");
    for (ipc_ j = 0; j < nnz_result; j++) {
        ipc_ i = INDEX_nz_result[j];
        printf(" * %7d %12.4e\n", i + 1, result[i]);
    }
}

