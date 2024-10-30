// THIS VERSION: CUTEST 2.3 - 2024-10-24 AT 07:40 GMT.

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "cutest_routines.h"
#include "cutest.h"

// Function prototypes
void write_x(ipc_ n, rpc_ *X, rpc_ *X_l, rpc_ *X_u);
void write_x_type(ipc_ n, ipc_ *X_type);
void write_f(rpc_ f);
void write_g(ipc_ n, rpc_ *G);
void write_h_dense(ipc_ n, ipc_ l_h2_1, rpc_ *H2_val);
void write_h_sparsity_pattern(ipc_ H_ne, ipc_ *H_row, ipc_ *H_col);
void write_h_sparse(ipc_ H_ne, rpc_ *H_val, ipc_ *H_row, ipc_ *H_col);
void write_h_element(ipc_ ne, ipc_ *HE_row_ptr, ipc_ *HE_val_ptr, 
                     ipc_ *HE_row, rpc_ *HE_val);
void write_result(ipc_ n, rpc_ *vector, rpc_ *result);
void write_sresult(ipc_ nnz_vector, ipc_ *INDEX_nz_vector, rpc_ *vector, 
                   ipc_ nnz_result, ipc_ *INDEX_nz_result, rpc_ *result);
void write_h_band(ipc_ n, ipc_ lbandh, rpc_ *H_band);

int main() {
    // CUTEst data file
    char *fname = "u_OUTSDIF.d";

    // Parameters
    ipc_ input = 55;
    ipc_ out = 6;
    ipc_ buffer = 77;
    rpc_ zero = 0.0;
    rpc_ one = 1.0;

    // Local Variables
    ipc_ i, j, n, HE_nel, HE_val_ne, HE_row_ne, status;
    ipc_ l_h2_1, l_h, lhe_ptr, H_ne, lhe_val, lhe_row;
    ipc_ nnz_vector, nnz_result, maxsbw, alloc_stat;
    ipc_ nsemib, lbandh;
    rpc_ f;
    logical byrows;
    logical grad;
    bool goth;
    ipc_ *X_type;
    ipc_ *H_row, *H_col, *HE_row, *HE_row_ptr, *HE_val_ptr;
    ipc_ *INDEX_nz_vector, *INDEX_nz_result;
    rpc_ *X, *X_l, *X_u, *G, *H_val, *HE_val, *H2_val, *H_band;
    rpc_ *vector, *result;
    char *classification, *p_name, *cptr;
    char *X_names_fortran;
    char **X_names;
    rpc_ CPU[4], CALLS[4];

    // Open the problem data file
    FORTRAN_open_r(&input, fname, &status);

    // Determine problem dimension
    printf("Call CUTEST_udimen\n");
    CUTEST_udimen_r(&status, &input, &n);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    printf("* n = %d\n", n);

    // Allocate basic arrays
    MALLOC(p_name, FSTRING_LEN + 1, char);
    MALLOC(X_names_fortran, n * ( FSTRING_LEN + 1 ), char); /* For Fortran */
    MALLOC(X_names, n, char *);
    for (i = 0; i < n; i++)
        MALLOC(X_names[i], FSTRING_LEN + 1, char);
    if (p_name == NULL || X_names_fortran == NULL || X_names == NULL) {
        perror("Error allocating memory for chars");
        return 1;
    }

    X = malloc(n * sizeof(rpc_));
    X_l = malloc(n * sizeof(rpc_));
    X_u = malloc(n * sizeof(rpc_));
    G = malloc(n * sizeof(rpc_));
    vector = malloc(n * sizeof(rpc_));
    result = malloc(n * sizeof(rpc_));
    X_type = malloc(n * sizeof(ipc_));
    INDEX_nz_vector = malloc(n * sizeof(ipc_));
    INDEX_nz_result = malloc(n * sizeof(ipc_));
    l_h2_1 = n;
    H2_val = malloc( l_h2_1 * n * sizeof(rpc_));
    if (X == NULL || X_l == NULL || X_u == NULL || G == NULL ||
        vector == NULL || result == NULL || X_names == NULL ||
        X_type == NULL || INDEX_nz_vector == NULL || 
        INDEX_nz_result == NULL || H2_val == NULL) {
        perror("Error allocating memory for ints and reals");
        return 1;
    }
    
    // Set up SIF data
    printf("Call CUTEST_usetup\n");
    CUTEST_usetup_r(&status, &input, &out, &buffer, &n, X, X_l, X_u);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_x(n, X, X_l, X_u);

    // Obtain variable and problem names
    printf("Call CUTEST_unames\n");

    CUTEST_unames_r(&status, &n, p_name, X_names_fortran);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }

    // Transfer variable names into arrays of null-terminated strings, 
    // and print them
    for (i = 0; i < n; i++) {
      cptr = X_names_fortran + i * ( FSTRING_LEN + 1 );
      for (j = 0; j < FSTRING_LEN; j++) {
        X_names[i][j] = *cptr;
        cptr++;
      }
      X_names[i][FSTRING_LEN] = '\0';
    }

    p_name[FSTRING_LEN] = '\0';
    printf(" * p_name: %-s\n", p_name);
    printf(" * Variable names:\n");
    for (i = 0; i < n; i++) printf(" *  %s\n", X_names[i]);

    // Obtain classification
    printf("Call CUTEST_classification\n");
    MALLOC(classification, FCSTRING_LEN + 1, char);
    CUTEST_classification_r(&status, &input, classification);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    printf(" * classification: %-s\n", classification);

    // Obtain problem name
    printf("Call CUTEST_probname\n");
    CUTEST_probname_r(&status, p_name);
    p_name[FSTRING_LEN] = '\0';
    printf(" * p_name: %-s\n", p_name);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    
    // Obtain variable names
    printf("Call CUTEST_varnames\n");
    CUTEST_varnames_r(&status, &n, X_names_fortran);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }

    // Transfer variable names into arrays of null-terminated strings, 
    // and print them
    for (i = 0; i < n; i++) {
      cptr = X_names_fortran + i * ( FSTRING_LEN + 1 );
      for (j = 0; j < FSTRING_LEN; j++) {
        X_names[i][j] = *cptr;
        cptr++;
      }
      X_names[i][FSTRING_LEN] = '\0';
    }
    printf(" * Variable names:\n");
    for (i = 0; i < n; i++) printf(" *  %s\n", X_names[i]);

    // Obtain variable types
    printf("Call CUTEST_uvartype\n");
    CUTEST_uvartype_r(&status, &n, X_type);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_x_type(n, X_type);
    
    // Compute the objective function value
    printf("Call CUTEST_ufn\n");
    CUTEST_ufn_r(&status, &n, X, &f);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_f(f);
    
    // Compute the gradient value
    printf("Call CUTEST_ugr\n");
    CUTEST_ugr_r(&status, &n, X, G);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_g(n, G);
    
    // Compute the objective function and gradient values
    grad = 1;
    printf("Call CUTEST_uofg with grad = TRUE\n");
    CUTEST_uofg_r(&status, &n, X, &f, G, &grad);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_f(f);
    write_g(n, G);
    
    grad = 0;
    printf("Call CUTEST_uofg with grad = FALSE\n");
    CUTEST_uofg_r(&status, &n, X, &f, G, &grad);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_f(f);
    
    // Compute the dense Hessian value
    printf("Call CUTEST_udh\n");
    CUTEST_udh_r(&status, &n, X, &l_h2_1, H2_val);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_h_dense(n, l_h2_1, H2_val);

    // Compute the gradient and dense Hessian values
    printf("Call CUTEST_ugrdh\n");
    CUTEST_ugrdh_r(&status, &n, X, G, &l_h2_1, H2_val);
    if (status != 0) {
        return 2;
        printf("error status = %d\n", status);
        return 2;
    }
    write_g(n, G);
    write_h_dense(n, l_h2_1, H2_val);
    
    // Compute the number of nonzeros in the sparse Hessian
    printf("Call CUTEST_udimsh\n");
    CUTEST_udimsh_r(&status, &H_ne);
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
    
    // Compute the sparsity pattern of the Hessian
    printf("Call CUTEST_ushp\n");
    CUTEST_ushp_r(&status, &n, &H_ne, &l_h, H_row, H_col);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_h_sparsity_pattern(H_ne, H_row, H_col);
    
    // Compute the sparse Hessian value
    printf("Call CUTEST_ush\n");
    CUTEST_ush_r(&status, &n, X, &H_ne, &l_h, H_val, H_row, H_col);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_h_sparse(H_ne, H_val, H_row, H_col);
    
    // Compute the gradient and sparse Hessian values
    printf("Call CUTEST_ugrsh\n");
    CUTEST_ugrsh_r(&status, &n, X, G, &H_ne, &l_h, H_val, H_row, H_col);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_g(n, G);
    write_h_sparse(H_ne, H_val, H_row, H_col);
    
    // Compute the number of nonzeros in the element Hessian
    printf("Call CUTEST_udimse\n");
    CUTEST_udimse_r(&status, &HE_nel, &HE_val_ne, &HE_row_ne);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    printf("* H_nel = %d, HE_val_ne = %d, HE_row_ne = %d\n", HE_nel, 
      HE_val_ne, HE_row_ne);
    
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

    byrows = false;
    printf("Call CUTEST_ueh with byrows = .FALSE.\n");
    CUTEST_ueh_r(&status, &n, X, &HE_nel, &lhe_ptr, HE_row_ptr, HE_val_ptr, 
                 &lhe_row, HE_row, &lhe_val, HE_val, &byrows);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_h_element(HE_nel, HE_row_ptr, HE_val_ptr, HE_row, HE_val);

    byrows = true;
    printf("Call CUTEST_ueh with byrows = .TRUE.\n");
    CUTEST_ueh_r(&status, &n, X, &HE_nel, &lhe_ptr, HE_row_ptr, HE_val_ptr, 
                 &lhe_row, HE_row, &lhe_val, HE_val, &byrows);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_h_element(HE_nel, HE_row_ptr, HE_val_ptr, HE_row, HE_val);

    // Compute gradient and element Hessian values
    byrows = false;
    printf("Call CUTEST_ugreh with byrows = .FALSE.\n");
    CUTEST_ugreh_r(&status, &n, X, G, &HE_nel, &lhe_ptr, HE_row_ptr, 
                   HE_val_ptr, &lhe_row, HE_row, &lhe_val, HE_val, &byrows);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_g(n, G);
    write_h_element(HE_nel, HE_row_ptr, HE_val_ptr, HE_row, HE_val);

    byrows = true;
    printf("Call CUTEST_ugreh with byrows = .TRUE.\n");
    CUTEST_ugreh_r(&status, &n, X, G, &HE_nel, &lhe_ptr, HE_row_ptr, 
                   HE_val_ptr, &lhe_row, HE_row, &lhe_val, HE_val, &byrows);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_g(n, G);
    write_h_element(HE_nel, HE_row_ptr, HE_val_ptr, HE_row, HE_val);

    // Compute Hessian-vector product
    vector[0] = 1.0;
    for (int i = 1; i < n; i++) vector[i] = 0.0;
    goth = false;

    printf("Call CUTEST_uhprod with goth = .FALSE.\n");
    CUTEST_uhprod_r(&status, &n, &goth, X, vector, result);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_result(n, vector, result);

    goth = true;
    printf("Call CUTEST_uhprod with goth = .TRUE.\n");
    CUTEST_uhprod_r(&status, &n, &goth, X, vector, result);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_result(n, vector, result);

    // Compute sparse Hessian-vector product
    nnz_vector = 1;
    INDEX_nz_vector[0] = 1;
    goth = false;

    printf("Call CUTEST_ushprod with goth = .FALSE.\n");
    CUTEST_ushprod_r(&status, &n, &goth, X, &nnz_vector, INDEX_nz_vector, 
                     vector, &nnz_result, INDEX_nz_result, result);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_sresult(nnz_vector, INDEX_nz_vector, vector, 
                  nnz_vector, INDEX_nz_vector, result);

    goth = true;
    printf("Call CUTEST_ushprod with goth = .TRUE.\n");
    CUTEST_ushprod_r(&status, &n, &goth, X, &nnz_vector, INDEX_nz_vector, 
                     vector, &nnz_result, INDEX_nz_result, result);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_sresult(nnz_vector, INDEX_nz_vector, vector, 
                  nnz_vector, INDEX_nz_vector, result);

    nsemib = n/2;
    lbandh = nsemib;
    H_band = malloc( ( lbandh + 1) * n * sizeof(rpc_));
    printf("Call CUTEST_ubandh\n");
    CUTEST_ubandh_r(&status, &n, X, &nsemib, H_band, &lbandh, &maxsbw);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }
    write_h_band(n, lbandh, H_band);

    // Calls and time report
    printf("CALL CUTEST_ureport\n");
    CUTEST_ureport_r(&status, CALLS, CPU);
    printf("CALLS(1-4) = %.2f %.2f %.2f %.2f\n", CALLS[0], CALLS[1], 
                                                 CALLS[2], CALLS[3]);
    printf("CPU(1-4) = %.2f %.2f %.2f %.2f\n", CPU[0], CPU[1], CPU[2], CPU[3]);

    // Terminal exit
    printf("Call CUTEST_uterminate\n");
    CUTEST_uterminate_r(&status);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }

    // One more setup
    printf("Call CUTEST_usetup\n");
    CUTEST_usetup_r(&status, &input, &out, &buffer, &n, X, X_l, X_u);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }

    // Terminal exit again
    printf("Call CUTEST_uterminate\n");
    CUTEST_uterminate_r(&status);
    if (status != 0) {
        printf("error status = %d\n", status);
        return 2;
    }

    // Cleanup and close files
    free(classification);
    free(p_name);
    for (i = 0; i < n; i++)
        free(X_names[i]);
    free(X_names);
    free(X_names_fortran);
    free(X);
    free(X_l);
    free(X_u);
    free(G);
    free(vector);
    free(result);
    free(X_type);
    free(INDEX_nz_vector);
    free(INDEX_nz_result);
    free(H2_val);
    free(H_val);
    free(H_row);
    free(H_col);
    free(HE_row_ptr);
    free(HE_val_ptr);
    free(HE_row);
    free(HE_val);
    free(H_band);
    
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

void write_x_type(ipc_ n, ipc_ *X_type) {
    printf(" *       i  X_type\n");
    for (ipc_ i = 0; i < n; i++) {
        printf(" * %7d %7d\n", i + 1, X_type[i]);
    }
}

void write_f(rpc_ f) {
    printf(" * f = %12.4e\n", f);
}

void write_g(ipc_ n, rpc_ *G) {
    printf(" *       i       G\n");
    for (ipc_ i = 0; i < n; i++) {
        printf(" * %7d %12.4e\n", i + 1, G[i]);
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
                if (k==5 && j < HE_row_ptr[i + 1]-2){
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
    printf(" *       i    vector     result\n");
    for (ipc_ i = 0; i < n; i++) {
        printf(" * %7d %12.4e %12.4e\n", i + 1, vector[i], result[i]);
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

void write_h_band(ipc_ n, ipc_ nsemib, rpc_ *H_band) {
    printf(" * H(band)\n");
    for (ipc_ i = 0; i < nsemib + 1; i += 4) {
        // Print column headers based on how many columns are left
        printf(" *       i   band");
        for (ipc_ k = 0; k < 4 && i + k < nsemib + 1; k++) {
            printf("%8d     ", i + k);
        }
        printf("\n");

        // Print matrix values
        for (ipc_ j = 0; j < n; j++) {
            printf(" * %7d      ", j + 1);
            for (ipc_ k = 0; k < 4 && i + k < nsemib + 1; k++) {
                printf(" %12.4e", H_band[i+k+(nsemib+1)*j]);
            }
            printf("\n");
        }
    }
}
