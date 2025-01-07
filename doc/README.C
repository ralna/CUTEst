           -------------------------------
           C interfaces to CUTEst packages
           -------------------------------

Each fortran CUTEst function has a pair of C equivalents.

** The first may be invoked by

#include "cutest_c.h"

and is fortran-orientated (to enable onward interfaces to Matlab, Python
and Julia), so C users need to be careful. The main issues are

  1. row and column indices when supplied are all fortran 1_based.
     If the row and column indices of a matrix are returned from
     a CUTEst call in the nnzh components of the arrays H_row and H_col,
     C users should follow this by

     for (i = 0; i < nnzh; i++) {
       H_row[i]--;
       H_col[i]--;
     }

  2. Some arrays are chopped into m segments so that the indices
     for segment l are stored 1-based in H_ind[i] for indices i between
     H_ptr[l] to H_ptr[l+1] - 1. C users should refer to

     for (l = 0; i < m+1; l++) {
       H_ptr[i]--;
     }
     for (l = 0; i < m; l++) {
       for (i = H_ptr[l]; i < H_ptr[l+1] - 1; i++} {
         H_ind[i]--;
       }
     }

  3. 2D arrays are returned in fortran in column-major order, while
     in C they are traditionally in row-major order. Thus
     the i,j entry of such an array A should refer to A[j][i].
     This is vital for rectangular and non-symmetric matrices such
     as Jacobians and compressed bands. Since Hessians are symmetric,
     there is no need to switch the indices in this case.

  Potential users should examine the provided CUTEst interfaces to
  C and C++ codes in the files

  test/utest.c and test/ctest.c
  derchk/derchk_main.c
  genc/genc.c and genc/genc_main.c
  gsl/gsl_main.c and gsl/gsl_test.c
  knitro/knitro_main.c
  loqo/loqo_main.c
  matlab/mcutest.c
  nomad/nomad_main.c
  octave/ocutest.c
  osqp/osqp_main.c
  osqp/osqp_test.c

  in the $CUTEST/src directory for examples of use.

  The CUTEst function xxx may be called in C as

    CUTEST_xxx(.)

** The second variant is C-oriented, and invoked as

#include "cutest_c.h"

This variant should be much more familiar to C users. In particular,
array indices are 0 based, and 2D arrays are returned in C row-major order.

The CUTEst function xxx may be called in C as

    CUTEST_xxx_c(.)

Examples of use for all CUTEst functions are provided in the files

    $CUTEST/src/test/utest_c.c and $CUTEST/src/test/ctest_c.c

If you need more help, contact the CUTEst team via

  https://github.com/ralna/CUTEst/discussions

October 26th 2024
