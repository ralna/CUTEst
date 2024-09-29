! THIS VERSION: CUTEST 2.2 - 2024-09-27 AT 11:00 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-*-  C U T E S T    I N T E R F A C E   M O D U L E  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!    November 10th 2000 (as a GALAHAD module) for CUTEr
!    February 6th 2013 for CUTEst

   MODULE CUTEST_INTERFACE_precision

!  Interface blocks for CUTEst fortran tools

     IMPLICIT NONE

     INTERFACE

!  Interface block for general tools

       SUBROUTINE CUTEST_classification_r( cutest_status, input,               &
                                           classification )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: input
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       CHARACTER ( LEN = 30 ), INTENT( OUT ) :: classification 
       END SUBROUTINE CUTEST_classification_r

       SUBROUTINE CUTEST_pname_r( cutest_status, input, pname )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: input
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       CHARACTER ( LEN = 10 ), INTENT( OUT ) :: pname
       END SUBROUTINE CUTEST_pname_r

       SUBROUTINE CUTEST_probname_r( cutest_status, p_name )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       CHARACTER ( LEN = 10 ), INTENT( OUT ) :: p_name
       END SUBROUTINE CUTEST_probname_r

       SUBROUTINE CUTEST_varnames_r( cutest_status, n, X_names )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( n ) :: X_names
       END SUBROUTINE CUTEST_varnames_r

!  Interface block for unconstrained tools

       SUBROUTINE CUTEST_udimen_r( cutest_status, input, n )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: input
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, n
       END SUBROUTINE CUTEST_udimen_r

       SUBROUTINE CUTEST_usetup_r( cutest_status, input, out, io_buffer,       &
                                   n, X, X_l, X_u )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: input, out, io_buffer
       INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: n
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: X, X_l, X_u
       END SUBROUTINE CUTEST_usetup_r

       SUBROUTINE CUTEST_unames_r( cutest_status, n, p_name, X_names )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       CHARACTER ( LEN = 10 ), INTENT( OUT ) :: p_name
       CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( n ) :: X_names
       END SUBROUTINE CUTEST_unames_r

       SUBROUTINE CUTEST_uvartype_r( cutest_status, n, X_type )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: X_type( n )
       END SUBROUTINE CUTEST_uvartype_r

       SUBROUTINE CUTEST_ufn_r( cutest_status, n, X, f )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( OUT ) :: f
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       END SUBROUTINE CUTEST_ufn_r

       SUBROUTINE CUTEST_ugr_r( cutest_status, n, X, G )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
       END SUBROUTINE CUTEST_ugr_r

       SUBROUTINE CUTEST_uofg_r( cutest_status, n, X, f, G, grad )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( OUT ) :: f
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
       LOGICAL, INTENT( IN ) :: grad
       END SUBROUTINE CUTEST_uofg_r

       SUBROUTINE CUTEST_udh_r( cutest_status, n, X, lh1, H )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lh1
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh1, n ) :: H
       END SUBROUTINE CUTEST_udh_r

       SUBROUTINE CUTEST_ugrdh_r( cutest_status, n, X, G, lh1, H )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lh1
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh1, n ) :: H
       END SUBROUTINE CUTEST_ugrdh_r

       SUBROUTINE CUTEST_udimsh_r( cutest_status, nnzh )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzh
       END SUBROUTINE CUTEST_udimsh_r

       SUBROUTINE CUTEST_ushp_r( cutest_status, n, nnzh, lh, IRNH, ICNH )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lh
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzh
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: IRNH, ICNH
       END SUBROUTINE CUTEST_ushp_r

       SUBROUTINE CUTEST_ush_r( cutest_status, n, X, nnzh, lh, H, IRNH, ICNH )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lh
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzh
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: IRNH, ICNH
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh ) :: H
       END SUBROUTINE CUTEST_ush_r

       SUBROUTINE CUTEST_ugrsh_r( cutest_status, n, X, G,                      &
                                  nnzh, lh, H, IRNH, ICNH )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lh
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzh
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: IRNH, ICNH
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh ) :: H
       END SUBROUTINE CUTEST_ugrsh_r

       SUBROUTINE CUTEST_udimse_r( cutest_status, ne, nnzh, nzirnh )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, ne, nnzh, nzirnh
       END SUBROUTINE CUTEST_udimse_r

       SUBROUTINE CUTEST_ueh_r( cutest_status, n, X, ne, le, IPRNHI, IPRHI,    &
                                lirnhi, IRNHI, lhi, Hi, byrows )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, le, lirnhi, lhi
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, ne
       LOGICAL, INTENT( IN ) :: byrows
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lirnhi ) :: IRNHI
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( le ) :: IPRNHI, IPRHI
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lhi ) :: HI
       END SUBROUTINE CUTEST_ueh_r

       SUBROUTINE CUTEST_ugreh_r( cutest_status, n, X, G, ne, le, IPRNHI,     &
                                  IPRHI, lirnhi, IRNHI, lhi, Hi, byrows )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, le, lirnhi, lhi
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, ne
       LOGICAL, INTENT( IN ) :: byrows
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lirnhi ) :: IRNHI
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( le ) :: IPRNHI, IPRHI
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lhi ) :: HI
       END SUBROUTINE CUTEST_ugreh_r

       SUBROUTINE CUTEST_uhprod_r( cutest_status, n, goth, X, P, RESULT )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       LOGICAL, INTENT( IN ) :: goth
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, P
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: RESULT
       END SUBROUTINE CUTEST_uhprod_r

       SUBROUTINE CUTEST_ushprod_r( cutest_status, n, goth, X,                 &
                                    nnz_vector, INDEX_nz_vector, VECTOR,       &
                                    nnz_result, INDEX_nz_result, RESULT )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, nnz_vector
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnz_result
       LOGICAL, INTENT( IN ) :: goth
       INTEGER ( KIND = ip_ ), DIMENSION( nnz_vector ),                        &
                               INTENT( IN ) :: INDEX_nz_vector
       INTEGER ( KIND = ip_ ), DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: RESULT
       END SUBROUTINE CUTEST_ushprod_r

       SUBROUTINE CUTEST_ubandh_r( cutest_status, n, X, nsemib, BANDH, lbandh, &
                                   maxsbw )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, nsemib, lbandh
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: maxsbw
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) ::  X
       REAL ( KIND = rp_ ), INTENT( OUT ),                                     &
                                DIMENSION( 0 : lbandh, n ) ::  BANDH
       END SUBROUTINE CUTEST_ubandh_r

       SUBROUTINE CUTEST_ureport_r( cutest_status, CALLS, CPU )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( 4 ) :: CALLS
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( 4 ) :: CPU
       END SUBROUTINE CUTEST_ureport_r

       SUBROUTINE CUTEST_uterminate_r( cutest_status )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       END SUBROUTINE CUTEST_uterminate_r

!  Interface block for constrained tools

       SUBROUTINE CUTEST_cdimen_r( cutest_status, input, n, m )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: input
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, n, m
       END SUBROUTINE CUTEST_cdimen_r

       SUBROUTINE CUTEST_cnoobj_r( status, input, noobj )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: input
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
       LOGICAL, INTENT( OUT ) :: noobj
       END SUBROUTINE CUTEST_cnoobj_r

       SUBROUTINE CUTEST_csetup_r( cutest_status, input, out, io_buffer,       &
                                   n, m, X, X_l, X_u,                          &
                                   Y, C_l, C_u, EQUATN, LINEAR,                &
                                   e_order, l_order, v_order )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) ::  input, out, io_buffer
       INTEGER ( KIND = ip_ ), INTENT( IN ) ::  e_order, l_order, v_order
       INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: n, m
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: X, X_l, X_u
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: Y, C_l, C_u
       LOGICAL, INTENT( OUT ), DIMENSION( m ) :: EQUATN, LINEAR
       END SUBROUTINE CUTEST_csetup_r

       SUBROUTINE CUTEST_cnames_r( cutest_status, n, m, p_name,                &
                                   X_names, C_names )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       CHARACTER ( LEN = 10 ), INTENT( OUT ) :: p_name
       CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( n ) :: X_names
       CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( m ) :: C_names
       END SUBROUTINE CUTEST_cnames_r

       SUBROUTINE CUTEST_connames_r( cutest_status, m, C_names )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: m
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( m ) :: C_names
       END SUBROUTINE CUTEST_connames_r

       SUBROUTINE CUTEST_cvartype_r( cutest_status, n, X_type )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: X_type( n )
       END SUBROUTINE CUTEST_cvartype_r

       SUBROUTINE CUTEST_cfn_r( cutest_status, n, m, X, f, C )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( OUT ) :: f
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: C
       END SUBROUTINE CUTEST_cfn_r

       SUBROUTINE CUTEST_cgr_r( cutest_status, n, m, X, Y, grlagf, G, jtrans,  &
                                lcjac1, lcjac2, CJAC  )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lcjac1, lcjac2
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       LOGICAL, INTENT( IN ) :: grlagf, jtrans
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
       REAL ( KIND = rp_ ), INTENT( OUT ),                                     &
                                DIMENSION( lcjac1, lcjac2 ) :: CJAC
       END SUBROUTINE CUTEST_cgr_r

       SUBROUTINE CUTEST_cofg_r( cutest_status, n, X, f, G, grad )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( OUT ) :: f
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
       LOGICAL, INTENT( IN ) :: grad
       END SUBROUTINE CUTEST_cofg_r

       SUBROUTINE CUTEST_cofsg_r( cutest_status, n, X, f, nnzg, lg,            &
                                  G_val, G_var, grad )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lg
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzg
       REAL ( KIND = rp_ ), INTENT( OUT ) :: f
       LOGICAL, INTENT( IN ) :: grad
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lg ) :: G_var
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lg ) :: G_val
       END SUBROUTINE CUTEST_cofsg_r

       SUBROUTINE CUTEST_cdimsg_r( cutest_status, nnzg )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzg
       END SUBROUTINE CUTEST_cdimsg_r

       SUBROUTINE CUTEST_cisgrp_r( cutest_status, n, iprob, nnzgr, lgr, GR_var )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, iprob, lgr
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzgr
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lgr ) :: GR_var
       END SUBROUTINE CUTEST_cisgrp_r

       SUBROUTINE CUTEST_cisgr_r( cutest_status, n, iprob, X, nnzgr, lgr,      &
                                  GR_val, GR_var )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, iprob, lgr
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzgr
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lgr ) :: GR_var
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lgr ) :: GR_val
       END SUBROUTINE CUTEST_cisgr_r

       SUBROUTINE CUTEST_cdimsj_r( cutest_status, nnzj )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzj
       END SUBROUTINE CUTEST_cdimsj_r

       SUBROUTINE CUTEST_csjp_r( cutest_status, nnzj, lj, J_var, J_fun )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: lj
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nnzj, cutest_status
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lj ) :: J_var, J_fun
       END SUBROUTINE CUTEST_csjp_r

       SUBROUTINE CUTEST_csgrp_r( cutest_status, n, nnzj, lj, J_var, J_fun )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lj
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nnzj, cutest_status
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lj ) :: J_var, J_fun
       END SUBROUTINE CUTEST_csgrp_r

       SUBROUTINE CUTEST_csgr_r( cutest_status, n, m, X, Y, grlagf, nnzj,      &
                                 lcjac, CJAC, INDVAR, INDFUN )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lcjac
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzj
       LOGICAL, INTENT( IN ) :: grlagf
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lcjac ) :: INDVAR
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lcjac ) :: INDFUN
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lcjac ) :: CJAC
       END SUBROUTINE CUTEST_csgr_r

       SUBROUTINE CUTEST_ccf_r( cutest_status, n, m, X, C )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: C
       END SUBROUTINE CUTEST_ccf_r

       SUBROUTINE CUTEST_ccfg_r( cutest_status, n, m, X, C, jtrans,            &
                                 lcjac1, lcjac2, CJAC, grad )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lcjac1, lcjac2
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: C
       REAL ( KIND = rp_ ), INTENT( OUT ),                                     &
                            DIMENSION( lcjac1, lcjac2 ) :: CJAC
       LOGICAL, INTENT( IN ) :: jtrans, grad
       END SUBROUTINE CUTEST_ccfg_r

       SUBROUTINE CUTEST_ccfsg_r( cutest_status, n, m, X, C, nnzj,             &
                                  lcjac, CJAC, INDVAR, INDFUN, grad )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lcjac
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzj
       LOGICAL, INTENT( IN ) :: grad
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lcjac ) :: INDVAR
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lcjac ) :: INDFUN
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: C
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lcjac ) :: CJAC
       END SUBROUTINE CUTEST_ccfsg_r

       SUBROUTINE CUTEST_clfg_r( cutest_status, n, m, X, Y, f, G, grad )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( OUT ) :: f
       LOGICAL, INTENT( IN ) :: grad
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
       END SUBROUTINE CUTEST_clfg_r

       SUBROUTINE CUTEST_ccifg_r( cutest_status, n, icon, X, ci, GCI, grad )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, icon
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       LOGICAL, INTENT( IN ) :: grad
       REAL ( KIND = rp_ ), INTENT( OUT ) :: ci
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: GCI
       END SUBROUTINE CUTEST_ccifg_r

       SUBROUTINE CUTEST_ccifsg_r( cutest_status, n, icon, X, ci,              &
                                   nnzgci, lgci, GCI, INDVAR, grad )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, icon, lgci
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzgci
       LOGICAL, INTENT( IN ) :: grad
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lgci ) :: INDVAR
       REAL ( KIND = rp_ ), INTENT( OUT ) :: ci
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lgci ) :: GCI
       END SUBROUTINE CUTEST_ccifsg_r

       SUBROUTINE CUTEST_cdh_r( cutest_status, n, m, X, Y, lh1, H )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lh1
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh1, n ) :: H
       END SUBROUTINE CUTEST_cdh_r

       SUBROUTINE CUTEST_cdhc_r( cutest_status, n, m, X, Y, lh1, H )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lh1
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh1, n ) :: H
       END SUBROUTINE CUTEST_cdhc_r

       SUBROUTINE CUTEST_cidh_r( cutest_status, n, X, iprob, lh1, H )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, iprob, lh1
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh1, n ) :: H
       END SUBROUTINE CUTEST_cidh_r

       SUBROUTINE CUTEST_cgrdh_r( cutest_status, n, m, X, Y, grlagf, G,        &
                                  jtrans, lcjac1, lcjac2, CJAC, lh1, H     )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lh1, lcjac1, lcjac2
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       LOGICAL, INTENT( IN ) :: grlagf, jtrans
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh1, n ) :: H
       REAL ( KIND = rp_ ), INTENT( OUT ),                                     &
                                DIMENSION( lcjac1, lcjac2 ) :: CJAC
       END SUBROUTINE CUTEST_cgrdh_r

       SUBROUTINE CUTEST_cdimsh_r( cutest_status, nnzh )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzh
       END SUBROUTINE CUTEST_cdimsh_r

       SUBROUTINE CUTEST_cshp_r( cutest_status, n, nnzh, lh, IRNH, ICNH )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lh
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzh
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: IRNH, ICNH
       END SUBROUTINE CUTEST_cshp_r

       SUBROUTINE CUTEST_csh_r( cutest_status, n, m, X, Y,                     &
                                nnzh, lh, H, IRNH, ICNH  )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lh
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzh
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: IRNH, ICNH
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh ) :: H
       END SUBROUTINE CUTEST_csh_r

       SUBROUTINE CUTEST_cshj_r( cutest_status, n, m, X, y0, Y,                &
                                 nnzh, lh, H_val, H_row, H_col )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lh
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nnzh, cutest_status
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: H_row, H_col
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ) :: y0
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh ) :: H_val
       END SUBROUTINE CUTEST_cshj_r

       SUBROUTINE CUTEST_cshc_r( cutest_status, n, m, X, Y,                    &
                                 nnzh, lh, H, IRNH, ICNH  )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lh
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzh
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: IRNH, ICNH
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh ) :: H
       END SUBROUTINE CUTEST_cshc_r

       SUBROUTINE CUTEST_cish_r( cutest_status, n, X, iprob,                   &
                                 nnzh, lh, H, IRNH, ICNH  )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, iprob, lh
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzh
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: IRNH, ICNH
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh ) :: H
       END SUBROUTINE CUTEST_cish_r

       SUBROUTINE CUTEST_csgrshp_r( cutest_status, n, nnzj, lj, J_var, J_fun,  &
                                    nnzh, lh, H_row, H_col )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lj, lh
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nnzh, nnzj, cutest_status
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lj ) :: J_var, J_fun
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: H_row, H_col
       END SUBROUTINE CUTEST_csgrshp_r

       SUBROUTINE CUTEST_csgrsh_r( cutest_status, n, m, X, Y, grlagf, nnzj,    &
                                   lcjac, CJAC, INDVAR, INDFUN, nnzh,          &
                                   lh, H, IRNH, ICNH  )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lcjac, lh
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzj, nnzh
       LOGICAL, INTENT( IN ) ::  grlagf
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lcjac ) :: INDVAR
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lcjac ) :: INDFUN
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: IRNH, ICNH
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh ) :: H
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lcjac ) :: CJAC
       END SUBROUTINE CUTEST_csgrsh_r

       SUBROUTINE CUTEST_cdimse_r( cutest_status, ne, nnzh, nzirnh )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, ne, nnzh, nzirnh
       END SUBROUTINE CUTEST_cdimse_r

       SUBROUTINE CUTEST_ceh_r( cutest_status, n, m, X, Y, ne, le, IPRNHI,     &
                                IPRHI, lirnhi, IRNHI, lhi, Hi, byrows )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, le, lirnhi, lhi
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, ne
       LOGICAL, INTENT( IN ) :: byrows
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lirnhi ) :: IRNHI
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( le ) :: IPRNHI, IPRHI
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lhi ) :: HI
       END SUBROUTINE CUTEST_ceh_r

       SUBROUTINE CUTEST_csgreh_r( cutest_status, n, m, X, Y, grlagf,          &
                                   nnzj, lcjac, CJAC, INDVAR, INDFUN,          &
                                   ne, le, IPRNHI, IPRHI, lirnhi, IRNHI, lhi,  &
                                   Hi, byrows )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lcjac, le, lirnhi, lhi
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, ne, nnzj
       LOGICAL, INTENT( IN ) :: grlagf, byrows
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lcjac ) :: INDVAR
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lcjac ) :: INDFUN
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lirnhi ) :: IRNHI
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( le ) :: IPRNHI, IPRHI
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lhi ) :: HI
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lcjac ) :: CJAC
       END SUBROUTINE CUTEST_csgreh_r

       SUBROUTINE CUTEST_chprod_r( cutest_status, n, m, goth, X, Y, P, RESULT )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       LOGICAL, INTENT( IN ) :: goth
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, P
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: RESULT
       END SUBROUTINE CUTEST_chprod_r

       SUBROUTINE CUTEST_chjprod_r( cutest_status, n, m, goth, X, y0, Y,       &
                                    VECTOR, RESULT )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       LOGICAL, INTENT( IN ) :: goth
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
       REAL ( KIND = rp_ ), INTENT( IN ) :: y0
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: RESULT
       END SUBROUTINE CUTEST_chjprod_r

       SUBROUTINE CUTEST_cshprod_r( cutest_status, n, m, goth, X, Y,           &
                                    nnz_vector, INDEX_nz_vector, VECTOR,       &
                                    nnz_result, INDEX_nz_result, RESULT )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, nnz_vector
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnz_result
       LOGICAL, INTENT( IN ) :: goth
       INTEGER ( KIND = ip_ ), DIMENSION( nnz_vector ),                        &
                               INTENT( IN ) :: INDEX_nz_vector
       INTEGER ( KIND = ip_ ), DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: RESULT
       END SUBROUTINE CUTEST_cshprod_r

       SUBROUTINE CUTEST_chcprod_r( cutest_status, n, m, goth, X, Y, P, RESULT )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       LOGICAL, INTENT( IN ) :: goth
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, P
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: RESULT
       END SUBROUTINE CUTEST_chcprod_r

       SUBROUTINE CUTEST_cshcprod_r( cutest_status, n, m, goth, X, Y,          &
                                     nnz_vector, INDEX_nz_vector, VECTOR,      &
                                     nnz_result, INDEX_nz_result, RESULT )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, nnz_vector
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnz_result
       LOGICAL, INTENT( IN ) :: goth
       INTEGER ( KIND = ip_ ), DIMENSION( nnz_vector ),                        &
                               INTENT( IN ) :: INDEX_nz_vector
       INTEGER ( KIND = ip_ ), DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: RESULT
       END SUBROUTINE CUTEST_cshcprod_r

       SUBROUTINE CUTEST_cjprod_r( cutest_status, n, m, gotj, jtrans, X,       &
                                   VECTOR, lvector, RESULT, lresult )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lvector, lresult
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       LOGICAL, INTENT( IN ) :: gotj, jtrans
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( lvector ) :: VECTOR
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lresult ) :: RESULT
       END SUBROUTINE CUTEST_cjprod_r

       SUBROUTINE CUTEST_csjprod_r( cutest_status, n, m, gotj, jtrans, X,      &
                                  nnz_vector, INDEX_nz_vector, VECTOR, lvector,&
                                  nnz_result, INDEX_nz_result, RESULT, lresult )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, nnz_vector
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: lvector, lresult
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnz_result
       LOGICAL, INTENT( IN ) :: gotj, jtrans
       INTEGER ( KIND = ip_ ), DIMENSION( nnz_vector ),                        &
                               INTENT( IN ) :: INDEX_nz_vector
       INTEGER ( KIND = ip_ ), DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( lvector ) :: VECTOR
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lresult ) :: RESULT
       END SUBROUTINE CUTEST_csjprod_r

       SUBROUTINE CUTEST_cdimohp_r( cutest_status, nnzohp )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzohp
       END SUBROUTINE CUTEST_cdimohp_r

       SUBROUTINE CUTEST_cohprodsp_r( cutest_status, nnzohp, lp, IND )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: lp
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzohp
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lp ) :: IND
       END SUBROUTINE CUTEST_cohprodsp_r

       SUBROUTINE CUTEST_cohprods_r( cutest_status, n, goth, X, VECTOR,        &
                                     nnzohp, lp, RESULT, IND )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lp
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzohp
       LOGICAL, INTENT( IN ) :: goth
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lp ) :: IND
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lp ) :: RESULT
       END SUBROUTINE CUTEST_cohprods_r

       SUBROUTINE CUTEST_cdimchp_r( cutest_status, nnzchp )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzchp
       END SUBROUTINE CUTEST_cdimchp_r

       SUBROUTINE CUTEST_cchprodsp_r( cutest_status, m, lchp, CHP_ind, CHP_ptr )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: m, lchp
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( m + 1 ) :: CHP_ptr
       INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( lchp ) :: CHP_ind
       END SUBROUTINE CUTEST_cchprodsp_r

       SUBROUTINE CUTEST_cchprods_r( cutest_status, n, m, goth, X, VECTOR,     &
                                     lchp, CHP_val, CHP_ind, CHP_ptr )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lchp
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       LOGICAL, INTENT( IN ) :: goth
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
       INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( m + 1 ) :: CHP_ptr
       INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( lchp ) :: CHP_ind
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lchp ) :: CHP_val
       END SUBROUTINE CUTEST_cchprods_r

       SUBROUTINE CUTEST_creport_r( cutest_status, CALLS, CPU )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( 7 ) :: CALLS
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( 4 ) :: CPU
       END SUBROUTINE CUTEST_creport_r

       SUBROUTINE CUTEST_cstats_r( cutest_status,                              &
                                   nonlinear_variables_objective,              &
                                   nonlinear_variables_constraints,            &
                                   equality_constraints, linear_constraint )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nonlinear_variables_objective
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nonlinear_variables_constraints
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: equality_constraints
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: linear_constraint
       END SUBROUTINE CUTEST_cstats_r

       SUBROUTINE CUTEST_cterminate_r( cutest_status )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       END SUBROUTINE CUTEST_cterminate_r

!  Interface block for threaded unconstrained tools

       SUBROUTINE CUTEST_usetup_threaded_r( cutest_status, input, out,         &
                                            threads, IO_BUFFERS, n, X,         &
                                             X_l, X_u )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: input, out, threads
       INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: n
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( threads ) :: IO_BUFFERS
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: X, X_l, X_u
       END SUBROUTINE CUTEST_usetup_threaded_r

       SUBROUTINE CUTEST_ufn_threaded_r( cutest_status, n, X, f, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( OUT ) :: f
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       END SUBROUTINE CUTEST_ufn_threaded_r

       SUBROUTINE CUTEST_ugr_threaded_r( cutest_status, n, X, G, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
       END SUBROUTINE CUTEST_ugr_threaded_r

       SUBROUTINE CUTEST_uofg_threaded_r( cutest_status, n, X, f, G, grad,     &
                                          thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( OUT ) :: f
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
       LOGICAL, INTENT( IN ) :: grad
       END SUBROUTINE CUTEST_uofg_threaded_r

       SUBROUTINE CUTEST_udh_threaded_r( cutest_status, n, X, lh1, H, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lh1, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh1, n ) :: H
       END SUBROUTINE CUTEST_udh_threaded_r

       SUBROUTINE CUTEST_ugrdh_threaded_r( cutest_status, n, X, G,             &
                                           lh1, H, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lh1, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh1, n ) :: H
       END SUBROUTINE CUTEST_ugrdh_threaded_r

       SUBROUTINE CUTEST_ush_threaded_r( cutest_status, n, X,                  &
                                         nnzh, lh, H, IRNH, ICNH, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lh, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzh
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: IRNH, ICNH
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh ) :: H
       END SUBROUTINE CUTEST_ush_threaded_r

       SUBROUTINE CUTEST_ugrsh_threaded_r( cutest_status, n, X, G,             &
                                           nnzh, lh, H, IRNH, ICNH, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lh, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzh
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: IRNH, ICNH
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh ) :: H
       END SUBROUTINE CUTEST_ugrsh_threaded_r

       SUBROUTINE CUTEST_ueh_threaded_r( cutest_status, n, X, ne, le,          &
                                         IPRNHI, IPRHI, lirnhi, IRNHI,         &
                                         lhi, Hi, byrows, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, ne, le, lirnhi, lhi, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       LOGICAL, INTENT( IN ) :: byrows
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lirnhi ) :: IRNHI
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( le ) :: IPRNHI, IPRHI
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lhi ) :: HI
       END SUBROUTINE CUTEST_ueh_threaded_r

       SUBROUTINE CUTEST_ugreh_threaded_r( cutest_status, n, X, G, ne, le,     &
                                           IPRNHI, IPRHI, lirnhi, IRNHI, lhi,  &
                                           Hi, byrows, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, le, lirnhi, lhi, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, ne
       LOGICAL, INTENT( IN ) :: byrows
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lirnhi ) :: IRNHI
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( le ) :: IPRNHI, IPRHI
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lhi ) :: HI
       END SUBROUTINE CUTEST_ugreh_threaded_r

       SUBROUTINE CUTEST_uhprod_threaded_r( cutest_status, n, goth, X, P,      &
                                            RESULT, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       LOGICAL, INTENT( IN ) :: goth
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, P
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: RESULT
       END SUBROUTINE CUTEST_uhprod_threaded_r

       SUBROUTINE CUTEST_ushprod_threaded_r( cutest_status, n, goth, X,        &
                                     nnz_vector, INDEX_nz_vector, VECTOR,      &
                                     nnz_result, INDEX_nz_result, RESULT,      &
                                     thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, nnz_vector, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnz_result
       LOGICAL, INTENT( IN ) :: goth
       INTEGER ( KIND = ip_ ), DIMENSION( nnz_vector ),                        &
                               INTENT( IN ) :: INDEX_nz_vector
       INTEGER ( KIND = ip_ ), DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: RESULT
       END SUBROUTINE CUTEST_ushprod_threaded_r

       SUBROUTINE CUTEST_ubandh_threaded_r( cutest_status, n, X, nsemib,       &
                                            BANDH, lbandh, maxsbw, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, nsemib, lbandh, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, maxsbw
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) ::  X
       REAL ( KIND = rp_ ), INTENT( OUT ),                                     &
                            DIMENSION( 0 : lbandh, n ) ::  BANDH
       END SUBROUTINE CUTEST_ubandh_threaded_r

       SUBROUTINE CUTEST_ureport_threaded_r( cutest_status, CALLS, CPU, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( 4 ) :: CALLS
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( 4 ) :: CPU
       END SUBROUTINE CUTEST_ureport_threaded_r

!  Interface block for threaded constrained tools

       SUBROUTINE CUTEST_csetup_threaded_r( cutest_status, input, out,         &
                                            threads, IO_BUFFERS,               &
                                            n, m, X, X_l, X_u,                 &
                                            Y, C_l, C_u, EQUATN, LINEAR,       &
                                            e_order, l_order, v_order )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) ::  input, out, threads
       INTEGER ( KIND = ip_ ), INTENT( IN ) ::  e_order, l_order, v_order
       INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: n, m
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( threads ) :: IO_BUFFERS
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: X, X_l, X_u
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: Y, C_l, C_u
       LOGICAL, INTENT( OUT ), DIMENSION( m ) :: EQUATN, LINEAR
       END SUBROUTINE CUTEST_csetup_threaded_r

       SUBROUTINE CUTEST_cfn_threaded_r( cutest_status, n, m, X, f, C, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( OUT ) :: f
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: C
       END SUBROUTINE CUTEST_cfn_threaded_r

       SUBROUTINE CUTEST_cgr_threaded_r( cutest_status, n, m, X, Y, grlagf, G, &
                                        jtrans, lcjac1, lcjac2, CJAC , thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lcjac1, lcjac2, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       LOGICAL, INTENT( IN ) :: grlagf, jtrans
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lcjac1, lcjac2 ) :: CJAC
       END SUBROUTINE CUTEST_cgr_threaded_r

       SUBROUTINE CUTEST_cofg_threaded_r( cutest_status, n, X, f, G, grad,     &
                                          thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( OUT ) :: f
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
       LOGICAL, INTENT( IN ) :: grad
       END SUBROUTINE CUTEST_cofg_threaded_r

       SUBROUTINE CUTEST_csgrp_threaded_r( cutest_status, n, nnzj, lj,         &
                                           J_var, J_fun, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, lj, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nnzj, cutest_status
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lj ) :: J_var, J_fun
       END SUBROUTINE CUTEST_csgrp_threaded_r

       SUBROUTINE CUTEST_csgr_threaded_r( cutest_status, n, m, X, Y, grlagf,   &
                                          nnzj, lcjac, CJAC, INDVAR, INDFUN,   &
                                          thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzj
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lcjac, thread
       LOGICAL, INTENT( IN ) :: grlagf
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lcjac ) :: INDVAR
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lcjac ) :: INDFUN
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lcjac ) :: CJAC
       END SUBROUTINE CUTEST_csgr_threaded_r

       SUBROUTINE CUTEST_cisgrp_threaded_r( cutest_status, n, iprob, nnzgr,    &
                                            lgr, GR_var, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, iprob, lgr, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzgr
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lgr ) :: GR_var
       END SUBROUTINE CUTEST_cisgrp_threaded_r

       SUBROUTINE CUTEST_cisgr_threaded_r( cutest_status, n, iprob, X, nnzgr,  &
                                           lgr, GR_val, GR_var, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, iprob, lgr, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzgr
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lgr ) :: GR_var
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lgr ) :: GR_val
       END SUBROUTINE CUTEST_cisgr_threaded_r

       SUBROUTINE CUTEST_ccf_threaded_r( cutest_status, n, m, X, C, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: C
       END SUBROUTINE CUTEST_ccf_threaded_r

       SUBROUTINE CUTEST_ccfg_threaded_r( cutest_status, n, m, X, C, jtrans,   &
                                          lcjac1, lcjac2, CJAC, grad, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lcjac1, lcjac2, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: C
       REAL ( KIND = rp_ ), INTENT( OUT ),                                     &
                                DIMENSION( lcjac1, lcjac2 ) :: CJAC
       LOGICAL, INTENT( IN ) :: jtrans, grad
       END SUBROUTINE CUTEST_ccfg_threaded_r

       SUBROUTINE CUTEST_ccfsg_threaded_r( cutest_status, n, m, X, C, nnzj,    &
                                           lcjac, CJAC, INDVAR, INDFUN, grad,  &
                                           thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzj
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lcjac, thread
       LOGICAL, INTENT( IN ) :: grad
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lcjac ) :: INDVAR
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lcjac ) :: INDFUN
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: C
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lcjac ) :: CJAC
       END SUBROUTINE CUTEST_ccfsg_threaded_r

       SUBROUTINE CUTEST_ccifg_threaded_r( cutest_status, n, icon, X, ci, GCI, &
                                           grad, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, icon, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       LOGICAL, INTENT( IN ) :: grad
       REAL ( KIND = rp_ ), INTENT( OUT ) :: ci
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: GCI
       END SUBROUTINE CUTEST_ccifg_threaded_r

       SUBROUTINE CUTEST_ccifsg_threaded_r( cutest_status, n, icon, X, ci,     &
                                            nnzgci, lgci, GCI, INDVAR, grad,   &
                                            thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, icon, lgci, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzgci
       LOGICAL, INTENT( IN ) :: grad
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lgci ) :: INDVAR
       REAL ( KIND = rp_ ), INTENT( OUT ) :: ci
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lgci ) :: GCI
       END SUBROUTINE CUTEST_ccifsg_threaded_r

       SUBROUTINE CUTEST_cdh_threaded_r( cutest_status, n, m, X, Y,            &
                                         lh1, H, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lh1, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh1, n ) :: H
       END SUBROUTINE CUTEST_cdh_threaded_r

       SUBROUTINE CUTEST_cidh_threaded_r( cutest_status, n, X, iprob,          &
                                          lh1, H, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, iprob, lh1, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh1, n ) :: H
       END SUBROUTINE CUTEST_cidh_threaded_r

       SUBROUTINE CUTEST_cgrdh_threaded_r( cutest_status, n, m, X, Y, grlagf,  &
                                           G, jtrans, lcjac1, lcjac2, CJAC,    &
                                           lh1, H, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lh1, lcjac1, lcjac2, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       LOGICAL, INTENT( IN ) :: grlagf, jtrans
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: G
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh1, n ) :: H
       REAL ( KIND = rp_ ), INTENT( OUT ),                                     &
                                DIMENSION( lcjac1, lcjac2 ) :: CJAC
       END SUBROUTINE CUTEST_cgrdh_threaded_r

       SUBROUTINE CUTEST_csh_threaded_r( cutest_status, n, m, X, Y,            &
                                         nnzh, lh, H, IRNH, ICNH , thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lh, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzh
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: IRNH, ICNH
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh ) :: H
       END SUBROUTINE CUTEST_csh_threaded_r

       SUBROUTINE CUTEST_cshj_threaded_r( cutest_status, n, m, X, y0, Y, nnzh, &
                                          lh, H_val, H_row, H_col, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lh, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nnzh, cutest_status
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: H_row, H_col
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ) :: y0
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh ) :: H_val
       END SUBROUTINE CUTEST_cshj_threaded_r

       SUBROUTINE CUTEST_cshc_threaded_r( cutest_status, n, m, X, Y,           &
                                          nnzh, lh, H, IRNH, ICNH , thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzh
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lh, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: IRNH, ICNH
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh ) :: H
       END SUBROUTINE CUTEST_cshc_threaded_r

       SUBROUTINE CUTEST_cish_threaded_r( cutest_status, n, X, iprob,          &
                                          nnzh, lh, H, IRNH, ICNH , thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, iprob, lh, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzh
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: IRNH, ICNH
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh ) :: H
       END SUBROUTINE CUTEST_cish_threaded_r

       SUBROUTINE CUTEST_csgrsh_threaded_r( cutest_status, n, m, X, Y, grlagf, &
                                            nnzj, lcjac, CJAC, INDVAR, INDFUN, &
                                            nnzh, lh, H, IRNH, ICNH , thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lcjac, lh, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnzj, nnzh
       LOGICAL, INTENT( IN ) ::  grlagf
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lcjac ) :: INDVAR
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lcjac ) :: INDFUN
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lh ) :: IRNH, ICNH
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lh ) :: H
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lcjac ) :: CJAC
       END SUBROUTINE CUTEST_csgrsh_threaded_r

       SUBROUTINE CUTEST_ceh_threaded_r( cutest_status, n, m, X, Y, ne, le,    &
                                         IPRNHI, IPRHI, lirnhi, IRNHI, lhi,    &
                                         HI, byrows, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, le, lirnhi, lhi, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, ne
       LOGICAL, INTENT( IN ) :: byrows
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lirnhi ) :: IRNHI
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( le ) :: IPRNHI, IPRHI
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lhi ) :: HI
       END SUBROUTINE CUTEST_ceh_threaded_r

       SUBROUTINE CUTEST_csgreh_threaded_r( cutest_status, n, m, X, Y, grlagf, &
                                            nnzj, lcjac, CJAC, INDVAR, INDFUN, &
                                            ne, le, IPRNHI, IPRHI, lirnhi,     &
                                            IRNHI, lhi, HI, byrows, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lcjac, le, lirnhi, lhi
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, ne, nnzj
       LOGICAL, INTENT( IN ) :: grlagf, byrows
       INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( lcjac ) :: INDVAR
       INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( lcjac ) :: INDFUN
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( lirnhi ) :: IRNHI
       INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( le ) :: IPRNHI, IPRHI
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lhi ) :: HI
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lcjac ) :: CJAC
       END SUBROUTINE CUTEST_csgreh_threaded_r

       SUBROUTINE CUTEST_chprod_threaded_r( cutest_status, n, m, goth,         &
                                            X, Y, P, RESULT, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       LOGICAL, INTENT( IN ) :: goth
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, P
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: RESULT
       END SUBROUTINE CUTEST_chprod_threaded_r

       SUBROUTINE CUTEST_cshprod_threaded_r( cutest_status, n, m, goth, X, Y,  &
                                     nnz_vector, INDEX_nz_vector, VECTOR,      &
                                     nnz_result, INDEX_nz_result, RESULT,      &
                                     thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, nnz_vector, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnz_result
       LOGICAL, INTENT( IN ) :: goth
       INTEGER ( KIND = ip_ ), DIMENSION( nnz_vector ),                        &
                               INTENT( IN ) :: INDEX_nz_vector
       INTEGER ( KIND = ip_ ), DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: RESULT
       END SUBROUTINE CUTEST_cshprod_threaded_r

       SUBROUTINE CUTEST_chcprod_threaded_r( cutest_status, n, m, goth,        &
                                           X, Y, P, RESULT, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       LOGICAL, INTENT( IN ) :: goth
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, P
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: RESULT
       END SUBROUTINE CUTEST_chcprod_threaded_r

       SUBROUTINE CUTEST_cshcprod_threaded_r( cutest_status, n, m, goth,       &
                                   X, Y, nnz_vector, INDEX_nz_vector, VECTOR,  &
                                   nnz_result, INDEX_nz_result, RESULT,        &
                                   thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, nnz_vector, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status, nnz_result
       LOGICAL, INTENT( IN ) :: goth
       INTEGER ( KIND = ip_ ), DIMENSION( nnz_vector ),                        &
                               INTENT( IN ) :: INDEX_nz_vector
       INTEGER ( KIND = ip_ ), DIMENSION( n ), INTENT( OUT ) :: INDEX_nz_result
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( m ) :: Y
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: RESULT
       END SUBROUTINE CUTEST_cshcprod_threaded_r

       SUBROUTINE CUTEST_cjprod_threaded_r( cutest_status, n, m, gotj, X,      &
                                            VECTOR, lvector, RESULT, lresult,  &
                                            thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lvector, lresult, thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       LOGICAL, INTENT( IN ) :: gotj
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( lvector ) :: VECTOR
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lresult ) :: RESULT
       END SUBROUTINE CUTEST_cjprod_threaded_r

       SUBROUTINE CUTEST_cchprods_threaded_r( cutest_status, n, m, goth, X,    &
                                              VECTOR, lchp, CHP_val, CHP_ind,  &
                                              CHP_ptr )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m, lchp
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       LOGICAL, INTENT( IN ) :: goth
       REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X, VECTOR
       INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( m + 1 ) :: CHP_ptr
       INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( lchp ) :: CHP_ind
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( lchp ) :: CHP_val
       END SUBROUTINE CUTEST_cchprods_threaded_r

       SUBROUTINE CUTEST_creport_threaded_r( cutest_status, CALLS, CPU, thread )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: thread
       INTEGER ( KIND = ip_ ), INTENT( OUT ) :: cutest_status
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( 7 ) :: CALLS
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( 4 ) :: CPU
       END SUBROUTINE CUTEST_creport_threaded_r

     END INTERFACE

!  End of module CUTEST_INTERFACE_precision

   END MODULE CUTEST_INTERFACE_precision
