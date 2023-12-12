! THIS VERSION: CUTEST 2.2 - 2023-11-29 AT 08:30 GMT.

#include "cutest_modules.h"

!  slimline CUTEst interface HiGHS
!  Nick Gould, November 2023

!=============================================================================
! abreviated header modified from 
!  github.com/ERGO-Code/HiGHS/blob/master/src/interfaces/highs_fortran_api.f90
! 29 Nov 2023
!=============================================================================

MODULE highs_fortran_api

  USE, INTRINSIC :: iso_c_binding
  USE CUTEST_KINDS_precision

CONTAINS

    FUNCTION Highs_run ( h ) result ( s ) bind( c, name='Highs_run' )
      USE iso_c_binding
      TYPE ( c_ptr ), VALUE :: h
      INTEGER ( ipc_ ) :: s
      s = 0_ipc_
    END FUNCTION Highs_run

    FUNCTION Highs_getModelStatus (h) &
        result(model_status) bind(c, name='Highs_getModelStatus')
      USE iso_c_binding
      TYPE ( c_ptr ), VALUE :: h
      INTEGER ( ipc_ ) :: model_status
      s = 0_ipc_
    END FUNCTION Highs_getModelStatus

    FUNCTION Highs_passLp ( h, numcol, numrow, numnz, aformat,&
         sense, offset, colcost, collower, colupper, rowlower, rowupper, &
         astart, aindex, avalue) result ( s ) bind ( c, name='Highs_passLp' )
      USE iso_c_binding
      TYPE ( c_ptr ), VALUE :: h
      INTEGER ( ipc_ ), VALUE :: numcol
      INTEGER ( ipc_ ), VALUE :: numrow
      INTEGER ( ipc_ ), VALUE :: numnz
      INTEGER ( ipc_ ), VALUE :: aformat
      INTEGER ( ipc_ ), VALUE :: sense
      REAL ( rpc_ ), VALUE :: offset
      REAL ( rpc_ ) :: colcost(*)
      REAL ( rpc_ ) :: collower(*)
      REAL ( rpc_ ) :: colupper(*)
      REAL ( rpc_ ) :: rowlower(*)
      REAL ( rpc_ ) :: rowupper(*)
      INTEGER ( ipc_ ) :: astart(*)
      INTEGER ( ipc_ ) :: aindex(*)
      REAL ( rpc_ ) :: avalue(*)
      INTEGER ( ipc_ ) :: s
      s = 0_ipc_
    END FUNCTION Highs_passLp

    FUNCTION Highs_passHessian ( h, dim, numnz, qformat, qstart, &
        qindex, qvalue) result ( s ) bind ( c, name='Highs_passHessian' )
      USE iso_c_binding
      TYPE ( c_ptr ), VALUE :: h
      INTEGER ( ipc_ ), VALUE :: dim
      INTEGER ( ipc_ ), VALUE :: numnz
      INTEGER ( ipc_ ), VALUE :: qformat
      INTEGER ( ipc_ ) :: qstart(*)
      INTEGER ( ipc_ ) :: qindex(*)
      REAL ( rpc_ ) :: qvalue(*)
      INTEGER ( ipc_ ) :: s
      s = 0_ipc_
    END FUNCTION Highs_passHessian

    FUNCTION Highs_create () result ( h ) bind( c, name='Highs_create' )
      USE iso_c_binding
      TYPE ( c_ptr ) :: h
    END FUNCTION Highs_create

    SUBROUTINE Highs_destroy ( h ) bind( c, name='Highs_destroy' )
      USE iso_c_binding
      TYPE ( c_ptr ), VALUE :: h 
    END SUBROUTINE Highs_destroy

    FUNCTION Highs_setBoolOptionValue ( h, o, v ) &
        result( s ) bind ( c, name='Highs_setBoolOptionValue' )
      USE iso_c_binding
      TYPE ( c_ptr ), VALUE :: h
      CHARACTER ( c_char ) :: o(*)
      LOGICAL ( c_bool ), VALUE :: v
      INTEGER ( ipc_ ) :: s
      s = 0_ipc_
    END FUNCTION Highs_setBoolOptionValue

    FUNCTION Highs_setIntOptionValue ( h, o, v ) &
        result( s ) bind ( c, name='Highs_setIntOptionValue' )
      USE iso_c_binding
      TYPE ( c_ptr ), VALUE :: h
      CHARACTER ( c_char ) :: o(*)
      INTEGER ( ipc_ ), VALUE :: v
      INTEGER ( ipc_ ) :: s
      s = 0_ipc_
    END FUNCTION Highs_setIntOptionValue

    FUNCTION Highs_setDoubleOptionValue ( h, o, v ) &
        result( s ) bind ( c, name='Highs_setDoubleOptionValue' )
      USE iso_c_binding
      TYPE ( c_ptr ), VALUE :: h
      CHARACTER ( c_char ) :: o(*)
      REAL ( rpc_ ), VALUE :: v
      INTEGER ( ipc_ ) :: s
      s = 0_ipc_
    END FUNCTION Highs_setDoubleOptionValue

   FUNCTION Highs_setStringOptionValue ( h, o, v ) &
       result( s ) bind ( c, name='Highs_setStringOptionValue' )
     USE iso_c_binding
     TYPE ( c_ptr ), VALUE :: h
     CHARACTER ( c_char ) :: o(*)
     CHARACTER ( c_char ) :: v(*)
     INTEGER ( ipc_ ) :: s
     s = 0_ipc_
   END FUNCTION Highs_setStringOptionValue

    FUNCTION Highs_setOptionValue ( h, o, v ) &
        result( s ) bind ( c, name='Highs_setOptionValue' )
      USE iso_c_binding
      TYPE ( c_ptr ), VALUE :: h
      CHARACTER ( c_char ) :: o(*)
      CHARACTER ( c_char ) :: v(*)
      INTEGER ( ipc_ ) :: s
      s = 0_ipc_
    END FUNCTION Highs_setOptionValue

    FUNCTION Highs_getIntInfoValue ( h, o, v ) &
        result( s ) bind ( c, name='Highs_getIntInfoValue' )
      USE iso_c_binding
      TYPE ( c_ptr ), VALUE :: h
      CHARACTER ( c_char ) :: o(*)
      INTEGER ( ipc_ ) :: v
      INTEGER ( ipc_ ) :: s
      v = 0_ipc_
      s = 0_ipc_
    END FUNCTION Highs_getIntInfoValue

    FUNCTION Highs_getDoubleInfoValue ( h, o, v ) &
        result( s ) bind ( c, name='Highs_getDoubleInfoValue' )
      USE iso_c_binding
      TYPE ( c_ptr ), VALUE :: h
      CHARACTER ( c_char ) :: o(*)
      REAL ( rpc_ ) :: v
      INTEGER ( ipc_ ) :: s
      v = 0.0_rpc_
      s = 0_ipc_
    END FUNCTION Highs_getDoubleInfoValue

    FUNCTION Highs_getSolution (h, cv, cd, rv, rd) &
        result ( s ) bind ( c, name='Highs_getSolution' )
      USE iso_c_binding
      TYPE ( c_ptr ), VALUE :: h
      REAL ( rpc_ ) :: cv(*)
      REAL ( rpc_ ) :: cd(*)
      REAL ( rpc_ ) :: rv(*)
      REAL ( rpc_ ) :: rd(*)
      INTEGER ( ipc_ ) :: s
      s = 0_ipc_
    END FUNCTION Highs_getSolution

    FUNCTION Highs_getBasis (h, cbs, rbs) result( s ) &
        bind (c, name='Highs_getBasis')
      USE iso_c_binding
      TYPE ( c_ptr ), VALUE :: h
      INTEGER ( ipc_ ) :: cbs(*)
      INTEGER ( ipc_ ) :: rbs(*)
      INTEGER ( ipc_ ) :: s
      s = 0_ipc_
    END FUNCTION Highs_getBasis

END MODULE highs_fortran_api
