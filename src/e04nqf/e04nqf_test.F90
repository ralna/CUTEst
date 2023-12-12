! THIS VERSION: CUTEST 2.2 - 2023-11-25 AT 15:30 GMT.

#include "cutest_modules.h"

!  slimline CUTEst interface E04NQF
!  Nick Gould, March 2021

!=============================================================================
! abreviated header from 
!  https://www.nag.com/numeric/fl/nagdoc_latest/html/e04/e04nqf.html
! 12 Mar 2021 
!=============================================================================

SUBROUTINE E04NQF( start, qphx, m, n, ne, nname, lenc, ncolh, iobj, objadd,    &
                   prob, acol, inda, loca, bl, bu, c, names, helast, hs,       &
                   x, pi, rc, ns, ninf, sinf, obj, cw, lencw, iw, leniw,       &
                   rw, lenrw, cuser, iuser, ruser, ifail )
  USE CUTEST_KINDS_precision
  IMPLICIT NONE
  INTEGER ( KIND = ip_ ), INTENT( IN ) :: m, n, ne, nname, lenc, ncolh, iobj
  INTEGER ( KIND = ip_ ), INTENT( IN ) :: inda( ne ), loca( n + 1 )
  INTEGER ( KIND = ip_ ), INTENT( IN ) :: helast(  n + m ), lencw, leniw, lenrw
  INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: hs( n + m ), ns, iw( leniw )
  INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: iuser( * ), ifail
  INTEGER ( KIND = ip_ ), INTENT( OUT ) :: ninf
  REAL ( KIND = rp_ ), INTENT( IN ) :: objadd
  REAL ( KIND = rp_ ), INTENT( INOUT ) :: acol( ne ), bl( n + m ), bu( n + m )
  REAL ( KIND = rp_ ), INTENT( INOUT ) :: c(max(1,lenc)), x( n + m )
  REAL ( KIND = rp_ ), INTENT( INOUT ) :: rw( lenrw ), ruser( * )
  REAL ( KIND = rp_ ), INTENT( OUT ) :: pi( m ), rc( n + m ), sinf, obj
  CHARACTER ( LEN = 1 ), INTENT ( IN ) :: start
  CHARACTER ( LEN = 8 ), INTENT ( IN ) :: prob, names( nname )
  CHARACTER ( LEN = 8 ), INTENT ( INOUT ) :: cw( lencw ), cuser( * )
  EXTERNAL :: qphx
  pi( : m ) = 0.0_rp_
  rc( : n + m ) = 0.0_rp_
  sinf = 0.0_rp_
  obj = 0.0_rp_
  ninf = 0
 ifail = 0
END SUBROUTINE E04NQF

SUBROUTINE qphx( ncolh, x, hx, nstate, cuser, iuser, ruser )
  USE CUTEST_KINDS_precision
  IMPLICIT NONE
  INTEGER ( KIND = ip_ ), INTENT( IN ) :: ncolh, nstate
  INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: iuser(*)
  REAL ( KIND = rp_ ), INTENT( IN ) :: x(ncolh)
  REAL ( KIND = rp_ ), INTENT( INOUT ) :: ruser( * )
  REAL ( KIND = rp_ ), INTENT( OUT ) :: hx(ncolh)
  CHARACTER ( LEN = 8 ), INTENT( INOUT) :: cuser(*)
END SUBROUTINE qphx

SUBROUTINE E04NPF( cw, lencw, iw, leniw, rw, lenrw, ifail )
  USE CUTEST_KINDS_precision
  IMPLICIT NONE
  INTEGER ( KIND = ip_ ), INTENT ( IN ) :: lencw, leniw, lenrw
  INTEGER ( KIND = ip_ ), INTENT ( INOUT ) :: ifail
  INTEGER ( KIND = ip_ ), INTENT ( OUT ) :: iw( leniw )
  REAL ( KIND = rp_ ), INTENT (OUT) :: rw( lenrw )
  CHARACTER ( LEN = 8 ), INTENT (OUT) :: cw( lencw )
  ifail = 0
END SUBROUTINE E04NPF

SUBROUTINE E04NRF( ispecs, cw, iw, rw, ifail )
  USE CUTEST_KINDS_precision
  IMPLICIT NONE
  INTEGER ( KIND = ip_ ), INTENT ( IN ) :: ispecs
  INTEGER ( KIND = ip_ ), INTENT ( INOUT ) :: iw( * ),  ifail
  REAL ( KIND  = rp_ ), INTENT ( INOUT ) :: rw( * )
  CHARACTER ( LEN = 8 ), INTENT ( INOUT ) :: cw( * )
  ifail = 0
END SUBROUTINE E04NRF
