!     ( Last modified on 12 Mar 2021 at 15:50:00 )

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
  INTEGER, PARAMETER :: wp = KIND( 1.0D+0 ) 
  INTEGER, INTENT( IN ) :: m, n, ne, nname, lenc, ncolh, iobj, inda( ne ),    &
                           loca( n + 1 ), helast(  n + m ), lencw, leniw, lenrw
  INTEGER, INTENT( INOUT ) :: hs(n+m), ns, iw(leniw), iuser(*), ifail
  INTEGER, INTENT( OUT ) :: ninf
  REAL ( KIND = wp ), INTENT( IN ) :: objadd
  REAL ( KIND = wp ), INTENT( INOUT ) :: acol( ne ), bl( n + m ), bu( n + m ), &
                                         c(max(1,lenc)), x( n + m ),           &
                                         rw( lenrw ), ruser( * )
  REAL ( KIND = wp ), INTENT( OUT ) :: pi( m ), rc( n + m ), sinf, obj
  CHARACTER (1), INTENT ( IN ) :: start
  CHARACTER (8), INTENT ( IN ) :: prob, names( nname )
  CHARACTER (8), INTENT ( INOUT ) :: cw( lencw ), cuser( * )
  EXTERNAL :: qphx
END SUBROUTINE E04NQF

SUBROUTINE qphx( ncolh, x, hx, nstate, cuser, iuser, ruser )
  INTEGER, PARAMETER :: wp = KIND( 1.0D+0 ) 
  INTEGER, INTENT( IN ) :: ncolh, nstate
  INTEGER, INTENT( INOUT ) :: iuser(*)
  REAL ( KIND = wp ), INTENT( IN ) :: x(ncolh)
  REAL ( KIND = wp ), INTENT( INOUT ) :: ruser( * )
  REAL ( KIND = wp ), INTENT( OUT ) :: hx(ncolh)
  CHARACTER (8), INTENT( INOUT) :: cuser(*)
END SUBROUTINE qphx
