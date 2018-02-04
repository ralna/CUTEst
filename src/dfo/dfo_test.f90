!     ( Last modified on 7 Jan 2013 at 15:15:00 )

!  Dummy DFO for testing dfo_main interface to CUTEst
!  Nick Gould, 7th January 2013

      SUBROUTINE DFO( n, nx, X, ldx, FX, CONX, ifiniv, m, C, nclin , ncnln,    &
                      LB, UB, A, lda, XNAMES, pname, CNAMES, it, nf, info,     &
                      maxit,  maxnf, stpcrtr, delmin, stpthr, cnstolp, delta,  &
                      pp, scale, ioutp, iprintp )
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER :: n, m, nx, ldx, nclin, ncnln, lda, it, nf, info
      INTEGER :: maxnf, maxit, stpcrtr, scale, ioutp, iprintp
      LOGICAL :: ifiniv
      CHARACTER ( LEN = 256 ) :: pname
      REAL ( KIND = wp ) :: delmin, stpthr, cnstolp, delta, pp
      REAL ( KIND = wp ) :: X( ldx * nx ), FX( nx ) , LB( * ), UB( * ) 
      REAL ( KIND = wp ) :: C( * ), CONX( * ), A( lda * n )
      CHARACTER ( LEN = 256 ) :: XNAMES( N ), CNAMES( * )

!  local variables

      LOGICAL :: iferr

      CALL FUN( n, m, X, FX( 1 ), C, iferr )
      it = 1
      nf = 1
      info = 2
      RETURN
      END SUBROUTINE DFO
