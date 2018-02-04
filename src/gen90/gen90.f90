!     ( Last modified on 23 Dec 2000 at 22:01:39 )

MODULE Generic_Driver

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: GEN, GENSPC, GETINFO
  INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

Contains

  SUBROUTINE GEN( dummy )

    REAL( KIND = wp ) :: dummy

    WRITE( *, * ) ' ********************************'
    WRITE( *, * )' *                              *'
    WRITE( *, * )' *      Hello from GEN90!       *'
    WRITE( *, * )' *     (Working precision)      *'
    WRITE( *, * )' *                              *'
    WRITE( *, * )' ********************************'
    WRITE( *, * )' '
    dummy = REAL( 41.9999999999999D0, wp )
    WRITE( *, * ) ' Optimal solution found'
    WRITE( *, * ) ' The answer is ', dummy
    RETURN
  END SUBROUTINE GEN

  SUBROUTINE GENSPC( funit, fname )

! This is a dummy routine to read a spec file
! possibly, this routine contains precision-dependent directives

    INTEGER :: funit
    INTEGER, PARAMETER :: ferror = 6
    CHARACTER( LEN = 7 ) :: FNAME

    OPEN( UNIT=funit, FILE=fname, STATUS='UNKNOWN', ERR=100 )
    REWIND( funit )

!     READ COMMANDS...

    CLOSE( funit )
    RETURN

100 WRITE( FERROR, '(A,A7)' ) 'Failure while reading ', FNAME
    RETURN
  END SUBROUTINE GENSPC

  SUBROUTINE GETINFO( n, m, BL, BU, EQUATN, LINEAR, nlin, neq, nbnds )
    !
    ! Input/Output variables
    !
    INTEGER, INTENT( IN  ) :: n, m
    INTEGER, INTENT( OUT ) :: nlin, neq, nbnds
    REAL( KIND = wp ), DIMENSION( n ), INTENT( IN ) :: BL, BU
    LOGICAL, DIMENSION( m ), INTENT( IN ) :: EQUATN, LINEAR
!
!     Local variables
!
    REAL( KIND = wp ), PARAMETER :: infty = 1.0D+20
    INTEGER :: i

    nlin = 0 ; neq = 0 ; nbnds = 0

    DO i = 1, m
      IF ( EQUATN( i ) ) neq  = neq  + 1
      IF ( LINEAR( i ) ) nlin = nlin + 1
    End Do

    DO i = 1, n
      IF ( BL( i ) > - infty .OR. BU( i ) < infty ) nbnds = nbnds + 1
    END DO
  END SUBROUTINE GETINFO

END MODULE Generic_Driver
