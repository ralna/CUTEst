! THIS VERSION: CUTEST 2.2 - 2023-11-23 AT 11:45 GMT.

#include "cutest_modules.h"

C     ( Last modified on 23 Dec 2000 at 22:01:38 )
      SUBROUTINE GEN( DUMMY )

      USE CUTEST_KINDS_precision
C
C   THIS IS THE DOUBLE PRECISION VERSION OF THE GENERIC PACKAGE
C
      REAL ( KIND = rp_ ) DUMMY
      WRITE(*,*)' ********************************'
      WRITE(*,*)' *                              *'
      WRITE(*,*)' *       HELLO FROM GEN!        *'
      WRITE(*,*)' *     (DOUBLE PRECISION)       *'
      WRITE(*,*)' *                              *'
      WRITE(*,*)' ********************************'
      WRITE(*,*)' '
      DUMMY = REAL( 41.9999999999999D0, KIND = rp_ )
      WRITE( *, * ) ' OPTIMAL SOLUTION FOUND'
      WRITE( *, * ) ' THE ANSWER IS ', DUMMY
      RETURN
      END

      SUBROUTINE GENSPC( FUNIT, FNAME )

      USE CUTEST_KINDS_precision

C     THIS IS A DUMMY ROUTINE TO READ A SPEC FILE
C     POSSIBLY, THIS ROUTINE CONTAINS PRECISION-DEPENDENT DIRECTIVES

      INTEGER ( KIND = ip_ )     FUNIT, FERROR
      PARAMETER( FERROR = 6 )
      CHARACTER*7 FNAME

      OPEN( UNIT=FUNIT, FILE=FNAME, STATUS='UNKNOWN', ERR=100 )
      REWIND( FUNIT )

C     READ COMMANDS...

      CLOSE( FUNIT )
      RETURN

 100  WRITE( FERROR, '(A,A7)' ) 'Failure while reading ', FNAME
      RETURN

      END

      SUBROUTINE GETINFO(N, M, BL, BU, EQUATN, LINEAR, NLIN, NEQ, NBNDS)

      USE CUTEST_KINDS_precision

C
C     Input/Output variables
C
      INTEGER ( KIND = ip_ ) N, M, NLIN, NEQ, NBNDS
      REAL ( KIND = rp_ ) BL( N ), BU( N )
      REAL ( KIND = rp_ ) INFTY
      PARAMETER  ( INFTY = REAL( 1.0D+20, KIND = rp_ ) )
      LOGICAL EQUATN( M ), LINEAR( M )
C
C     Local variables
C
      INTEGER ( KIND = ip_ ) I

      NLIN  = 0
      NEQ   = 0
      NBNDS = 0

      DO 200 I = 1, M
         IF( EQUATN( I ) ) NEQ  = NEQ  + 1
         IF( LINEAR( I ) ) NLIN = NLIN + 1
 200  CONTINUE

      DO 300 I = 1, N
         IF( BL( I ) .GT. -INFTY .OR. BU( I ) .LT. INFTY ) 
     *       NBNDS = NBNDS + 1
 300  CONTINUE

      END
