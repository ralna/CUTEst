! THIS VERSION: CUTEST 1.0 - 28/12/2012 AT 17:00 GMT.

!-*-*-*-*-*-  C U T E S T    C O N N A M E S    S U B R O U T I N E  -*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28th December 2012

      SUBROUTINE CUTEST_connames( status, m, CNAME )
      USE CUTEST

!  dummy arguments

      INTEGER, INTENT( IN ) :: m
      INTEGER, INTENT( OUT ) :: status
      CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( m ) :: CNAME

!  -------------------------------------------
!  obtain the names of the general constraints
!  -------------------------------------------

      CALL CUTEST_connames_threadsafe( CUTEST_data_global, status, m, CNAME )
      RETURN

!  end of subroutine CUTEST_connames

      END SUBROUTINE CUTEST_connames

!-  C U T E S T   C O N N A M E S _ t h r e a d s a f e   S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Dominique Orban

!  History -
!   fortran 77 version originally released in CUTEr, August 2005
!   fortran 2003 version released in CUTEst, 19th November 2012

      SUBROUTINE CUTEST_connames_threadsafe( data, status, m, CNAME )
      USE CUTEST

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      INTEGER, INTENT( IN ) :: m
      INTEGER, INTENT( OUT ) :: status
      CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( m ) :: CNAME

!  -------------------------------------------
!  obtain the names of the general constraints
!  -------------------------------------------

      INTEGER :: ig

!  Set the names of the general constraints

      IF ( data%numcon > 0 ) THEN
        DO ig = 1, data%ng
          IF ( data%KNDOFC( ig ) /= 0 )                                        &
            CNAME( data%KNDOFC( ig ) ) = data%GNAMES( ig )
        END DO
      END IF
      status = 0
      RETURN

!  end of subroutine CUTEST_connames_threadsafe

      END SUBROUTINE CUTEST_connames_threadsafe
