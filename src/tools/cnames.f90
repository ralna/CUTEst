! THIS VERSION: CUTEST 1.0 - 28/12/2012 AT 16:55 GMT.

!-*-*-*-*-*-*-  C U T E S T    C N A M E S    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28th December 2012

      SUBROUTINE CUTEST_cnames( status, n, m, pname, VNAME, CNAME )
      USE CUTEST

!  dummy arguments

      INTEGER, INTENT( IN ) :: n, m
      INTEGER, INTENT( OUT ) :: status
      CHARACTER ( LEN = 10 ), INTENT( OUT ) :: pname
      CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( n ) :: VNAME
      CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( m ) :: CNAME

!  ----------------------------------------------------------------------
!  obtain the names of the problem, its variables and general constraints
!  ----------------------------------------------------------------------

      CALL CUTEST_cnames_threadsafe( CUTEST_data_global,                       &
                                     status, n, m, pname, VNAME, CNAME )
      RETURN

!  end of subroutine CUTEST_cnames

      END SUBROUTINE CUTEST_cnames

!-*-  C U T E S T   C N A M E S _ t h r e a d s a f e   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTE, September 1992
!   fortran 2003 version released in CUTEst, 19th November 2012

      SUBROUTINE CUTEST_cnames_threadsafe( data, status, n, m, pname,          &
                                           VNAME, CNAME )
      USE CUTEST

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      INTEGER, INTENT( IN ) :: n, m
      INTEGER, INTENT( OUT ) :: status
      CHARACTER ( LEN = 10 ), INTENT( OUT ) :: pname
      CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( n ) :: VNAME
      CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( m ) :: CNAME

!  ----------------------------------------------------------------------
!  obtain the names of the problem, its variables and general constraints
!  ----------------------------------------------------------------------

!  local variables

      INTEGER :: ig

!  set the problem name

      pname = data%pname

!  set the names of the variables

      VNAME( : n ) = data%VNAMES( : n )

!  set the names of the general constraints

      IF ( data%numcon > 0 ) THEN
        DO ig = 1, data%ng
          IF ( data%KNDOFC( ig ) /= 0 )                                        &
             CNAME( data%KNDOFC( ig ) ) = data%GNAMES( ig )
        END DO
      END IF
      status = 0
      RETURN

!  end of subroutine CUTEST_cnames_threadsafe

      END SUBROUTINE CUTEST_cnames_threadsafe
