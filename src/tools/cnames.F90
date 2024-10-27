! THIS VERSION: CUTEST 2.3 - 2024-10-27 AT 09:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-  C U T E S T  C I N T _  C N A M E S    S U B R O U T I N E  -*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 28th September 2024

      SUBROUTINE CUTEST_Cint_cnames_r( status, n, m, pname, VNAME, CNAME )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_CHAR, C_NULL_CHAR

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( KIND = C_CHAR ), INTENT( OUT ), DIMENSION( 11 ) :: pname
      CHARACTER ( KIND = C_CHAR ), INTENT( OUT ), DIMENSION( 11, n ) :: VNAME
      CHARACTER ( KIND = C_CHAR ), INTENT( OUT ), DIMENSION( 11, m ) :: CNAME

!  local variables

      INTEGER ( KIND = ip_ ) :: i, j, l
      CHARACTER ( LEN = 10 ) :: pname_fortran
      CHARACTER ( LEN = 10 ), DIMENSION( n ) :: VNAME_fortran
      CHARACTER ( LEN = 10 ), DIMENSION( m ) :: CNAME_fortran

      CALL CUTEST_cnames_r( status, n, m, pname_fortran, VNAME_fortran,        &
                            CNAME_fortran )

      l = LEN( pname_fortran )
      DO i = 1, l
        pname( i ) = pname_fortran( i : i )
      END DO
      pname( l + 1 ) = C_NULL_CHAR

      DO j = 1, n
        l = LEN( VNAME_fortran( j ) )
        DO i = 1, l
          VNAME( i, j ) = VNAME_fortran( j )( i : i )
        END DO
        VNAME( l + 1, j ) = C_NULL_CHAR
      END DO
      DO j = 1, m
        l = LEN( CNAME_fortran( j ) )
        DO i = 1, l
          CNAME( i, j ) = CNAME_fortran( j )( i : i )
        END DO
        CNAME( l + 1, j ) = C_NULL_CHAR
      END DO

!  end of subroutine CUTEST_Cint_cnames_r

      END SUBROUTINE CUTEST_Cint_cnames_r

!-*-*-*-*-*-*-  C U T E S T    C N A M E S    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28th December 2012

      SUBROUTINE CUTEST_cnames_r( status, n, m, pname, VNAME, CNAME )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( LEN = 10 ), INTENT( OUT ) :: pname
      CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( n ) :: VNAME
      CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( m ) :: CNAME

!  ----------------------------------------------------------------------
!  obtain the names of the problem, its variables and general constraints
!  ----------------------------------------------------------------------

      CALL CUTEST_cnames_threadsafe_r( CUTEST_data_global,                     &
                                       status, n, m, pname, VNAME, CNAME )
      RETURN

!  end of subroutine CUTEST_cnames_r

      END SUBROUTINE CUTEST_cnames_r

!-*-  C U T E S T   C N A M E S _ t h r e a d s a f e   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTE, September 1992
!   fortran 2003 version released in CUTEst, 19th November 2012

      SUBROUTINE CUTEST_cnames_threadsafe_r( data, status, n, m, pname,        &
                                             VNAME, CNAME )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( LEN = 10 ), INTENT( OUT ) :: pname
      CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( n ) :: VNAME
      CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( m ) :: CNAME

!  ----------------------------------------------------------------------
!  obtain the names of the problem, its variables and general constraints
!  ----------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: ig

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

!  end of subroutine CUTEST_cnames_threadsafe_r

      END SUBROUTINE CUTEST_cnames_threadsafe_r
