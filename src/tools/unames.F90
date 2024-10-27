! THIS VERSION: CUTEST 2.2 - 2024-10-27 AT 09:20 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-  C U T E S T  C I N T _  U N A M E S    S U B R O U T I N E  -*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 29th September 2024

      SUBROUTINE CUTEST_Cint_unames_r( status, n, pname, VNAME )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_CHAR, C_NULL_CHAR

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( KIND = C_CHAR ), INTENT( OUT ), DIMENSION( 11 ) :: pname
      CHARACTER ( KIND = C_CHAR ), INTENT( OUT ), DIMENSION( 11, n ) :: VNAME

!  local variables

      INTEGER ( KIND = ip_ ) :: i, j, l
      CHARACTER ( LEN = 10 ) :: pname_fortran
      CHARACTER ( LEN = 10 ), DIMENSION( n ) :: VNAME_fortran

      CALL CUTEST_unames_r( status, n, pname_fortran, VNAME_fortran )

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

!  end of subroutine CUTEST_Cint_unames_r

      END SUBROUTINE CUTEST_Cint_unames_r

!-*-*-*-*-*-*-  C U T E S T    U N A M E S   S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 23rd December 2012

      SUBROUTINE CUTEST_unames_r( status, n, pname, VNAME )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  Dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( LEN = 10 ), INTENT( OUT ) :: pname
      CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( n ) :: VNAME

!  -------------------------------------------------
!  Obtain the names of the problem and its variables
!  -------------------------------------------------

      CALL CUTEST_unames_threadsafe_r( CUTEST_data_global,                     &
                                     status, n, pname, VNAME )
      RETURN

!  end of subroutine CUTEST_unames_r

      END SUBROUTINE CUTEST_unames_r

!-  C U T E S T    U N A M E S  _ t h r e a d s a f e   S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTE, September 1992
!   fortran 2003 version released in CUTEst, 19th November 2012

      SUBROUTINE CUTEST_unames_threadsafe_r( data, status, n, pname, VNAME )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  Dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( LEN = 10 ), INTENT( OUT ) :: pname
      CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( n ) :: VNAME

!  -------------------------------------------------
!  Obtain the names of the problem and its variables
!  -------------------------------------------------

!  set the problem name

      pname = data%pname

!  set the names of the variables

      VNAME( : n ) = data%VNAMES( : n )
      status = 0
      RETURN

!  end of subroutine CUTEST_unames_threadsafe_r

      END SUBROUTINE CUTEST_unames_threadsafe_r
