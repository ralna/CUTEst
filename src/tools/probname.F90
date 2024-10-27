! THIS VERSION: CUTEST 2.3 - 2024-10-27 AT 09:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-  C U T E S T  C I N T _  P R O B N A M E    S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 29th September 2024

      SUBROUTINE CUTEST_Cint_probname_r( status, pname )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_CHAR, C_NULL_CHAR

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( KIND = C_CHAR ), INTENT( OUT ), DIMENSION( 11 ) :: pname

!  local variables

      INTEGER ( KIND = ip_ ) :: i, l
      CHARACTER ( LEN = 10 ) :: pname_fortran

      CALL CUTEST_probname_r( status, pname_fortran )

      l = LEN( pname_fortran )
      DO i = 1, l
        pname( i ) = pname_fortran( i : i )
      END DO
      pname( l + 1 ) = C_NULL_CHAR

!  end of subroutine CUTEST_Cint_probname_r

     END SUBROUTINE CUTEST_Cint_probname_r

!-*-*-*-*-*-  C U T E S T    P R O B N A M E     S U B R O U T I N E  -*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Dominique Orban

!  History -
!   fortran 2003 version released in CUTEst, 23rd Decemeber 2012

      SUBROUTINE CUTEST_probname_r( status, pname )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  Dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( LEN = 10 ), INTENT( OUT ) :: pname

!  ------------------------------
!  obtain the name of the problem
!  ------------------------------

      CALL CUTEST_probname_threadsafe_r( CUTEST_data_global, status, pname )
      RETURN

!  end of subroutine CUTEST_probname_r

      END SUBROUTINE CUTEST_probname_r

!-  C U T E S T    P R O B N A M E _ t h r e a d s a f e   S U B R O U T I N E -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Dominique Orban

!  History -
!   fortran 77 version originally released as PBNAME in CUTEr, August 2005
!   fortran 2003 version released in CUTEst, 19th November 2012

      SUBROUTINE CUTEST_probname_threadsafe_r( data, status, pname )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  Dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( LEN = 10 ), INTENT( OUT ) :: pname

!  ------------------------------
!  obtain the name of the problem
!  ------------------------------

      pname = data%pname
      status = 0
      RETURN

!  end of subroutine CUTEST_probname_threadsafe_r

      END SUBROUTINE CUTEST_probname_threadsafe_r
