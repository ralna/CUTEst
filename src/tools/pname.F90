! THIS VERSION: CUTEST 2.3 - 2024-10-27 AT 09:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-  C U T E S T  C I N T _  P N A M E    S U B R O U T I N E  -*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 29th September 2024

      SUBROUTINE CUTEST_Cint_pname_r( status, input, pname )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_CHAR, C_NULL_CHAR

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: input
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( KIND = C_CHAR ), INTENT( OUT ), DIMENSION( 11 ) :: pname

!  local variables

      INTEGER ( KIND = ip_ ) :: i, l
      CHARACTER ( LEN = 10 ) :: pname_fortran

      CALL CUTEST_pname_r( status, input, pname_fortran )

      l = LEN( pname_fortran )
      DO i = 1, l
        pname( i ) = pname_fortran( i : i )
      END DO
      pname( l + 1 ) = C_NULL_CHAR

!  end of subroutine CUTEST_Cint_pname_r

      END SUBROUTINE CUTEST_Cint_pname_r

!-*-*-*-*-*-*-  C U T E S T    P N A M E    S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 3rd April 2014

      SUBROUTINE CUTEST_pname_r( status, input, pname )

      USE CUTEST_KINDS_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: input
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( LEN = 10 ), INTENT( OUT ) :: pname

!  -------------------------------------------------------------
!  obtain the name of the problem directly from the OUTSDIF file
!  -------------------------------------------------------------

      INTEGER ( KIND = ip_ ), DIMENSION( 10 ) :: I_temp
      pname = REPEAT( ' ', 10 )
      REWIND( input )
      READ( input, "( 10I10 )" ) I_temp
      READ( input, "( I2, A10 )" ) I_temp( 1 ), pname
      REWIND( input )
      status = 0
      RETURN

!  End of subroutine CUTEST_pname_r

      END SUBROUTINE CUTEST_pname_r
