! THIS VERSION: CUTEST 2.3 - 2024-10-27 AT 09:20 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-  C U T E S T   C I N T _ C L A S S I F I C A T I O N   S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 27th September 2024

      SUBROUTINE CUTEST_Cint_classification_r( status, input, classification )
      USE CUTEST_KINDS_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_CHAR, C_NULL_CHAR

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: input
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( KIND = C_CHAR ), INTENT( OUT ),                             &
         DIMENSION( 31 ) :: classification

!  local variables

      INTEGER ( KIND = ip_ ) :: i, l
      CHARACTER ( LEN = 30 ) :: classification_fortran
      
      CALL CUTEST_classification_r( status, input, classification_fortran )
      l = LEN( classification_fortran )
      DO i = 1, l
        classification( i ) = classification_fortran( i : i )
      END DO
      classification( l + 1 ) = C_NULL_CHAR

!  End of subroutine CUTEST_Cint_classification_r

      END SUBROUTINE CUTEST_Cint_classification_r

!-*-  C U T E S T    C L A S S I F I C A T I O N     S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 27th September 2024

      SUBROUTINE CUTEST_classification_r( status, input, classification )
      USE CUTEST_KINDS_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: input
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( LEN = 30 ), INTENT( OUT ) :: classification

!  ---------------------------------------------------------
!  obtain the classification string for the decoded SIF file
!  ---------------------------------------------------------

!  status = 0, string is in classification
!         = 1, no SIF file matching the problem name provided in OUTSDIF.d
!         = 2, SIF file does not have a classification string
!  input is the open unit attached to the OUTSDIF.d file
!  classification is the string (if found, or blank otherwise)

!  local variables

      CHARACTER ( LEN = 10 ) :: pname
      INTEGER ( KIND = ip_ ), DIMENSION( 10 ) :: I_temp

      classification = REPEAT( ' ', 30 )
      REWIND( input )
      READ( input, "( 10I10 )" ) I_temp
      READ( input, "( I2, A10, I2, 1X, A30 )" )                                &
        I_temp( 1 ), pname, I_temp( 2 ), classification
      REWIND( input )
      status = 0
      RETURN

!  End of subroutine CUTEST_classification_r

      END SUBROUTINE CUTEST_classification_r
