! THIS VERSION: CUTEST 2.2 - 2024-09-28 AT 13:00 GMT.

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

      INTEGER :: i, l
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
      INTEGER :: i, j, jstart, jstop, read_error, sif_unit
      LOGICAL :: sif_exists, cstart
      INTEGER, PARAMETER :: max_record_length = 80
      CHARACTER ( LEN = max_record_length ) :: nuline

      classification = REPEAT( ' ', 30 )

!  find the name of the SIF file from which OUTSDIF.d was generated

      CALL CUTEST_pname_r( status, input, pname )

!  check that the SIF file still exists

      INQUIRE( FILE = TRIM( pname ) // '.SIF', EXIST = sif_exists )
      IF ( .NOT. sif_exists ) THEN
        status = - 1

!  open the SIF file

      ELSE
        OPEN( NEWUNIT = sif_unit, FILE = TRIM( pname ) // '.SIF' )

!  read each line, one by one, until the string 'classification' is found

   rec: DO
          nuline = REPEAT( ' ', max_record_length )
          READ( sif_unit, "( A80 )", IOSTAT = read_error ) nuline

!  check that the end of file has not been reached

          IF ( read_error == 0 ) THEN

!  skip lines that are not comments

            IF ( nuline( 1 : 1 ) /= '*' ) CYCLE
            DO i = 1, 67
              IF ( nuline( i : i + 13 ) == 'classification' .OR.               &
                   nuline( i : i + 13 ) == 'CLASSIFICATION' ) THEN

!  the string has been found, now search for the classification string itself

                cstart = .FALSE. ; jstop = 80
                DO j = i + 14, 80
                  IF ( .NOT. cstart ) THEN
                    IF ( nuline( j : j ) /= ' ' ) THEN
                      jstart = j
                      cstart = .TRUE.
                    END IF
                  ELSE
                    IF ( nuline( j : j ) == ' ' ) THEN
                      jstop = j - 1
                      EXIT
                    END IF
                  END IF
                END DO

!  copy the string and exit

                classification( 1 : jstop - jstart + 1 )                       &
                  = nuline( jstart : jstop )
                status = 0
                EXIT rec
              END IF
            END DO

!  the end of file has been reached without identifying the string

          ELSE
            status = - 2
            EXIT rec
          END IF
        END DO rec
        CLOSE( sif_unit )
      END IF 
      RETURN

!  End of subroutine CUTEST_classification_r

      END SUBROUTINE CUTEST_classification_r
