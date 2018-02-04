! THIS VERSION: CUTEST 1.1 - 03/04/2014 AT 07:50 GMT.

!-*-*-*-*-*-*-  C U T E S T    P N A M E    S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 3rd April 2014

      SUBROUTINE CUTEST_pname( status, input, pname )

!  dummy arguments

      INTEGER, INTENT( IN ) :: input
      INTEGER, INTENT( OUT ) :: status
      CHARACTER ( LEN = 10 ), INTENT( OUT ) :: pname

!  -------------------------------------------------------------
!  obtain the name of the problem directly from the OUTSDIF file
!  -------------------------------------------------------------

      INTEGER, DIMENSION( 10 ) :: I_temp
      pname = REPEAT( ' ', 10 )
      REWIND( input )
      READ( input, "( 10I10 )" ) I_temp
      READ( input, "( I2, A10 )" ) I_temp( 1 ), pname
      REWIND( input )
      status = 0
      RETURN

!  End of subroutine CUTEST_pname

      END SUBROUTINE CUTEST_pname
