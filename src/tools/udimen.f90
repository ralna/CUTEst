! THIS VERSION: CUTEST 1.0 - 19/11/2012 AT 13:30 GMT.

!-*-*-*-*-*-*-  C U T E S T    U D I M E N    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTEr, 26th August 1999
!   fortran 2003 version released in CUTEst, 19th November 2012

      SUBROUTINE CUTEST_udimen( status, input, n )

!  dummy arguments

      INTEGER, INTENT( IN ) :: input
      INTEGER, INTENT( OUT ) :: status, n

!  --------------------------------------------------------------------------
!  compute the basic array dimension for the unconstrained optimization tools
!  --------------------------------------------------------------------------

      REWIND( input )
      READ( input, "( I10 )" ) n
      REWIND( input )
      status = 0
      RETURN

!  End of subroutine CUTEST_udimen

      END SUBROUTINE CUTEST_udimen
