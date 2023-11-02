! THIS VERSION: CUTEST 2.2 - 2023-11-02 AT 12:00 GMT.

!-*-*-*-*-*-*-  C U T E S T    U D I M E N    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTEr, 26th August 1999
!   fortran 2003 version released in CUTEst, 19th November 2012

      SUBROUTINE CUTEST_udimen( status, input, n )

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: input
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, n

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
