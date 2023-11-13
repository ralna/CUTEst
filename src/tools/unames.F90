! THIS VERSION: CUTEST 2.2 - 2023-11-12 AT 10:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

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
