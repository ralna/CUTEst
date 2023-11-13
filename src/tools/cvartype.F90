! THIS VERSION: CUTEST 2.2 - 2023-11-12 AT 10:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-  C U T E S T    C V A R T Y P E    S U B R O U T I N E  -*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTEr, December 1999
!   fortran 2003 version released in CUTEst, 20th November 2012

      SUBROUTINE CUTEST_cvartype_r( status, n, X_type )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( n ) :: X_type

!  --------------------------------------------------------------
!  determine the type (continuous, 0-1, integer) of each variable
!  --------------------------------------------------------------

      CALL CUTEST_cvartype_threadsafe_r( CUTEST_data_global, status, n, X_type )
      RETURN

!  end of subroutine CUTEST_cvartype_r

      END SUBROUTINE CUTEST_cvartype_r

!-  C U T E S T   C V A R T Y P E _ t h r e a d s a f e   S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTEr, December 1999
!   fortran 2003 version released in CUTEst, 20th November 2012

      SUBROUTINE CUTEST_cvartype_threadsafe_r( data, status, n, X_type )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( n ) :: X_type

!  --------------------------------------------------------------
!  determine the type (continuous, 0-1, integer) of each variable
!  --------------------------------------------------------------

!  set the type of each variable (0 = continuous, 1 = 0-1, 2 = integer)

      X_type( : n ) = data%ITYPEV( : n )
      status = 0
      RETURN

!  end of subroutine CUTEST_cvartype_threadsafe_r

      END SUBROUTINE CUTEST_cvartype_threadsafe_r
