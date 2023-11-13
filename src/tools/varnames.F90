! THIS VERSION: CUTEST 2.2 - 2023-11-12 AT 10:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-  C U T E S T    V A R N A M E S    S U B R O U T I N E  -*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Dominique Orban

!  History -
!   fortran 77 version originally released in CUTEr, August 2005
!   fortran 2003 version released in CUTEst, 19th November 2012

      SUBROUTINE CUTEST_varnames_r( status, n, VNAME )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( n ) :: VNAME

!  -----------------------------------------
!  obtain the names of the problem variables
!  -----------------------------------------

      CALL CUTEST_varnames_threadsafe_r( CUTEST_data_global, status, n, VNAME )
      RETURN

!  end of subroutine CUTEST_varnames_r

      END SUBROUTINE CUTEST_varnames_r

!-  C U T E S T    V A R N A M E S _ t h r e a d s a f e   S U B R O U T I N E -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Dominique Orban

!  History -
!   fortran 77 version originally released in CUTEr, August 2005
!   fortran 2003 version released in CUTEst, 19th November 2012

      SUBROUTINE CUTEST_varnames_threadsafe_r( data, status, n, VNAME )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( LEN = 10 ), INTENT( OUT ), DIMENSION( n ) :: VNAME

!  -----------------------------------------
!  obtain the names of the problem variables
!  -----------------------------------------

!  set the names of the variables

      VNAME( : n ) = data%VNAMES( : n )
      status = 0
      RETURN

!  end of subroutine CUTEST_varnames_threadsafe_r

      END SUBROUTINE CUTEST_varnames_threadsafe_r
