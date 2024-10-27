! THIS VERSION: CUTEST 2.3 - 2024-10-27 AT 09:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-  C U T E S T  C I N T _  V A R N A M E S    S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 29th September 2024

      SUBROUTINE CUTEST_Cint_varnames_r( status, n, VNAME )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_CHAR, C_NULL_CHAR

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( KIND = C_CHAR ), INTENT( OUT ), DIMENSION( 11, n ) :: VNAME

!  local variables

      INTEGER ( KIND = ip_ ) :: i, j, l
      CHARACTER ( LEN = 10 ), DIMENSION( n ) :: VNAME_fortran

      CALL CUTEST_varnames_r( status, n, VNAME_fortran )

      DO j = 1, n
        l = LEN( VNAME_fortran( j ) )
        DO i = 1, l
          VNAME( i, j ) = VNAME_fortran( j )( i : i )
        END DO
        VNAME( l + 1, j ) = C_NULL_CHAR
      END DO

!  end of subroutine CUTEST_Cint_varnames_r

      END SUBROUTINE CUTEST_Cint_varnames_r

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
