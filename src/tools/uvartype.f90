! THIS VERSION: CUTEST 1.0 - 23/12/2012 AT 15:55 GMT.

!-*-*-*-*-*-*-  C U T E S T    U V A R T Y P E   S U B R O U T I N E  -*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 23rd December 2012

      SUBROUTINE CUTEST_uvartype( status, n, X_type )
      USE CUTEST

!  dummy arguments

      INTEGER, INTENT( IN ) :: n
      INTEGER, INTENT( OUT ) :: status
      INTEGER, INTENT( OUT ), DIMENSION( n ) :: X_type

!  --------------------------------------------------------------
!  Determine the type (continuous, 0-1, integer) of each variable
!  --------------------------------------------------------------

      CALL CUTEST_uvartype_threadsafe( CUTEST_data_global, status, n, X_type )
      RETURN

!  end of subroutine CUTEST_uvartype

      END SUBROUTINE CUTEST_uvartype

!-  C U T E S T    U V A R T Y P E _ t h r e a d s a f e  S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released as UVARTY in CUTEr, December 1999
!   fortran 2003 version released in CUTEst, 20th November 2012

      SUBROUTINE CUTEST_uvartype_threadsafe( data, status, n, X_type )
      USE CUTEST

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      INTEGER, INTENT( IN ) :: n
      INTEGER, INTENT( OUT ) :: status
      INTEGER, INTENT( OUT ), DIMENSION( n ) :: X_type

!  --------------------------------------------------------------
!  Determine the type (continuous, 0-1, integer) of each variable
!  --------------------------------------------------------------

!  set the type of each variable (0 = continuous, 1 = 0-1, 2 = integer)

      X_type( : n ) = data%ITYPEV( : n )
      status = 0
      RETURN

!  end of subroutine CUTEST_uvartype_threadsafe

      END SUBROUTINE CUTEST_uvartype_threadsafe
