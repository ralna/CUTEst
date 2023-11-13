! THIS VERSION: CUTEST 2.2 - 2023-11-12 AT 10:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-  C U T E S T    C D I M E N    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTEr, September 1999
!   fortran 2003 version released in CUTEst, 19th November 2012

      SUBROUTINE CUTEST_cdimen_r( status, input, n, m )
      USE CUTEST_KINDS_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: input
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: n, m, status

!  -------------------------------------------------------------------------
!  Compute the basic array dimensions for the constrained optimization tools
!  -------------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: ialgor, i, iend, j, ng, ng1, nel1, nel
      INTEGER ( KIND = ip_ ), DIMENSION( 10 ) :: IARRAY
      CHARACTER ( LEN = 8 ) :: pname

!  input the problem dimensions

      REWIND( input )
      READ( input, "( 3I10 )" ) n, ng, nel

!  input the problem type

      READ( input, "( I2, A8 )" ) ialgor, pname
      IF ( ialgor < 2 ) THEN
        m = 0

!  set useful integer values

      ELSE
        ng1 = ng + 1
        nel1 = nel + 1

!  print out problem data. input the number of variables, groups, elements and 
!  the identity of the objective function group (i = nslack, j = nobjgr)

        IF ( ialgor == 2 ) READ( input, "( 2I10 )" ) i, j

!  input the starting addresses of the elements in each group, of the 
!  parameters used for each group and of the nonzeros of the linear element 
!  in each group

        READ( input, "( ( 10I8 ) )" ) ( iend, i = 1, ng1 )
        READ( input, "( ( 10I8 ) )" ) ( iend, i = 1, ng1 )
        READ( input, "( ( 10I8 ) )" ) ( iend, i = 1, ng1 )

!  Input the starting addresses of the variables and parameters in each element

        READ( input, "( ( 10I8 ) )" ) ( iend, i = 1, nel1 )
        READ( input, "( ( 10I8 ) )" ) ( iend, i = 1, nel1 )

!  input the group type of each group

        READ( input, "( ( 10I8 ) )" ) ( iend, i = 1, ng )

!  count the number of constraint groups

        m = 0
        DO i = 1, ng, 10
          iend = MIN( i + 9, ng )
          READ( input, "( ( 10I8 ) )" ) ( IARRAY( j - i + 1 ), j = i, iend )
          DO j = i, iend
            IF ( IARRAY( j - i + 1 ) /= 1 ) m = m + 1
          END DO
        END DO
      END IF

      REWIND( input )
      status = 0
      RETURN

!  End of subroutine CUTEST_cdimen_r

      END SUBROUTINE CUTEST_cdimen_r
