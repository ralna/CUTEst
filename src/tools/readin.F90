! THIS VERSION: CUTEST 2.2 - 2023-11-12 AT 10:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-*-*-*-*-*-  GALAHAD READ_input  *-*-*-*-*-*-*-*-*-*-*-*-*-*

!  Nick Gould, for GALAHAD productions
!  Copyright reserved
!  June 13th 1995

      MODULE READ_input

      USE CUTEST_KINDS_precision

!  Read and replace data as required

         IMPLICIT NONE

         INTERFACE READ_data
            MODULE PROCEDURE READ_real, READ_integer, READ_logical
         END INTERFACE

         INTERFACE OVERIDE_control
            MODULE PROCEDURE OVERIDE_real, OVERIDE_integer, READ_logical
         END INTERFACE

         REAL ( KIND = rp_ ), PARAMETER, PRIVATE :: zero = 0.0_rp_

      CONTAINS

!  Read and replace real data as required

         SUBROUTINE READ_real( r, rdefault, unit )
         INTEGER ( KIND = ip_ ), INTENT( IN ) :: unit
         REAL ( KIND = rp_ ), INTENT( IN ) :: rdefault
         REAL ( KIND = rp_ ), INTENT( INOUT ) :: r
         REAL ( KIND = rp_ ) :: rtemp
         READ( unit, 1000 ) rtemp
         IF ( rtemp >= zero ) THEN
            r = rtemp
         ELSE
            r = rdefault
         END IF
         RETURN
 1000    FORMAT( ES10.3 )
         END SUBROUTINE READ_real
         
!  Read and replace integer data as required

         SUBROUTINE READ_integer( i, idefault, unit )
         INTEGER ( KIND = ip_ ), INTENT( IN ) :: unit
         INTEGER ( KIND = ip_ ), INTENT( IN ) :: idefault
         INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: i
         INTEGER ( KIND = ip_ ) :: itemp
         READ( unit, "( I10 )" ) itemp
         IF ( itemp >= 0 ) THEN
            i = itemp
         ELSE
            i = idefault
         END IF
         RETURN
         END SUBROUTINE READ_integer

!  Read logical data as required

         SUBROUTINE READ_logical( l, unit )
         INTEGER ( KIND = ip_ ), INTENT( IN ) :: unit
         LOGICAL, INTENT( INOUT ) :: l
         READ( unit, "( L10 )" ) l
         END SUBROUTINE READ_logical

!  Read and replace real data as required

         SUBROUTINE OVERIDE_real( r, unit )
         INTEGER ( KIND = ip_ ), INTENT( IN ) :: unit
         REAL ( KIND = rp_ ), INTENT( INOUT ) :: r
         REAL ( KIND = rp_ ) :: rtemp
         READ( unit, "( ES10.3 )" ) rtemp
         IF ( rtemp >= zero ) r = rtemp
         RETURN
         END SUBROUTINE OVERIDE_real
         
!  Read and replace integer data as required

         SUBROUTINE OVERIDE_integer( i, unit )
         INTEGER ( KIND = ip_ ), INTENT( IN ) :: unit
         INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: i
         INTEGER ( KIND = ip_ ) :: itemp
         READ( unit, "( I10 )" ) itemp
         IF ( itemp >= 0 ) i = itemp
         RETURN
         END SUBROUTINE OVERIDE_integer

!        End of module READ_input

      END MODULE READ_input



