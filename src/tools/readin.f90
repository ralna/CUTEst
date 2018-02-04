!  ( Last modified on 21 Dec 2000 at 23:46:51 )

!-*-*-*-*-*-*-*-*-*-*-*-  GALAHAD READ_input  *-*-*-*-*-*-*-*-*-*-*-*-*-*

!  Nick Gould, for GALAHAD productions
!  Copyright reserved
!  June 13th 1995

      MODULE READ_input

!  Read and replace data as required

         IMPLICIT NONE
!S       INTEGER, PARAMETER :: wp = KIND( 1.0E+0 )
!D       INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

         INTERFACE READ_data
            MODULE PROCEDURE READ_real, READ_integer, READ_logical
         END INTERFACE

         INTERFACE OVERIDE_control
            MODULE PROCEDURE OVERIDE_real, OVERIDE_integer, READ_logical
         END INTERFACE

         REAL ( KIND = wp ), PARAMETER, PRIVATE :: zero = 0.0_wp

      CONTAINS

!  Read and replace real data as required

         SUBROUTINE READ_real( r, rdefault, unit )
         INTEGER, INTENT( IN ) :: unit
         REAL ( KIND = wp ), INTENT( IN ) :: rdefault
         REAL ( KIND = wp ), INTENT( INOUT ) :: r
         REAL ( KIND = wp ) :: rtemp
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
         INTEGER, INTENT( IN ) :: unit
         INTEGER, INTENT( IN ) :: idefault
         INTEGER, INTENT( INOUT ) :: i
         INTEGER :: itemp
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
         INTEGER, INTENT( IN ) :: unit
         LOGICAL, INTENT( INOUT ) :: l
         READ( unit, "( L10 )" ) l
         END SUBROUTINE READ_logical

!  Read and replace real data as required

         SUBROUTINE OVERIDE_real( r, unit )
         INTEGER, INTENT( IN ) :: unit
         REAL ( KIND = wp ), INTENT( INOUT ) :: r
         REAL ( KIND = wp ) :: rtemp
         READ( unit, "( ES10.3 )" ) rtemp
         IF ( rtemp >= zero ) r = rtemp
         RETURN
         END SUBROUTINE OVERIDE_real
         
!  Read and replace integer data as required

         SUBROUTINE OVERIDE_integer( i, unit )
         INTEGER, INTENT( IN ) :: unit
         INTEGER, INTENT( INOUT ) :: i
         INTEGER :: itemp
         READ( unit, "( I10 )" ) itemp
         IF ( itemp >= 0 ) i = itemp
         RETURN
         END SUBROUTINE OVERIDE_integer

!        End of module READ_input

      END MODULE READ_input



