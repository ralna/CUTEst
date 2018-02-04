! THIS VERSION: CUTEST 1.0 - 12/01/2013 AT 15:00 GMT.

!-*-*-*-*-*-*-  C U T E S T   C S T A T S    S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 12th January 2013

      SUBROUTINE CUTEST_cstats( status, nonlinear_variables_objective,         &
                                nonlinear_variables_constraints,               &
                                equality_constraints, linear_constraints )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER, INTENT( OUT ) :: status, nonlinear_variables_objective 
      INTEGER, INTENT( OUT ) :: nonlinear_variables_constraints
      INTEGER, INTENT( OUT ) :: equality_constraints, linear_constraints

!  ------------------------------------------------------------------------
!  Report the numbers of nonlinear variables used by the objective function and
!  the constraints as well as the numbers of constraints that are equations and
!  those that are linear of a problem initially written in Standard Input 
!  Format (SIF)

!  NB. CSETUP must have been called first

!  ------------------------------------------------------------------------

      CALL CUTEST_cstats_threadsafe( CUTEST_data_global, status,               &
                                     nonlinear_variables_objective,            &
                                     nonlinear_variables_constraints,          &
                                     equality_constraints, linear_constraints )
      RETURN

!  end of subroutine CUTEST_cstats

      END SUBROUTINE CUTEST_cstats

!-*-  C U T E S T   C S T A T S _ t h r e a d s a f e   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTEr, August 1999
!   fortran 2003 version released in CUTEst, 24th November 2012

      SUBROUTINE CUTEST_cstats_threadsafe( data, status,                       &
                                           nonlinear_variables_objective,      &
                                           nonlinear_variables_constraints,    &
                                           equality_constraints,               &
                                           linear_constraints )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      INTEGER, INTENT( OUT ) :: status, nonlinear_variables_objective 
      INTEGER, INTENT( OUT ) :: nonlinear_variables_constraints
      INTEGER, INTENT( OUT ) :: equality_constraints, linear_constraints

!  ------------------------------------------------------------------------
!  Report the numbers of nonlinear variables used by the objective function and
!  the constraints as well as the numbers of constraints that are equations and
!  those that are linear of a problem initially written in Standard Input 
!  Format (SIF)

!  NB. csetup_threadsafe must have been called first

!  ------------------------------------------------------------------------

      nonlinear_variables_objective = data%nnov
      nonlinear_variables_constraints = data%nnjv
      equality_constraints = data%meq
      linear_constraints = data%mlin

      status = 0
      RETURN

!  end of subroutine CUTEST_cstats_threadsafe

      END SUBROUTINE CUTEST_cstats_threadsafe
