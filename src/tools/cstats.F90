! THIS VERSION: CUTEST 2.2 - 2023-11-12 AT 10:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-  C U T E S T   C S T A T S    S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 12th January 2013

      SUBROUTINE CUTEST_cstats_r( status, nonlinear_variables_objective,       &
                                nonlinear_variables_constraints,               &
                                equality_constraints, linear_constraints )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nonlinear_variables_objective 
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nonlinear_variables_constraints
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: equality_constraints
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: linear_constraints

!  ------------------------------------------------------------------------
!  Report the numbers of nonlinear variables used by the objective function and
!  the constraints as well as the numbers of constraints that are equations and
!  those that are linear of a problem initially written in Standard Input 
!  Format (SIF)

!  NB. CSETUP must have been called first

!  ------------------------------------------------------------------------

      CALL CUTEST_cstats_threadsafe_r( CUTEST_data_global, status,             &
                                     nonlinear_variables_objective,            &
                                     nonlinear_variables_constraints,          &
                                     equality_constraints, linear_constraints )
      RETURN

!  end of subroutine CUTEST_cstats_r

      END SUBROUTINE CUTEST_cstats_r

!-*-  C U T E S T   C S T A T S _ t h r e a d s a f e   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released in CUTEr, August 1999
!   fortran 2003 version released in CUTEst, 24th November 2012

      SUBROUTINE CUTEST_cstats_threadsafe_r( data, status,                     &
                                           nonlinear_variables_objective,      &
                                           nonlinear_variables_constraints,    &
                                           equality_constraints,               &
                                           linear_constraints )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nonlinear_variables_objective 
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: nonlinear_variables_constraints
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: equality_constraints
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: linear_constraints

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

!  end of subroutine CUTEST_cstats_threadsafe_r

      END SUBROUTINE CUTEST_cstats_threadsafe_r
