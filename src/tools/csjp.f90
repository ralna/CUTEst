! THIS VERSION: CUTEST 1.6 - 22/02/2018 AT 15:15 GMT.

!-*-*-*-*-*-*-*-  C U T E S T    C S J P  S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 22nd February 2018

      SUBROUTINE CUTEST_csjp( status, nnzj, lj, J_var, J_con )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER, INTENT( IN ) :: lj
      INTEGER, INTENT( OUT ) :: nnzj, status
      INTEGER, INTENT( OUT ), DIMENSION( lj ) :: J_var, J_con

!  ----------------------------------------------------------------------
!  compute the spasity pattern of the Jacobian matrix of gradients of
!  the general constraints of a group partially separable function.

!  The Jacobian is stored as a sparse matrix in coordinate form.
!  The i-th entry of this matrix represents the derivative of constraint
!  J_con(i) with respect to variable J_var(i) for  i = 1,...,nnzj
!  ----------------------------------------------------------------------

      CALL CUTEST_csjp_threadsafe( CUTEST_data_global,                         &
                                   CUTEST_work_global( 1 ),                    &
                                   status, nnzj, lj, J_var, J_con )
      RETURN

!  end of subroutine CUTEST_csjp

      END SUBROUTINE CUTEST_csjp


!-*-*-  C U T E S T    C S J P _ t h r e a d s a f e  S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 22nd February 2018

      SUBROUTINE CUTEST_csjp_threadsafe( data, work, status,                   &
                                         nnzj, lj, J_var, J_con )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER, INTENT( IN ) :: lj
      INTEGER, INTENT( OUT ) :: nnzj, status
      INTEGER, INTENT( OUT ), DIMENSION( lj ) :: J_var, J_con

!  ----------------------------------------------------------------------
!  compute the spasity pattern of the Jacobian matrix of gradients of
!  the general constraints of a group partially separable function.

!  The Jacobianis stored as a sparse matrix in coordinate form.
!  The i-th entry of this matrix represents the derivative of constraint
!  J_con(i) with respect to variable J_var(i) for  i = 1,...,nnzj
!  ----------------------------------------------------------------------

!  local variables

      INTEGER :: i, j, ig, ig1, icon, alloc_status
      INTEGER :: nin, nvarel, istrgv
      CHARACTER ( LEN = 80 ) :: bad_alloc = REPEAT( ' ', 80 )
      REAL :: time_in, time_out

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  determine the Jacobian pattern

      nnzj = 0

!  consider the ig-th group

      IF ( data%numcon > 0 ) THEN
        DO ig = 1, data%ng
          icon = data%KNDOFC( ig )

!  the group defines a constraint

          IF ( icon /= 0 ) THEN
            DO i = data%ISTAGV( ig ), data%ISTAGV( ig + 1 ) - 1
              nnzj = nnzj + 1
              IF ( nnzj <= lj ) THEN
                J_con( nnzj ) = icon
                J_var( nnzj ) = data%ISVGRP( i )
              END IF
            END DO
          END IF
        END DO
      END IF

!  exit if the space provided was insufficient

     IF ( nnzj > lj ) THEN
       IF ( data%out > 0 ) WRITE( data%out,                                    &
          "( /, ' ** SUBROUTINE CSJP: array length lj too small.',             &
         &    /, ' -- Increase the parameter lj to at least ', I0 )" ) nnzj
        status = 2 ; GO TO 990
      END IF

!  update the counters for the report tool

      work%nc2cg = work%nc2cg + work%pnc
      status = 0

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_csjp = work%time_csjp + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_csjp_threadsafe

      END SUBROUTINE CUTEST_csjp_threadsafe
