! THIS VERSION: CUTEST 1.5 - 29/03/2017 AT 14:15 GMT.

!-*-*-*-*-*-*-*-  C U T E S T    C S G R P  S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 29th March 2017

      SUBROUTINE CUTEST_csgrp( status, n, nnzj, lj, J_var, J_fun )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      INTEGER, INTENT( IN ) :: n, lj
      INTEGER, INTENT( OUT ) :: nnzj, status
      INTEGER, INTENT( OUT ), DIMENSION( lj ) :: J_var, J_fun

!  ----------------------------------------------------------------------
!  compute the spasity pattern of the gradients of the objective function
!  and general constraints of a group partially separable function.

!  The gradients are stored as a sparse matrix in coordinate form.
!  The i-th entry of this matrix represents the derivative of
!  function J_fun(i) with respect to variable J_var(i) for
!  i = 1,...,nnzj, where function 0 is the objective function,
!  and function j>0 is the jth constraint
!  ----------------------------------------------------------------------

      CALL CUTEST_csgrp_threadsafe( CUTEST_data_global,                        &
                                    CUTEST_work_global( 1 ),                   &
                                    status, n, nnzj, lj, J_var, J_fun )
      RETURN

!  end of subroutine CUTEST_csgrp

      END SUBROUTINE CUTEST_csgrp


!-*-*-  C U T E S T    C S G R P _ t h r e a d s a f e  S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 29th March 2017

      SUBROUTINE CUTEST_csgrp_threadsafe( data, work, status, n,              &
                                          nnzj, lj, J_var, J_fun )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER, INTENT( IN ) :: n, lj
      INTEGER, INTENT( OUT ) :: nnzj, status
      INTEGER, INTENT( OUT ), DIMENSION( lj ) :: J_var, J_fun

!  ----------------------------------------------------------------------
!  compute the spasity pattern of the gradients of the objective function
!  and general constraints of a group partially separable function.

!  The gradients are stored as a sparse matrix in coordinate form.
!  The i-th entry of this matrix represents the derivative of
!  function J_fun(i) with respect to variable J_var(i) for
!  i = 1,...,nnzj, where function 0 is the objective function,
!  and function j>0 is the jth constraint
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
                J_fun( nnzj ) = icon
                J_var( nnzj ) = data%ISVGRP( i )
              END IF
            END DO
          END IF
        END DO
      END IF

!  transfer the gradient of the objective function to the sparse storage scheme

      DO i = 1, n
        nnzj = nnzj + 1
        IF ( nnzj <= lj ) THEN
          J_fun( nnzj ) = 0
          J_var( nnzj ) = i
        END IF
      END DO

!  exit if the space provided was insufficient

     IF ( nnzj > lj ) THEN
       IF ( data%out > 0 ) WRITE( data%out,                                    &
          "( /, ' ** SUBROUTINE CSGRP: array length lj too small.',            &
         &    /, ' -- Increase the parameter lj to at least ', I0 )" ) nnzj
        status = 2 ; GO TO 990
      END IF

      work%firstg = .FALSE.

!  update the counters for the report tool

      work%nc2og = work%nc2og + 1
      work%nc2cg = work%nc2cg + work%pnc
      status = 0

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_csgrp = work%time_csgrp + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_csgrp_threadsafe

      END SUBROUTINE CUTEST_csgrp_threadsafe
