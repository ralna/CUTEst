! THIS VERSION: CUTEST 2.3 - 2024-10-27 AT 08:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-*-*-  C U T E S T    C I G R _ C   S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 22nd October 2024

      SUBROUTINE CUTEST_cigr_c_r( status, n, iprob, X, GR )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, iprob
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: GR

!  -------------------------------------------------------------------
!  compute the gradient of a specified problem function (iprob < 0 is
!  the objective function, while iprob >= 0 is the 0-based iprob-th 
!  constraint) of a problem initially written in Standard Input Format (SIF).
!  The gradient is stored as a dense vector in array GR;
!  that is, GR(j) is the partial derivative of constraint iprob with
!  respect to variable j. (Subroutine CISGR performs the same
!  calculations for a sparse gradient vector.)

!  -------------------------------------------------------------------
      INTEGER ( KIND = ip_ ) :: iprob_fortran

      iprob_fortran = iprob + 1
      CALL CUTEST_cigr_threadsafe_r( CUTEST_data_global,                       &
                                     CUTEST_work_global( 1 ),                  &
                                     status, n, iprob_fortran, X, GR )
      RETURN

!  end of subroutine CUTEST_cigr_c_r

      END SUBROUTINE CUTEST_cigr_c_r

!-*-*-*-*-*-*-  C U T E S T    C I G R    S U B R O U T I N E  -*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Ingrid Bongartz and Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 12th October 2016

      SUBROUTINE CUTEST_cigr_r( status, n, iprob, X, GR )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, iprob
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: GR

!  -------------------------------------------------------------------
!  compute the gradient of a specified problem function (iprob = 0 is
!  the objective function, while iprob > 0 is the iprob-th constraint)
!  of a problem initially written in Standard Input Format (SIF).
!  The gradient is stored as a dense vector in array GR;
!  that is, GR(j) is the partial derivative of constraint iprob with
!  respect to variable j. (Subroutine CISGR performs the same
!  calculations for a sparse gradient vector.)
!  -------------------------------------------------------------------

      CALL CUTEST_cigr_threadsafe_r( CUTEST_data_global,                       &
                                     CUTEST_work_global( 1 ),                  &
                                     status, n, iprob, X, GR )
      RETURN

!  end of subroutine CUTEST_cigr_r

      END SUBROUTINE CUTEST_cigr_r

!-*-*-  C U T E S T    C I G R _ t h r e a d e d   S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Ingrid Bongartz and Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 12th October 2016

      SUBROUTINE CUTEST_cigr_threaded_r( status, n, iprob, X, GR, thread )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, iprob, thread
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: GR

!  -------------------------------------------------------------------
!  compute the gradient of a specified problem function (iprob = 0 is
!  the objective function, while iprob > 0 is the iprob-th constraint)
!  of a problem initially written in Standard Input Format (SIF).
!  The gradient is stored as a dense vector in array GR;
!  that is, GR(j) is the partial derivative of constraint iprob with
!  respect to variable j. (Subroutine CISGR performs the same
!  calculations for a sparse gradient vector.)
!  -------------------------------------------------------------------

!  check that the specified thread is within range

      IF ( thread < 1 .OR. thread > CUTEST_data_global%threads ) THEN
        IF ( CUTEST_data_global%out > 0 )                                      &
          WRITE( CUTEST_data_global%out, "( ' ** CUTEST error: thread ', I0,   &
         &  ' out of range [1,', I0, ']' )" ) thread, CUTEST_data_global%threads
        status = 4 ; RETURN
      END IF

!  evaluate using specified thread

      CALL CUTEST_cigr_threadsafe_r( CUTEST_data_global,                       &
                                     CUTEST_work_global( thread ),             &
                                     status, n, iprob, X, GR )
      RETURN

!  end of subroutine CUTEST_cigr_threaded_r

      END SUBROUTINE CUTEST_cigr_threaded_r

!-*-  C U T E S T    C I G R _ t h r e a d s a f e   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Ingrid Bongartz and Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 12th October 2016

      SUBROUTINE CUTEST_cigr_threadsafe_r( data, work, status, n, iprob, X, GR )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, iprob
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: X
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: GR

!  -------------------------------------------------------------------
!  compute the gradient of a specified problem function (iprob = 0 is
!  the objective function, while iprob > 0 is the iprob-th constraint)
!  of a problem initially written in Standard Input Format (SIF).
!  The gradient is stored as a dense vector in array GR;
!  that is, GR(j) is the partial derivative of constraint iprob with
!  respect to variable j. (Subroutine CISGR performs the same
!  calculations for a sparse gradient vector.)
!  -------------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, j, iel, k, ig, ii, ig1, l, ll
      INTEGER ( KIND = ip_ ) :: istrgv, iendgv, ifstat, igstat
      INTEGER ( KIND = ip_ ) :: nin, nvarel, nelow, nelup, ncalcg, neling
      REAL ( KIND = rp_ ) :: ftt, gi, scalee
      REAL :: time_in, time_out
      LOGICAL :: nontrv
      INTEGER ( KIND = ip_ ), DIMENSION( 1 ) :: ICALCG

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  check input parameters

      IF ( iprob < 0 ) THEN
        IF ( data%out > 0 ) WRITE( data%out, "( ' ** SUBROUTINE CIGR: ',       &
       &    'invalid constraint index iprob ' )" )
        status = 2 ; GO TO 990
      END IF

!  constraint gradient required

      IF ( iprob > 0 ) THEN

!  find group index ig of constraint iprob

        ig = 0
        DO i = 1, data%ng
          IF ( data%KNDOFC( i ) == iprob ) THEN
            ig = i
            EXIT
          END IF
        END DO
        IF ( ig == 0 ) THEN
          IF ( data%out > 0 ) WRITE( data%out, "( ' ** SUBROUTINE CIGR: ',     &
         &    'invalid constraint index iprob ' )" )
          status = 2 ; GO TO 990
        END IF

!  determine nonlinear elements in group ig. Record their indices in ICALCF

        neling = data%ISTADG( ig + 1 ) - data%ISTADG( ig )
        j = data%ISTADG( ig ) - 1
        DO i = 1, neling
          j = j + 1
          work%ICALCF( i ) = data%IELING( j )
        END DO

!  objective gradient required

      ELSE

!  identify which elements are included in objective function. Use LOGIC
!  to keep track of elements already included

        work%LOGIC( : data%nel ) = .FALSE.

!  now identify elements in objective function groups

        neling = 0
        DO ig = 1, data%ng
          IF ( data%KNDOFC( ig ) == 0 ) THEN
            DO ii = data%ISTADG( ig ), data%ISTADG( ig + 1 ) - 1
              iel = data%IELING( ii )
              IF ( .NOT. work%LOGIC( iel ) ) THEN
                work%LOGIC( iel ) = .TRUE.
                neling = neling + 1
                work%ICALCF( neling ) = iel
              END IF
            END DO
          END IF
        END DO
      END IF

!  evaluate the element functions

      CALL ELFUN_r( work%FUVALS, X, data%EPVALU, neling, data%ITYPEE,          &
                    data%ISTAEV, data%IELVAR, data%INTVAR, data%ISTADH,        &
                    data%ISTEP, work%ICALCF, data%ltypee, data%lstaev,         &
                    data%lelvar, data%lntvar, data%lstadh, data%lstep,         &
                    data%lcalcf, data%lfuval, data%lvscal, data%lepvlu,        &
                    1, ifstat )
      IF ( ifstat /= 0 ) GO TO 930

!  evaluate the element function derivatives

      CALL ELFUN_r( work%FUVALS, X, data%EPVALU, neling, data%ITYPEE,          &
                    data%ISTAEV, data%IELVAR, data%INTVAR, data%ISTADH,        &
                    data%ISTEP, work%ICALCF, data%ltypee, data%lstaev,         &
                    data%lelvar, data%lntvar, data%lstadh, data%lstep,         &
                    data%lcalcf, data%lfuval, data%lvscal, data%lepvlu,        &
                    2, ifstat )
      IF ( ifstat /= 0 ) GO TO 930

!  compute the gradient. Initialize the gradient vector as zero

      GR( : n ) = 0.0_rp_

!  constraint gradient required

      IF ( iprob > 0 ) THEN
        nelow = data%ISTADG( ig )
        nelup = data%ISTADG( ig + 1 ) - 1

!  compute the group argument value FTT. Consider only the group associated
!  with constraint iprob

        ftt = - data%B( ig )

!  include the contribution from the linear element only if the variable
!  belongs to the first n variables

        DO i = data%ISTADA( ig ), data%ISTADA( ig + 1 ) - 1
          j = data%ICNA( i )
          IF ( j <= n ) ftt = ftt + data%A( i ) * X( j )
        END DO

!  include the contributions from the nonlinear elements

        DO i = nelow, nelup
           ftt = ftt + data%ESCALE( i ) * work%FUVALS( data%IELING( i ) )
        END DO
        work%FT( ig ) = ftt

!  if ig is a trivial group, record the function value and derivative

        IF ( data%GXEQX( ig ) ) THEN
          work%GVALS( ig, 1 ) = work%FT( ig )
          work%GVALS( ig, 2 ) = 1.0_rp_

!  otherwise, evaluate group ig

        ELSE
          ICALCG( 1 ) = ig
          CALL GROUP_r( work%GVALS, data%ng, work%FT, data%GPVALU, 1_ip_,      &
                        data%ITYPEG, data%ISTGP, ICALCG, data%ltypeg,          &
                        data%lstgp, 1_ip_, data%lcalcg, data%lgpvlu,           &
                        .FALSE., igstat )
          IF ( igstat /= 0 ) GO TO 930

!  evaluate the group derivative

          CALL GROUP_r( work%GVALS, data%ng, work%FT, data%GPVALU, 1_ip_,      &
                        data%ITYPEG, data%ISTGP, ICALCG, data%ltypeg,          &
                        data%lstgp, 1_ip_, data%lcalcg, data%lgpvlu,           &
                        .TRUE., igstat )
          IF ( igstat /= 0 ) GO TO 930
        END IF

!  consider only group ig

        ig1 = ig + 1
        istrgv = data%ISTAGV( ig )
        iendgv = data%ISTAGV( ig1 ) - 1
        nontrv = .NOT. data%GXEQX( ig )

!  compute the first derivative of the group

        gi = data%GSCALE( ig )
        IF ( nontrv ) gi = gi  * work%GVALS( ig, 2 )

!  the group has nonlinear elements

        IF ( nelow <= nelup ) THEN
          work%W_ws( data%ISVGRP( istrgv : iendgv ) ) = 0.0_rp_

!  loop over the group's nonlinear elements

          DO ii = nelow, nelup
            iel = data%IELING( ii )
            k = data%INTVAR( iel )
            l = data%ISTAEV( iel )
            nvarel = data%ISTAEV( iel + 1 ) - l
            scalee = data%ESCALE( ii )

!  the iel-th element has an internal representation

            IF ( data%INTREP( iel ) ) THEN
              nin = data%INTVAR( iel + 1 ) - k
              CALL RANGE_r( iel, .TRUE., work%FUVALS( k ), work%W_el,          &
                            nvarel, nin, data%ITYPEE( iel ), nin, nvarel )
!DIR$ IVDEP
              DO i = 1, nvarel
                j = data%IELVAR( l )
                work%W_ws( j ) = work%W_ws( j ) + scalee * work%W_el( i )
                l = l + 1
              END DO

!  the iel-th element has no internal representation

            ELSE
!DIR$ IVDEP
              DO i = 1, nvarel
                j = data%IELVAR( l )
                work%W_ws( j ) = work%W_ws( j ) + scalee * work%FUVALS( k )
                k = k + 1 ; l = l + 1
              END DO
            END IF
          END DO

!  include the contribution from the linear element

!DIR$ IVDEP
          DO k = data%ISTADA( ig ), data%ISTADA( ig1 ) - 1
            j = data%ICNA( k )
            work%W_ws( j ) = work%W_ws( j ) + data%A( k )
          END DO

!  allocate a gradient

!DIR$ IVDEP
          DO i = istrgv, iendgv
            ll = data%ISVGRP( i )

!  include contributions from the first n variables only

            IF ( ll <= n ) GR( ll ) = gi * work%W_ws( ll )
          END DO

!  the group has only linear elements

        ELSE

!  allocate a gradient

!DIR$ IVDEP
          DO k = data%ISTADA( ig ), data%ISTADA( ig1 ) - 1
            ll = data%ICNA( k )

!  include contributions from the first n variables only

            IF ( ll <= n ) GR( ll ) = gi * data%A( k )
          END DO
        END IF

!  update the constraint gradient evaluation counter

        work%nc2cg = work%nc2cg + 1

!  objective gradient required

      ELSE

!  compute the list of groups involved in the required problem function

        ncalcg = 0
        DO ig = 1, data%ng
          IF ( data%KNDOFC( ig ) == 0 ) THEN
            ncalcg = ncalcg + 1
            work%ICALCF( ncalcg ) = ig

!  compute the group argument values ft

            ftt = - data%B( ig )

!  include the contribution from the linear element

            DO j = data%ISTADA( ig ), data%ISTADA( ig + 1 ) - 1
               ftt = ftt + data%A( j ) * X( data%ICNA( j ) )
            END DO

!  include the contributions from the nonlinear elements

            DO j = data%ISTADG( ig ), data%ISTADG( ig + 1 ) - 1
               ftt = ftt + data%ESCALE( j ) *  &
                      work%FUVALS( data%IELING( j ) )
            END DO
            work%FT( ig ) = ftt

!  record the derivatives of trivial groups

            IF ( data%GXEQX( ig ) ) work%GVALS( ig, 2 ) = 1.0_rp_

!  if this is the first ever evaluation, initialize GVALS

          ELSE
            IF ( work%firstg ) work%GVALS( ig, 2 ) = 1.0_rp_
          END IF
        END DO

!  compute the group function values

!  all group functions are trivial

        IF ( data%altriv ) THEN
          work%GVALS( : data%ng, 1 ) = work%FT( : data%ng )
          work%GVALS( : data%ng, 2 ) = 1.0_rp_

!  evaluate the group function values

        ELSE
          CALL GROUP_r( work%GVALS, data%ng, work%FT, data%GPVALU, ncalcg,     &
                        data%ITYPEG, data%ISTGP, work%ICALCF, data%ltypeg,     &
                        data%lstgp, data%lcalcf, data%lcalcg, data%lgpvlu,     &
                        .FALSE., igstat )
          IF ( igstat /= 0 ) GO TO 930

!  evaluate the group derivative values.

          CALL GROUP_r( work%GVALS, data%ng, work%FT, data%GPVALU, ncalcg,     &
                        data%ITYPEG, data%ISTGP, work%ICALCF, data%ltypeg,     &
                        data%lstgp, data%lcalcf, data%lcalcg, data%lgpvlu,     &
                        .TRUE., igstat )
          IF ( igstat /= 0 ) GO TO 930
        END IF

        DO ig = 1, data%ng

!  consider only those groups in the objective function

          IF ( data%KNDOFC( ig ) > 0 ) CYCLE
          ig1 = ig + 1
          istrgv = data%ISTAGV( ig )
          iendgv = data%ISTAGV( ig1 ) - 1
          nelow = data%ISTADG( ig )
          nelup = data%ISTADG( ig1 ) - 1

!  compute the first derivative of the group

          gi = data%GSCALE( ig )
          IF ( .NOT. data%GXEQX( ig ) ) gi = gi  * work%GVALS( ig, 2 )

!  the group has nonlinear elements

          IF ( nelow <= nelup ) THEN
            work%W_ws( data%ISVGRP( istrgv : iendgv ) ) = 0.0_rp_

!  loop over the group's nonlinear elements

            DO ii = nelow, nelup
              iel = data%IELING( ii )
              k = data%INTVAR( iel )
              l = data%ISTAEV( iel )
              nvarel = data%ISTAEV( iel + 1 ) - l
              scalee = data%ESCALE( ii )
              IF ( data%INTREP( iel ) ) THEN

!  the iel-th element has an internal representation

                nin = data%INTVAR( iel + 1 ) - k
                CALL RANGE_r( iel, .TRUE., work%FUVALS( k ), work%W_el,        &
                              nvarel, nin, data%ITYPEE( iel ), nin, nvarel )
!DIR$ IVDEP
                DO i = 1, nvarel
                  j = data%IELVAR( l )
                  work%W_ws( j ) = work%W_ws( j ) + scalee * work%W_el( i )
                  l = l + 1
                END DO
              ELSE

!  the iel-th element has no internal representation

!DIR$ IVDEP
                DO i = 1, nvarel
                  j = data%IELVAR( l )
                  work%W_ws( j ) = work%W_ws( j ) + scalee * work%FUVALS( k )
                   k = k + 1 ; l = l + 1
                END DO
              END IF
            END DO

!  include the contribution from the linear element

!DIR$ IVDEP
            DO k = data%ISTADA( ig ), data%ISTADA( ig1 ) - 1
             j = data%ICNA( k )
             work%W_ws( j ) = work%W_ws( j ) + data%A( k )
            END DO

!  allocate a gradient

!DIR$ IVDEP
            DO i = istrgv, iendgv
              ll = data%ISVGRP( i )

!  include the contributions from only the first n variables

              IF ( ll <= n ) GR( ll ) = GR( ll ) + gi * work%W_ws( ll )
            END DO

!  the group has only linear elements

          ELSE

!  allocate a gradient

!DIR$ IVDEP
            DO k = data%ISTADA( ig ), data%ISTADA( ig1 ) - 1
              ll = data%ICNA( k )

!  include the contributions from linear elements for only the first n
!  variables

              IF ( ll <= n ) GR( ll ) = GR( ll ) + gi * data%A( k )
            END DO
          END IF
        END DO

!  update the objective gradient evaluation counter

        work%nc2og = work%nc2og + 1
      END IF
      status = 0
      GO TO 990

!  unsuccessful returns

  930 CONTINUE
      IF ( data%out > 0 ) WRITE( data%out,                                     &
        "( ' ** SUBROUTINE CIGR: error flag raised during SIF evaluation' )" )
      status = 3

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_cigr = work%time_cigr + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_cigr_threadsafe_r

      END SUBROUTINE CUTEST_cigr_threadsafe_r
