! THIS VERSION: CUTEST 2.1 - 2023-10-21 AT 13:00 GMT.

!-*-*-*-*-*-*-  C U T E S T    C D I M S G    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 17th October 2023

      SUBROUTINE CUTEST_cdimsg( status, nnzg )
      USE CUTEST

!  dummy arguments

      INTEGER, INTENT( OUT ) :: status, nnzg

!  -------------------------------------------------------------
!  compute the sparsity pattern of the gradient of the objective 
!  function initially written in Standard Input Format (SIF). 
!  -------------------------------------------------------------

      CALL CUTEST_cdimsg_threadsafe( CUTEST_data_global,                       &
                                     CUTEST_work_global( 1 ),                  &
                                     status, nnzg )
      RETURN

!  end of subroutine CUTEST_cdimsg

      END SUBROUTINE CUTEST_cdimsg

!-*-  C U T E S T   C D I M S G _ t h r e a d s a f e   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Ingrid Bongartz and Nick Gould

!  History -
!   modern fortran version released in CUTEst, 17th October 2023

      SUBROUTINE CUTEST_cdimsg_threadsafe( data, work, status, nnzg )
      USE CUTEST

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER, INTENT( OUT ) :: status, nnzg

!  -------------------------------------------------------------------
!  compute the sparsity pattern of the gradient of the objective 
!  function initially written in Standard Input Format (SIF). 
!  -------------------------------------------------------------------

!  local variables

      INTEGER :: i, j, iel, k, ig, ii, ig1, l, ll, ncalcg, neling
      INTEGER :: nin, nvarel, nelow, nelup, istrgv, iendgv, ifstat, igstat
      LOGICAL :: nontrv
      INTEGER, DIMENSION( 1 ) :: ICALCG
      EXTERNAL :: RANGE

!  Use ISWKSP to flag which variables have nonzero partial derivatives

      work%ISWKSP( : data%n ) = 0
      nnzg = 0

!  loop over the groups

      DO ig = 1, data%ng

!  consider only those groups in the objective function

        IF ( data%KNDOFC( ig ) > 0 ) CYCLE
        ig1 = ig + 1

!  the group has nonlinear elements

        IF ( data%ISTADG( ig ) <= data%ISTADG( ig1 ) - 1 ) THEN

!DIR$ IVDEP
          DO i = data%ISTAGV( ig ), data%ISTAGV( ig1 ) - 1
            ll = data%ISVGRP( i )

!  include the contributions from only the first n variables

            IF ( ll <= data%n ) THEN
              IF ( work%ISWKSP( ll ) == 0 ) THEN
                nnzg = nnzg + 1
                work%ISWKSP( ll ) = nnzg
              END IF
            END IF
          END DO

!  the group has only linear elements

        ELSE

!DIR$ IVDEP
          DO k = data%ISTADA( ig ), data%ISTADA( ig1 ) - 1
            ll = data%ICNA( k )

!  include the contributions from linear elements for only the first n
!  variables

            IF ( ll <= data%n ) THEN
              IF ( work%ISWKSP( ll ) == 0 ) THEN
                nnzg = nnzg + 1
                work%ISWKSP( ll ) = nnzg
              END IF
            END IF
          END DO
        END IF
      END DO
      work%nbprod = 0
      work%ISWKSP( : data%n ) = 0
      status = 0

      RETURN

!  end of subroutine CUTEST_cdimsg_threadsafe

      END SUBROUTINE CUTEST_cdimsg_threadsafe
