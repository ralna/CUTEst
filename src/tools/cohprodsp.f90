! THIS VERSION: CUTEST 2.1 - 2023-10-29 AT 14:40 GMT.

!-*-*-*-*-*-  C U T E S T    C O H P R O D S P   S U B R O U T I N E  -*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   moden fortran version released in CUTEst, 29th October 2023

      SUBROUTINE CUTEST_cohprodsp( status, nnzohp, lohp, IND )
      USE CUTEST

!  dummy arguments

      INTEGER, INTENT( IN ) :: lohp
      INTEGER, INTENT( OUT ) :: status, nnzohp
      INTEGER, INTENT( OUT ), DIMENSION( lohp ) :: IND

!  ----------------------------------------------------------------------------
!  determine the integer storage array needed when computing the matrix-vector
!  product H(x) v between the Hessian matrix of the objective function and a
!  given vector v. The indices of the nonzero entries of the resulting 
!  product H(x) v occur in IND(k), k = 1,...,nnzohp
!  ----------------------------------------------------------------------------

      CALL CUTEST_cohprodsp_threadsafe( CUTEST_data_global,                    &
                                        CUTEST_work_global( 1 ),               &
                                        status, nnzohp, lohp, IND )
      RETURN

!  end of subroutine CUTEST_cohprodsp

      END SUBROUTINE CUTEST_cohprodsp

!-  C U T E S T  C O H P R O D S P _ t h r e a d s a f e  S U B R O U T I N E  -

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal authors: Nick Gould

!  History -
!   modern fortran version released in CUTEst, 29th October 2023

      SUBROUTINE CUTEST_cohprodsp_threadsafe( data, work, status,              &
                                              nnzohp, lohp, IND )
      USE CUTEST

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER, INTENT( IN ) :: lohp
      INTEGER, INTENT( OUT ) :: status, nnzohp
      INTEGER, INTENT( OUT ), DIMENSION( lohp ) :: IND

!  ----------------------------------------------------------------------------
!  determine the integer storage array needed when computing the matrix-vector
!  product H(x) v between the Hessian matrix of the objective function and a
!  given vector v. The indices of the nonzero entries of the resulting 
!  product H(x) v occur in IND(k), k = 1,...,nnzohp
!  ----------------------------------------------------------------------------

!  local variables

      INTEGER :: ii, iel, iell, ig, j, k, l, ll
      REAL :: time_in, time_out

      IF ( work%record_times ) CALL CPU_TIME( time_in )

      nnzohp = 0

!  loop over all the groups, but ignore those that are constraints

      DO ig = 1, data%ng
        IF ( data%KNDOFC( ig ) == 0 ) THEN

!  store the nonzeros of the Hessian-vector product in W_ws

!  =========================== rank-one terms ============================

!  if the ig-th group is non-trivial, the indices of its rank-one term
!  grad h_ig * g''(h_ig) * grad(trans) h_ig, occur in the sparsity pattern
!  of H * v

          IF ( .NOT. data%GXEQX( ig ) ) THEN
            DO l = data%ISTAGV( ig ), data%ISTAGV( ig + 1 ) - 1
              j = data%ISVGRP( l )
              IF ( work%IUSED( j ) == 0 ) THEN
                work%IUSED( j ) = 1
                nnzohp = nnzohp + 1
                IND( nnzohp ) = j
              END IF
            END DO

!  ======================= second-order terms =======================

!  otherwise the indices of the second order term g'(h_ig) * Hess h_ig
!  (which is a subset of those for the rank-one term) occur in the 
!  sparsity pattern of H * v

          ELSE

!  consider all nonlinear elements for the group

            DO iell = data%ISTADG( ig ), data%ISTADG( ig + 1 ) - 1
              iel = data%IELING( iell )
               DO l = data%ISTAEV( iel ), data%ISTAEV( iel + 1 ) - 1
                 j = data%IELVAR( l )
                 IF ( work%IUSED( j ) == 0 ) THEN
                   work%IUSED( j ) = 1
                   nnzohp = nnzohp + 1
                   IND( nnzohp ) = j
                 END IF
               END DO
            END DO
          END IF
        END IF
      END DO

!  reset IUSED to zero

      work%IUSED( IND( : nnzohp ) ) = 0

!  update elapsed CPU time if required

      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_cohprodsp = work%time_cohprodsp + time_out - time_in
      END IF
      status = 0

      RETURN

!  end of subroutine CUTEST_cohprodsp_threadsafe

      END SUBROUTINE CUTEST_cohprodsp_threadsafe


