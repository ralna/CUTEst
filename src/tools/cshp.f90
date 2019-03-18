! THIS VERSION: CUTEST 1.1 - 14/06/2013 AT 14:00 GMT.

!-*-*-*-*-*-*-*-  C U T E S T    C S H P  S U B R O U T I N E  -*-*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 8th April 2013

      SUBROUTINE CUTEST_cshp( status, n, nnzh, lh, H_row, H_col )
      USE CUTEST

!  dummy arguments

      INTEGER, INTENT( IN ) :: n, lh
      INTEGER, INTENT( OUT ) :: nnzh, status
      INTEGER, INTENT( OUT ), DIMENSION( lh ) :: H_row, H_col

!  ---------------------------------------------------------------
!  compute the spasity pattern of the Hessian matrix of a group
!  partially separable function. The upper triangle of the Hessian
!  is stored in coordinate form, i.e., the entry has row index
!  H_row(i) and column index H_col(i) for i = 1, ...., nnzh
!  ---------------------------------------------------------------

      CALL CUTEST_cshp_threadsafe( CUTEST_data_global,                         &
                                   CUTEST_work_global( 1 ), status, n,         &
                                   nnzh, lh, H_row, H_col )
      RETURN

!  end of subroutine CUTEST_cshp

      END SUBROUTINE CUTEST_cshp

!-*-*-  C U T E S T   C S H P _ t h r e a d s a f e   S U B R O U T I N E  -*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 8th April 2013

      SUBROUTINE CUTEST_cshp_threadsafe( data, work, status, n,                &
                                         nnzh, lh, H_row, H_col )
      USE CUTEST
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER, INTENT( IN ) :: n, lh
      INTEGER, INTENT( OUT ) :: nnzh, status
      INTEGER, INTENT( OUT ), DIMENSION( lh ) :: H_row, H_col

!  ---------------------------------------------------------------
!  compute the spasity pattern of the Hessian matrix of a group
!  partially separable function. The upper triangle of the Hessian
!  is stored in coordinate form, i.e., the entry has row index
!  H_row(i) and column index H_col(i) for i = 1, ...., nnzh
!  ---------------------------------------------------------------

!  local variables

      INTEGER :: alloc_status
      CHARACTER ( LEN = 80 ) :: bad_alloc = REPEAT( ' ', 80 )
      REAL :: time_in, time_out

      IF ( work%record_times ) CALL CPU_TIME( time_in )

!  determine the Hessian pattern

      CALL CUTEST_assemble_hessian_pattern(                                    &
             n, data%ng, data%nel, data%ntotel, data%nvrels, data%nvargp,      &
             data%IELVAR, data%IELING, data%ISTADG,                            &
             data%ISTAEV, data%ISTAGV, data%ISVGRP, data%GXEQX,                &
             0, data%out, data%out, status, alloc_status, bad_alloc,           &
             work%hessian_setup_complete, work%lh_row, work%lh_col,            &
             work%H_row, work%H_col, work%ROW_start, work%POS_in_H, work%USED, &
             work%FILLED, work%lrowst, work%lpos, work%lused, work%lfilled,    &
             nnzh )

!  record the pattern if it is available

      IF ( status == 0 ) THEN
        H_row( : nnzh ) = work%H_row( : nnzh )
        H_col( : nnzh ) = work%H_col( : nnzh )

!  update the counters for the report tool

        work%nc2oh = work%nc2oh + 1
        work%nc2ch = work%nc2ch + work%pnc
      END IF

!  update elapsed CPU time if required

  990 CONTINUE
      IF ( work%record_times ) THEN
        CALL CPU_TIME( time_out )
        work%time_cshp = work%time_cshp + time_out - time_in
      END IF
      RETURN

!  end of subroutine CUTEST_cshp_threadsafe

      END SUBROUTINE CUTEST_cshp_threadsafe
