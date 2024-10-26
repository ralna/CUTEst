! THIS VERSION: CUTEST 2.2 - 2023-11-12 AT 10:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*-*-*-  C U T E S T  C I N T _  C S E T U P    S U B R O U T I N E  -*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Dominique Orban

!  History -
!   fortran 2003 version released in CUTEst, 21st August 2013

      SUBROUTINE CUTEST_Cint_csetup_r( status, input, out, io_buffer,          &
                                       n, m, X, X_l, X_u, Y, C_l, C_u,         &
                                       EQUATN, LINEAR,                         &
                                       e_order, l_order, v_order )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_Bool

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: input, out, io_buffer
      INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: n, m
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: e_order, l_order, v_order
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: X, X_l, X_u
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: Y, C_l, C_u
      LOGICAL (KIND = C_Bool), INTENT( OUT ), DIMENSION( m ) :: EQUATN, LINEAR

!  --------------------------------------------------------------------------
!  set up the input data for the constrained optimization tools (C interface)
!  --------------------------------------------------------------------------

!  local variables

      LOGICAL, DIMENSION( m ) :: EQUATN_FORTRAN, LINEAR_FORTRAN

      CALL CUTEST_csetup_r( status, input, out,                                &
                            io_buffer, n, m, X, X_l, X_u, Y, C_l, C_u,         &
                            EQUATN_FORTRAN, LINEAR_FORTRAN,                    &
                            e_order, l_order, v_order )

!   copy Fortran logical arrays into C-bound logical arrays

      EQUATN = EQUATN_FORTRAN
      LINEAR = LINEAR_FORTRAN

      RETURN

!  End of subroutine CUTEST_Cint_csetup_r

      END SUBROUTINE CUTEST_Cint_csetup_r

!-*-*-*-*-*-*-  C U T E S T    C S E T U P    S U B R O U T I N E  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28th December 2012

      SUBROUTINE CUTEST_csetup_r( status, input, out,                          &
                                  io_buffer, n, m, X, X_l, X_u, Y, C_l, C_u,   &
                                  EQUATN, LINEAR, e_order, l_order, v_order )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: input, out, io_buffer
      INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: n, m
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: e_order, l_order, v_order
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: X, X_l, X_u
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: Y, C_l, C_u
      LOGICAL, INTENT( OUT ), DIMENSION( m ) :: EQUATN, LINEAR

!  ------------------------------------------------------------
!  set up the input data for the constrained optimization tools
!  ------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: alloc_status
      CHARACTER ( LEN = 80 ) :: bad_alloc = REPEAT( ' ', 80 )

!  allocate space for the global workspace

      ALLOCATE( CUTEST_work_global( 1 ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'CUTEST_work_global' ; GO TO 910
      END IF

!  set the data

      CALL CUTEST_csetup_threadsafe_r( CUTEST_data_global,                     &
                                       CUTEST_work_global( 1 ),                &
                                       status, input, out, io_buffer,          &
                                       n, m, X, X_l, X_u, Y, C_l, C_u,         &
                                       EQUATN, LINEAR,                         &
                                       e_order, l_order, v_order )
      CUTEST_data_global%threads = 1
      RETURN

!  allocation error

  910 CONTINUE
      status = 1
      IF ( out > 0 ) WRITE( out,                                               &
        "( /, ' ** SUBROUTINE CUTEST_csetup: allocation error for ', A,        &
       & ' status = ', I0, /, ' Execution terminating ' )" )                   &
           TRIM( bad_alloc ), alloc_status
      RETURN

!  End of subroutine CUTEST_csetup_r

      END SUBROUTINE CUTEST_csetup_r

!-*-  C U T E S T    C S E T U P  _ t h r e a d e d   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 2003 version released in CUTEst, 28th December 2012

      SUBROUTINE CUTEST_csetup_threaded_r( status, input, out, threads,        &
                                           IO_BUFFERS, n, m, X, X_l, X_u,      &
                                           Y, C_l, C_u, EQUATN, LINEAR,        &
                                           e_order, l_order, v_order )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision

!  dummy arguments

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: input, out, threads
      INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: n, m
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: e_order, l_order, v_order
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( threads ) :: IO_BUFFERS
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: X, X_l, X_u
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: Y, C_l, C_u
      LOGICAL, INTENT( OUT ), DIMENSION( m ) :: EQUATN, LINEAR

!  ------------------------------------------------------------
!  set up the input data for the constrained optimization tools
!  ------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: i, alloc_status
      CHARACTER ( LEN = 80 ) :: bad_alloc = REPEAT( ' ', 80 )

!  threads must be positive

      IF ( threads < 1 ) GO TO 940

!  allocate space for the threaded workspace data

      ALLOCATE( CUTEST_work_global( threads ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'CUTEST_work_global' ; GO TO 910
      END IF

!  set the data

      CALL CUTEST_csetup_threadsafe_r( CUTEST_data_global,                     &
                                       CUTEST_work_global( 1 ),                &
                                       status, input, out, IO_BUFFERS( 1 ),    &
                                       n, m, X, X_l, X_u, Y, C_l, C_u,         &
                                       EQUATN, LINEAR,                         &
                                       e_order, l_order, v_order )
      CUTEST_data_global%threads = threads

!  initialize additional thread data

      DO i = 2, threads
        CALL CUTEST_initialize_thread( CUTEST_data_global,                     &
                                       CUTEST_work_global( i ), .TRUE.,        &
                                       status, alloc_status, bad_alloc )
        IF ( status /= 0 ) RETURN

!  ensure that each thread uses its own i/o buffer if required

        CUTEST_work_global( i )%io_buffer = IO_BUFFERS( i )

!  copy necessary starting addresses

        CUTEST_work_global( i )%ISTAJC( : CUTEST_data_global%n + 1 )           &
         = CUTEST_work_global( 1 )%ISTAJC( : CUTEST_data_global%n + 1 )
        CUTEST_work_global( i )%pnc = CUTEST_work_global( 1 )%pnc
      END DO
      RETURN

!  allocation error

  910 CONTINUE
      status = 1
      IF ( out > 0 ) WRITE( out,                                               &
        "( /, ' ** SUBROUTINE CUTEST_csetup: allocation error for ', A,        &
       & ' status = ', I0, /, ' Execution terminating ' )" )                   &
           TRIM( bad_alloc ), alloc_status
      RETURN

!  thread error

  940 CONTINUE
      status = 4
      IF ( out > 0 ) WRITE( out,                                               &
        "( /, ' ** SUBROUTINE CUTEST_csetup: argument threads must be',        &
       &  ' positive, execution terminating ' )" )
      RETURN

!  End of subroutine CUTEST_csetup_threaded_r

      END SUBROUTINE CUTEST_csetup_threaded_r

!-*-  C U T E S T   C S E T U P _ t h r e a d s a f e   S U B R O U T I N E  -*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould with modifications by Ingrid Bongartz

!  History -
!   fortran 77 version originally released in CUTE, 30th October, 1991
!   fortran 2003 version released in CUTEst, 4th November 2012

      SUBROUTINE CUTEST_csetup_threadsafe_r( data, work, status, input, out,   &
                                io_buffer, n, m, X, X_l, X_u, Y, C_l, C_u,     &
                                EQUATN, LINEAR, e_order, l_order, v_order )
      USE CUTEST_KINDS_precision
      USE CUTEST_precision
      INTEGER ( KIND = ip_ ), PARAMETER :: sp = KIND( 1.0 )

!  dummy arguments

      TYPE ( CUTEST_data_type ), INTENT( INOUT ) :: data
      TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: input, out, io_buffer
      INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: n, m
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: e_order, l_order, v_order
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: X, X_l, X_u
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( m ) :: Y, C_l, C_u
      LOGICAL, INTENT( OUT ), DIMENSION( m ) :: EQUATN, LINEAR

!  ------------------------------------------------------------
!  set up the input data for the constrained optimization tools
!  ------------------------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: count, count_rate, ialgor, i, ig, j, jg
      INTEGER ( KIND = ip_ ) :: ii, k, iel, jwrk, kndv, itemp
      INTEGER ( KIND = ip_ ) :: mend, mmax, nend, neltyp, ngrtyp, nnlin
      INTEGER ( KIND = ip_ ) :: alloc_status
      LOGICAL :: debug, ltemp
      CHARACTER ( LEN = 10 ) :: pname
      CHARACTER ( LEN = 10 ) :: ctemp
      CHARACTER ( LEN = 80 ) :: bad_alloc = REPEAT( ' ', 80 )
      REAL ( KIND = rp_ ) :: atemp
      INTEGER ( KIND = ip_ ), PARAMETER :: lmin = 10000
      REAL ( KIND = rp_ ), DIMENSION( 2 ) :: OBFBND

      CALL CPU_TIME( data%su_cpu_time )
      CALL SYSTEM_CLOCK( count = count, count_rate = count_rate )
      data%su_time = REAL( count, KIND = sp ) / REAL( count_rate, KIND = sp )
      data%out = out
      work%io_buffer = io_buffer
      debug = .FALSE.
!     debug = .TRUE.
      debug = debug .AND. out > 0

!  input the problem dimensions

      REWIND( input )
      READ( input, "( 10I10 )" ) data%n, data%ng, data%nel, data%ntotel,       &
        data%nvrels, data%nnza, data%ngpvlu, data%nepvlu, neltyp, ngrtyp
      n = data%n
      IF ( n <= 0 ) THEN
        CLOSE( input )
        IF ( out > 0 ) WRITE( out,                                             &
          "( /, ' ** SUBROUTINE CUTEST_csetup: the problem uses no',           &
          &     ' variables. Execution terminating ' )" )
        status = 2 ; RETURN
      END IF
      IF ( data%ng <= 0 ) THEN
        CLOSE( input )
        IF ( out > 0 ) WRITE( out,                                             &
          "( /, ' ** SUBROUTINE CUTEST_csetup: the problem is vacuous.',       &
          &     ' Execution terminating ' )" )
        status = 2 ; RETURN
      END IF
      IF ( SIZE( X ) < n ) THEN
        CLOSE( input )
        IF ( out > 0 ) WRITE( out, 2000 ) 'X', n
        status = 2 ; RETURN
      END IF
      IF ( SIZE( X_l ) < n ) THEN
        CLOSE( input )
        IF ( out > 0 ) WRITE( out, 2000 ) 'X_l', n
        status = 2 ; RETURN
      END IF
      IF ( SIZE( X_u ) < n ) THEN
        CLOSE( input )
        IF ( out > 0 ) WRITE( out, 2000 ) 'X_u', n
        status = 2 ; RETURN
      END IF

!  input the problem type

      READ( input, "( I2, A10 )" ) ialgor, pname

!  set useful integer values

      data%ng1 = data%ng + 1
      data%ngng = data%ng + data%ng
      data%nel1 = data%nel + 1

!  allocate integer workspace

      ALLOCATE( data%ISTADG( data%ng1 ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%ISTADG' ; GO TO 910
      END IF

      ALLOCATE( data%ISTGP( data%ng1 ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%ISTGP' ; GO TO 910
      END IF

      ALLOCATE( data%ISTADA( data%ng1 ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%ISTADA' ; GO TO 910
      END IF

      ALLOCATE( data%ISTAEV( data%nel1 ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%ISTAEV' ; GO TO 910
      END IF

      ALLOCATE( data%ISTEP( data%nel1 ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%ISTEP' ; GO TO 910
      END IF

      ALLOCATE( data%ITYPEG( data%ng ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%ITYPEG' ; GO TO 910
      END IF

      ALLOCATE( data%KNDOFC( data%ng ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%KNDOFC' ; GO TO 910
      END IF

      ALLOCATE( data%ITYPEE( data%nel ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%ITYPEE' ; GO TO 910
      END IF

      ALLOCATE( data%IELING( data%ntotel ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%IELING' ; GO TO 910
      END IF

      ALLOCATE( data%IELVAR( data%nvrels ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%IELVAR' ; GO TO 910
      END IF

      ALLOCATE( data%ICNA( data%nnza ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%ICNA' ; GO TO 910
      END IF

      ALLOCATE( data%ISTADH( data%nel1 ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%ISTADH' ; GO TO 910
      END IF

      ALLOCATE( data%INTVAR( data%nel1 ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%INTVAR' ; GO TO 910
      END IF

      ALLOCATE( data%IVAR( n ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%IVAR' ; GO TO 910
      END IF

      ALLOCATE( work%ICALCF( MAX( data%nel, data%ng ) ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'work%ICALCF' ; GO TO 910
      END IF

      ALLOCATE( data%ITYPEV( n ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%ITYPEV' ; GO TO 910
      END IF

      ALLOCATE( data%CGROUP( MAX( m, 2 * n ) ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%CGROUP' ; GO TO 910
      END IF

!  allocate real workspace

      ALLOCATE( data%A( data%nnza ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%A' ; GO TO 910
      END IF

      ALLOCATE( data%B( data%ng ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%B' ; GO TO 910
      END IF

      ALLOCATE( data%U( data%ng ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%U' ; GO TO 910
      END IF

      ALLOCATE( data%GPVALU( data%ngpvlu ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%GPVALU' ; GO TO 910
      END IF

      ALLOCATE( data%EPVALU( data%nepvlu ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%EPVALU' ; GO TO 910
      END IF

      ALLOCATE( data%ESCALE( data%ntotel ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%ESCALE' ; GO TO 910
      END IF

      ALLOCATE( data%GSCALE( data%ng ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%GSCALE' ; GO TO 910
      END IF

      ALLOCATE( work%GSCALE_used( data%ng ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'work%GSCALE_used' ; GO TO 910
      END IF

      ALLOCATE( data%VSCALE( n ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%VSCALE' ; GO TO 910
      END IF

      ALLOCATE( work%GVALS( data%ng, 3 ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'work%GVALS' ; GO TO 910
      END IF

      ALLOCATE( work%G_temp( n ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'work%G_temp' ; GO TO 910
      END IF

      ALLOCATE( work%FT( data%ng ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'work%FT' ; GO TO 910
      END IF

!  allocate logical workspace

      ALLOCATE( data%INTREP( data%nel ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%INTREP' ; GO TO 910
      END IF

      ALLOCATE( data%GXEQX( data%ngng ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%GXEQX' ; GO TO 910
      END IF

      ALLOCATE( work%LOGIC( data%nel ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'work%LOGIC' ; GO TO 910
      END IF

!  allocate character workspace

      ALLOCATE( data%GNAMES( data%ng ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%GNAMES' ; GO TO 910
      END IF

      ALLOCATE( data%VNAMES( n ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'data%VNAMES' ; GO TO 910
      END IF

!  record the lengths of arrays

      data%ltypee = data%nel
      data%ltypeg = data%ng
      data%lstep  = data%nel1
      data%lstgp = data%ng1
      data%lcalcf = MAX( data%nel, data%ng )
      data%lcalcg = data%ng
      data%lstaev = data%nel1
      data%lelvar = data%nvrels
      data%lntvar = data%nel1
      data%lstadh = data%nel1
      data%lvscal = n
      data%lepvlu = data%nepvlu
      data%lgpvlu = data%ngpvlu

!  assign initial guesses for allocatable array lengths

      work%lh_row = lmin
      work%lh_col = lmin
      work%lh_val = lmin

!  print out problem data. input the number of variables, groups, elements and
!  the identity of the objective function group

      IF ( debug ) WRITE( out, 1100 ) pname, n, data%ng, data%nel
      data%pname = pname

!  input the starting addresses of the elements in each group, of the parameters
!  used for each group and of the nonzeros of the linear element in each group

      READ( input, 1010 ) ( data%ISTADG( i ), i = 1, data%ng1 )
      IF ( debug ) WRITE( out, 1110 ) 'ISTADG',                                &
        ( data%ISTADG( i ), i = 1, data%ng1 )
      READ( input, 1010 ) ( data%ISTGP( i ), i = 1, data%ng1 )
      IF ( debug ) WRITE( out, 1110 ) 'ISTGP ',                                &
        ( data%ISTGP( i ), i = 1, data%ng1 )
      READ( input, 1010 ) ( data%ISTADA( i ), i = 1, data%ng1 )
      IF ( debug ) WRITE( out, 1110 ) 'ISTADA',                                &
        ( data%ISTADA( i ), i = 1, data%ng1 )

!  input the starting addresses of the variables and parameters in each element

      READ( input, 1010 ) ( data%ISTAEV( i ), i = 1, data%nel1 )
      IF ( debug ) WRITE( out, 1110 ) 'ISTAEV',                                &
        ( data%ISTAEV( i ), i = 1, data%nel1 )
      READ( input, 1010 ) ( data%ISTEP( i ), i = 1, data%nel1 )
      IF ( debug ) WRITE( out, 1110 ) 'ISTEP ',                                &
        ( data%ISTEP( i ), i = 1, data%nel1 )

!  input the group type of each group

      READ( input, 1010 ) ( data%ITYPEG( i ), i = 1, data%ng )
      IF ( debug ) WRITE( out, 1110 ) 'ITYPEG',                                &
        ( data%ITYPEG( i ), i = 1, data%ng )
      IF ( ialgor >= 2 ) THEN
         READ( input, 1010 ) ( data%KNDOFC( i ), i = 1, data%ng )
         IF ( debug ) WRITE( out, 1110 ) 'KNDOFC',                             &
        ( data%KNDOFC( i ), i = 1, data%ng )
      END IF

!  input the element type of each element

      READ( input, 1010 ) ( data%ITYPEE( i ), i = 1, data%nel )
      IF ( debug ) WRITE( out, 1110 ) 'ITYPEE',                                &
        ( data%ITYPEE( i ), i = 1, data%nel )

!  input the number of internal variables for each element

      READ( input, 1010 ) ( data%INTVAR( i ), i = 1, data%nel )
      IF ( debug ) WRITE( out, 1110 ) 'INTVAR',                                &
        ( data%INTVAR( i ), i = 1, data%nel )

!  input the identity of each individual element

      READ( input, 1010 ) ( data%IELING( i ), i = 1, data%ntotel )
      IF ( debug ) WRITE( out, 1110 ) 'IELING',                                &
        ( data%IELING( i ), i = 1, data%ntotel )

!  input the variables in each group's elements

      data%nvrels = data%ISTAEV( data%nel1 ) - 1
      READ( input, 1010 ) ( data%IELVAR( i ), i = 1, data%nvrels )
      IF ( debug ) WRITE( out, 1110 ) 'IELVAR',                                &
        ( data%IELVAR( i ), i = 1, data%nvrels )

!  input the column addresses of the nonzeros in each linear element

      READ( input, 1010 ) ( data%ICNA( i ), i = 1, data%nnza )
      IF ( debug ) WRITE( out, 1110 ) 'ICNA  ',                                &
        ( data%ICNA( i ), i = 1, data%nnza )

!  input the values of the nonzeros in each linear element, the constant term
!  in each group, the lower and upper bounds on the variables.

      READ( input, 1020 ) ( data%A( i ), i = 1, data%nnza )
      IF ( debug ) WRITE( out, 1120 ) 'A     ', ( data%A( i ), i = 1, data%nnza)
      READ( input, 1020 ) ( data%B( i ), i = 1, data%ng )
      IF ( debug ) WRITE( out, 1120 ) 'B     ', ( data%B( i ), i = 1, data%ng )
      IF ( ialgor <= 2 ) THEN
         READ( input, 1020 ) ( X_l( i ), i = 1, n )
         IF ( debug ) WRITE( out, 1120 ) 'X_l    ', ( X_l( i ), i = 1, n )
         READ( input, 1020 ) ( X_u( i ), i = 1, n )
         IF ( debug ) WRITE( out, 1120 ) 'X_u    ', ( X_u( i ), i = 1, n )
      ELSE

!  use GVALS and FT as temporary storage for the constraint bounds

         READ( input, 1020 ) ( X_l( i ), i = 1, n ),                           &
           ( work%GVALS( i, 1 ), i = 1, data%ng )
         IF ( debug ) WRITE( out, 1120 ) 'X_l    ',                            &
           ( X_l( i ), i = 1, n ), ( work%GVALS( i, 1 ), i = 1, data%ng )
         READ( input, 1020 ) ( X_u( i ), i = 1, n ),                           &
           ( work%FT( i ), i = 1, data%ng )
         IF ( debug ) WRITE( out, 1120 ) 'X_u    ',                            &
           ( X_u( i ), i = 1, n ), ( work%FT( i ), i = 1, data%ng )
      END IF

!   input the starting point for the minimization

      READ( input, 1020 ) ( X( i ), i = 1, n )
      IF ( debug ) WRITE( out, 1120 ) 'X     ', ( X( i ), i = 1, n )
      IF ( ialgor >= 2 ) THEN
         READ( input, 1020 )( data%U( i ), i = 1, data%ng )
         IF ( debug ) WRITE( out, 1120 ) 'U     ',                             &
            ( data%U( i ), i = 1, data%ng )
      END IF

!  input the parameters in each group

      READ( input, 1020 ) ( data%GPVALU( i ), i = 1, data%ngpvlu )
      IF ( debug ) WRITE( out, 1120 ) 'GPVALU',                                &
        ( data%GPVALU( i ), i = 1, data%ngpvlu )

!  input the parameters in each individual element

      READ( input, 1020 ) ( data%EPVALU( i ), i = 1, data%nepvlu )
      IF ( debug ) WRITE( out, 1120 ) 'EPVALU',                                &
        ( data%EPVALU( i ), i = 1, data%nepvlu )

!  input the scale factors for the nonlinear elements

      READ( input, 1020 ) ( data%ESCALE( i ), i = 1, data%ntotel )
      IF ( debug ) WRITE( out, 1120 ) 'ESCALE',                                &
        ( data%ESCALE( i ), i = 1, data%ntotel )

!  input the scale factors for the groups

      READ( input, 1020 ) ( data%GSCALE( i ), i = 1, data%ng )
      IF ( debug ) WRITE( out, 1120 ) 'GSCALE',                                &
        ( data%GSCALE( i ), i = 1, data%ng )

!  input the scale factors for the variables

      READ( input, 1020 ) ( data%VSCALE( i ), i = 1, n )
      IF ( debug ) WRITE( out, 1120 ) 'VSCALE',                                &
        ( data%VSCALE( i ), i = 1, n )

!  input the lower and upper bounds on the objective function

      READ( input, 1080 ) OBFBND( 1 ), OBFBND( 2 )
      IF ( debug ) WRITE( out, 1180 ) 'OBFBND', OBFBND( 1 ), OBFBND( 2 )

!  input a logical array which says whether an element has internal variables

      READ( input, 1030 ) ( data%INTREP( i ), i = 1, data%nel )
      IF ( debug ) WRITE( out, 1130 ) 'INTREP',                                &
        ( data%INTREP( i ), i = 1, data%nel )

!  input a logical array which says whether a group is trivial

      READ( input, 1030 ) ( data%GXEQX( i ), i = 1, data%ng )
      IF ( debug ) WRITE( out, 1130 ) 'GXEQX ',                                &
        ( data%GXEQX( i ), i = 1, data%ng )

!  input the names given to the groups and to the variables

      READ( input, 1040 ) ( data%GNAMES( i ), i = 1, data%ng )
      IF ( debug ) WRITE( out, 1140 ) 'GNAMES',                                &
        ( data%GNAMES( i ), i = 1, data%ng )
      READ( input, 1040 ) ( data%VNAMES( i ), i = 1, n )
      IF ( debug ) WRITE( out, 1140 ) 'VNAMES',                                &
        ( data%VNAMES( i ), i = 1, n )

!  dummy input for the names given to the element and group types

      READ( input, 1040 ) ( ctemp, i = 1, neltyp )
      READ( input, 1040 ) ( ctemp, i = 1, ngrtyp )

!  input the type of each variable.

      READ( input, 1010 ) ( data%ITYPEV( i ), i = 1, n )

!  consider which groups are constraints. Of these, decide which are
!  equations, which are linear, allocate starting values for the
!  Lagrange multipliers and set lower and upper bounds on any inequality
!  constraints. Reset KNDOFC to point to the list of constraint groups.

      mmax = MIN( SIZE( LINEAR ), SIZE( EQUATN ),                              &
                  SIZE( C_l ), SIZE( C_u ), SIZE( Y )  )
      m = 0
      DO i = 1, data%ng
        IF ( data%KNDOFC( i ) == 1 ) THEN
          data%KNDOFC( i ) = 0
        ELSE
          m = m + 1
          IF ( m <= mmax ) THEN
            Y( m ) = data%U( i )
            LINEAR( m ) =                                                      &
              data%GXEQX( i ) .AND. data%ISTADG( i ) >= data%ISTADG( i + 1 )
            IF ( data%KNDOFC( i ) == 2 ) THEN
              EQUATN( m ) = .TRUE.
              C_l( m ) = 0.0_rp_ ; C_u( m ) = 0.0_rp_
            ELSE
              EQUATN( m ) = .FALSE.
              C_l( m ) = work%GVALS( i, 1 ) ; C_u( m ) = work%FT( i )
            END IF
          END IF
          data%KNDOFC( i ) = m
        END IF
      END DO

      IF ( m == 0 .AND. out > 0 ) WRITE( out,                                  &
        "( /, ' ** SUBROUTINE CUTEST_csetup: Warning. The problem has no',     &
       &      ' general constraints. ', /,  ' ** Other tools may be',          &
      &       ' preferable ** ' )" )
      IF ( SIZE( Y ) < m ) THEN
        CLOSE( input )
        IF ( out > 0 ) WRITE( out, 2000 ) 'Y', m
        status = 2 ; RETURN
      END IF
      IF ( SIZE( C_l ) < m ) THEN
        CLOSE( input )
        IF ( out > 0 ) WRITE( out, 2000 ) 'C_l', m
        status = 2 ; RETURN
      END IF
      IF ( SIZE( C_u ) < m ) THEN
        CLOSE( input )
        IF ( out > 0 ) WRITE( out, 2000 ) 'C_u', m
        status = 2 ; RETURN
      END IF
      IF ( SIZE( EQUATN ) < m ) THEN
        CLOSE( input )
        IF ( out > 0 ) WRITE( out, 2000 ) 'EQUATN', m
        status = 2 ; RETURN
      END IF
      IF ( SIZE( LINEAR ) < m ) THEN
        CLOSE( input )
        IF ( out > 0 ) WRITE( out, 2000 ) 'LINEAR', m
        status = 2 ; RETURN
      END IF
      data%numcon = m

      IF ( v_order == 1 .OR. v_order == 2 ) THEN

!  set addresses in CGROUP to reorder variables

        kndv = 0
        jwrk = kndv + n

!  initialize jwrk and kndv

        DO j = 1, n
          data%CGROUP( kndv + j ) = 0
          data%CGROUP( jwrk + j ) = j
        END DO

!  now identify and count nonlinear variables; keep separate counts for
!  nonlinear objective and Jacobian variables.
!  data%CGROUP(kndv + j) = 0 ==> j linear everywhere
!  data%CGROUP(kndv + j) = 1 ==> j linear in objective, nonlinear in constraints
!  data%CGROUP(kndv + j) = 2 ==> j linear in constraints, nonlinear in objective
!  data%CGROUP(kndv + j) = 3 ==> j nonlinear everywhere

        nnlin = 0
        data%nnov = 0
        data%nnjv = 0
        DO ig = 1, data%ng
          i = data%KNDOFC( ig )
          DO  ii = data%ISTADG( ig ), data%ISTADG( ig + 1 ) - 1
            iel = data%IELING( ii )
            DO k = data%ISTAEV( iel ), data%ISTAEV( iel + 1 ) - 1
              j = data%IELVAR( k )
              IF ( i > 0 ) THEN
                IF ( data%CGROUP( kndv + j ) == 0 ) THEN
                  data%CGROUP( kndv + j ) = 1
                  data%nnjv = data%nnjv + 1
                  nnlin = nnlin + 1
                ELSE IF ( data%CGROUP( kndv + j ) == 2 ) THEN
                  data%CGROUP( kndv + j ) = 3
                  data%nnjv = data%nnjv + 1
                END IF
              ELSE
                IF ( data%CGROUP( kndv + j ) == 0 ) THEN
                  data%CGROUP( kndv + j ) = 2
                  data%nnov = data%nnov + 1
                  nnlin = nnlin + 1
                ELSE IF ( data%CGROUP( kndv + j ) == 1 ) THEN
                  data%CGROUP( kndv + j ) = 3
                  data%nnov = data%nnov + 1
                END IF
              END IF
            END DO
          END DO
          IF ( .NOT. data%GXEQX( ig ) ) THEN
            DO ii = data%ISTADA( ig ), data%ISTADA( ig + 1 ) - 1
              j = data%ICNA( ii )
              IF ( i > 0 ) THEN
                IF ( data%CGROUP( kndv + j ) == 0 ) THEN
                  data%CGROUP( kndv + j ) = 1
                  data%nnjv = data%nnjv + 1
                  nnlin = nnlin + 1
                ELSE IF ( data%CGROUP( kndv + j ) == 2 ) THEN
                  data%CGROUP( kndv + j ) = 3
                  data%nnjv = data%nnjv + 1
                END IF
              ELSE
                IF ( data%CGROUP( kndv + j ) == 0 ) THEN
                  data%CGROUP( kndv + j ) = 2
                  data%nnov = data%nnov + 1
                  nnlin = nnlin + 1
                ELSE IF ( data%CGROUP( kndv + j ) == 1 ) THEN
                  data%CGROUP( kndv + j ) = 3
                  data%nnov = data%nnov + 1
                END IF
              END IF
            END DO
          END IF
        END DO
        IF ( nnlin == 0 .OR. ( data%nnov == n .AND. data%nnjv == n ) ) GO TO 300
        IF ( nnlin == n ) GO TO 200

!  reorder the variables so that all nonlinear variables occur before the
!  linear ones

        IF ( v_order == 1 ) THEN
          nend = n

!  run forward through the variables until a linear variable is encountered

          DO 120 i = 1, n
            IF ( i > nend ) GO TO 130

!  variable i is linear. Now, run backwards through the variables until a
!  nonlinear one is encountered

            IF ( data%CGROUP( kndv + i ) == 0 ) THEN
              DO j = nend, i, - 1
                IF ( data%CGROUP( kndv + j ) > 0 ) THEN
                  nend = j - 1

!  interchange the data for variables i and j

                  itemp = data%CGROUP( jwrk + i )
                  data%CGROUP( jwrk + i ) = data%CGROUP( jwrk + j )
                  data%CGROUP( jwrk + j ) = itemp
                  itemp = data%CGROUP( kndv + i )
                  data%CGROUP( kndv + i ) = data%CGROUP( kndv + j )
                  data%CGROUP( kndv + j ) = itemp
                  atemp = X_l( i ) ; X_l( i ) = X_l( j ) ; X_l( j ) = atemp
                  atemp = X_u( i ) ; X_u( i ) = X_u( j ) ; X_u( j ) = atemp
                  atemp = X( i ) ; X( i ) = X( j ) ; X( j ) = atemp
                  atemp = data%VSCALE( i )
                  data%VSCALE( i ) = data%VSCALE( j )
                  data%VSCALE( j ) = atemp
                  ctemp = data%VNAMES( i )
                  data%VNAMES( i ) = data%VNAMES( j )
                  data%VNAMES( j ) = ctemp
                  GO TO 120
                END IF
              END DO
              GO TO 130
            END IF
  120     CONTINUE
  130     CONTINUE

!  reorder the variables so that all nonlinear variables occur after the
!  linear ones

        ELSE
          nend = n

!  run forward through the variables until a nonlinear variable is encountered

          DO 140 i = 1, n
            IF ( i > nend ) GO TO 150

!  variable i is linear. Now, run backwards through the variables until a
!  linear one is encountered

            IF ( data%CGROUP( kndv + i ) > 0 ) THEN
              DO j = nend, i, - 1
                IF ( data%CGROUP( kndv + j ) == 0 ) THEN
                  nend = j - 1

!  interchange the data for variables i and j

                  itemp = data%CGROUP( jwrk + i )
                  data%CGROUP( jwrk + i ) = data%CGROUP( jwrk + j )
                  data%CGROUP( jwrk + j ) = itemp
                  itemp = data%CGROUP( kndv + i )
                  data%CGROUP( kndv + i ) = data%CGROUP( kndv + j )
                  data%CGROUP( kndv + j ) = itemp
                  atemp = X_l( i ) ; X_l( i ) = X_l( j ) ; X_l( j ) = atemp
                  atemp = X_u( i ) ; X_u( i ) = X_u( j ) ; X_u( j ) = atemp
                  atemp = X( i ) ; X( i ) = X( j ) ; X( j ) = atemp
                  atemp = data%VSCALE( i )
                  data%VSCALE( i ) = data%VSCALE( j )
                  data%VSCALE( j ) = atemp
                  ctemp = data%VNAMES( i )
                  data%VNAMES( i ) = data%VNAMES( j )
                  data%VNAMES( j ) = ctemp
                  GO TO 140
                END IF
              END DO
              GO TO 150
            END IF
  140     CONTINUE
  150     CONTINUE
        END IF

!  change entries in IELVAR and ICNA to reflect reordering of variables

        DO i = 1, data%nvrels
          j = data%IELVAR( i )
          data%IELVAR( i ) = data%CGROUP( jwrk + j )
        END DO
        DO i = 1, data%nnza
          j = data%ICNA( i )
          data%ICNA( i ) = data%CGROUP( jwrk + j )
        END DO
        DO j = 1, n
           data%CGROUP( jwrk + j ) = j
        END DO
  200   CONTINUE
        IF ( ( data%nnov == nnlin .AND. data%nnjv == nnlin )                   &
           .OR. ( data%nnov == 0 ) .OR. ( data%nnjv == 0 ) ) GO TO 300

!  reorder the nonlinear variables so that the smaller set (nonlinear objective
!  or nonlinear Jacobian) occurs at the beginning of the larger set

        nend = nnlin
        IF ( data%nnjv <= data%nnov ) THEN

!  put the nonlinear Jacobian variables first. Reset data%nnov to indicate all
!  nonlinear variables are treated as  nonlinear objective variables.

          data%nnov = nnlin
          DO 220 i = 1, nnlin
            IF ( i > nend ) GO TO 290

!  variable i is linear in the Jacobian. Now, run backwards through the
!  variables until a nonlinear Jacobian variable is encountered

            IF ( data%CGROUP( kndv + i ) == 2 ) THEN
              DO j = nend, i, - 1
                IF ( data%CGROUP( kndv + j ) == 1 .OR.                         &
                     data%CGROUP( kndv + j ) == 3 ) THEN
                  nend = j - 1

!  Interchange the data for variables i and j

                  itemp = data%CGROUP( jwrk + i )
                  data%CGROUP( jwrk + i ) = data%CGROUP( jwrk + j )
                  data%CGROUP( jwrk + j ) = itemp
                  itemp = data%CGROUP( kndv + i )
                  data%CGROUP( kndv + i ) = data%CGROUP( kndv + j )
                  data%CGROUP( kndv + j ) = itemp
                  atemp = X_l( i ) ; X_l( i ) = X_l( j ) ; X_l( j ) = atemp
                  atemp = X_u( i ) ; X_u( i ) = X_u( j ) ; X_u( j ) = atemp
                  atemp = X( i ) ; X( i ) = X( j ) ; X( j ) = atemp
                  atemp = data%VSCALE( i )
                  data%VSCALE( i ) = data%VSCALE( j )
                  data%VSCALE( j ) = atemp
                  ctemp = data%VNAMES( i )
                  data%VNAMES( i ) = data%VNAMES( j )
                  data%VNAMES( j ) = ctemp
                  GO TO 220
                END IF
              END DO
              GO TO 290
            END IF
  220     CONTINUE

!  put the nonlinear objective variables first. Reset data%nnjv to indicate all
!  nonlinear variables are treated as nonlinear Jacobian variables.

        ELSE
          data%nnjv = nnlin
          DO 250 i = 1, nnlin
            IF ( i > nend ) GO TO 290

!  variable i is linear in the objective. Now, run backwards through the
!  variables until a nonlinear objective variable is encountered

            IF ( data%CGROUP( kndv + i ) == 1 ) THEN
              DO 240 j = nend, i, - 1
                 IF ( data%CGROUP( kndv + j ) > 1 ) THEN
                   nend = j - 1

!  interchange the data for variables i and j

                   itemp = data%CGROUP( jwrk + i )
                   data%CGROUP( jwrk + i ) = data%CGROUP( jwrk + j )
                   data%CGROUP( jwrk + j ) = itemp
                   itemp = data%CGROUP( kndv + i )
                   data%CGROUP( kndv + i ) = data%CGROUP( kndv + j )
                   data%CGROUP( kndv + j ) = itemp
                   atemp = X_l( i ) ;  X_l( i ) = X_l( j ) ;  X_l( j ) = atemp
                   atemp = X_u( i ) ;  X_u( i ) = X_u( j ) ;  X_u( j ) = atemp
                   atemp = X( i ) ;  X( i ) = X( j ) ;  X( j ) = atemp
                   atemp = data%VSCALE( i )
                   data%VSCALE( i ) = data%VSCALE( j )
                   data%VSCALE( j ) = atemp
                   ctemp = data%VNAMES( i )
                   data%VNAMES( i ) = data%VNAMES( j )
                   data%VNAMES( j ) = ctemp
                   GO TO 250
                 END IF
  240         CONTINUE
              GO TO 290
            END IF
  250     CONTINUE
        END IF

!  change entries in IELVAR and ICNA to reflect reordering of variables

  290   CONTINUE
        DO i = 1, data%nvrels
          j = data%IELVAR( i )
          data%IELVAR( i ) = data%CGROUP( jwrk + j )
        END DO
        DO i = 1, data%nnza
          j = data%ICNA( i )
          data%ICNA( i ) = data%CGROUP( jwrk + j )
        END DO
  300   CONTINUE
      ELSE
        data%nnov = n
        data%nnjv = n
      END IF

!  allocate and initialize workspace

      work%firstg = .TRUE.

      data%ntotel = data%ISTADG( data%ng + 1 ) - 1
      data%nvrels = data%ISTAEV( data%nel + 1 ) - 1
      data%nnza = data%ISTADA( data%ng + 1 ) - 1

      CALL CUTEST_initialize_workspace(                                        &
             data%n, data%ng, data%nel, data%ntotel, data%nvrels, data%nnza,   &
             data%nvargp, data%IELING, data%ISTADG, data%IELVAR, data%ISTAEV,  &
             data%INTVAR, data%ISTADH, data%ICNA, data%ISTADA,                 &
             data%GXEQX, data%alllin, data%altriv, data%lfxi, data%lgxi,       &
             data%lhxi, data%lggfx, data%ldx, data%lgrjac, data%lnguvl,        &
             data%lnhuvl, data%ntotin, data%maxsel, data%maxsin, 0_ip_,        &
             out, work%io_buffer, data%l_link_e_u_v, work%nbprod,              &
             work%FUVALS, data%lfuval, data%LINK_elem_uses_var,                &
             work%ISWKSP, work%IUSED, work%ISTAJC, data%ISTAGV, data%ISVGRP,   &
             data%ISLGRP, data%IGCOLJ, data%IVALJR, data%ISYMMH,               &
             data%LIST_elements, work%NZ_components_w,                         &
             work%W_ws, work%W_el, work%W_in, work%H_el, work%H_in,            &
             status, alloc_status, bad_alloc, work%array_status )
      IF ( status /= 0 ) RETURN

!  record which group is associated with each constraint

      data%meq = 0 ; data%mlin = 0
      DO ig = 1, data%ng
        i = data%KNDOFC( ig )
        IF ( i > 0 ) THEN
          data%CGROUP( i ) = ig
          IF ( EQUATN( i ) ) data%meq = data%meq + 1
          IF ( LINEAR( i ) ) data%mlin = data%mlin + 1
        END IF
      END DO

      IF ( .NOT. ( e_order == 1 .OR. e_order == 2 .OR.                         &
                   l_order == 1 .OR. l_order == 2 ) .OR. m == 0 ) GO TO 700

!  the constraints are to be reordered to separate linear and nonlinear ones

      IF ( l_order == 1 .OR. l_order == 2 ) THEN
        IF ( data%mlin == 0 .OR. data%mlin == m ) GO TO 390

!  reorder the constraints so that the linear constraints occur before the
!  nonlinear ones

        IF ( l_order == 1 ) THEN
          mend = m

!  run forward through the constraints until a nonlinear constraint is found

          DO 320 i = 1, m
            IF ( i > mend ) GO TO 390
            ig = data%CGROUP( i )

!  constraint i is nonlinear. Now, run backwards through the constraints until
!  a linear one is encountered

            IF ( .NOT. LINEAR( i ) ) THEN
              DO j = mend, i, - 1
                jg = data%CGROUP( j )
                IF ( LINEAR( j ) ) THEN
                   mend = j - 1

!  interchange the data for constraints i and j

                   data%CGROUP( i ) = jg
                   data%CGROUP( j ) = ig
                   data%KNDOFC( ig ) = j
                   data%KNDOFC( jg ) = i
                   ltemp = LINEAR( i )
                   LINEAR( i ) = LINEAR( j )
                   LINEAR( j ) = ltemp
                   ltemp = EQUATN( i )
                   EQUATN( i ) = EQUATN( j )
                   EQUATN( j ) = ltemp
                   atemp = Y( i ) ; Y( i ) = Y( j ) ; Y( j ) = atemp
                   atemp = C_l( i ) ; C_l( i ) = C_l( j ) ; C_l( j ) = atemp
                   atemp = C_u( i ) ; C_u( i ) = C_u( j ) ; C_u( j ) = atemp
                   GO TO 320
                 END IF
              END DO
              GO TO 390
            END IF
  320     CONTINUE

!  reorder the constraints so that the nonlinear constraints occur before the
!  linear ones

        ELSE
          mend = m

!  run forward through the constraints until a nonlinear constraint is found

          DO 340 i = 1, m
            IF ( i > mend ) GO TO 390
            ig = data%CGROUP( i )

!  constraint i is nonlinear. Now, run backwards through the constraints until
!  a linear one is encountered

            IF ( LINEAR( i ) ) THEN
              DO j = mend, i, - 1
                jg = data%CGROUP( j )
                IF ( .NOT. LINEAR( j ) ) THEN
                   mend = j - 1

!  interchange the data for constraints i and j

                   data%CGROUP( i ) = jg
                   data%CGROUP( j ) = ig
                   data%KNDOFC( ig ) = j
                   data%KNDOFC( jg ) = i
                   ltemp = LINEAR( i )
                   LINEAR( i ) = LINEAR( j )
                   LINEAR( j ) = ltemp
                   ltemp = EQUATN( i )
                   EQUATN( i ) = EQUATN( j )
                   EQUATN( j ) = ltemp
                   atemp = Y( i ) ; Y( i ) = Y( j ) ; Y( j ) = atemp
                   atemp = C_l( i ) ; C_l( i ) = C_l( j ) ; C_l( j ) = atemp
                   atemp = C_u( i ) ; C_u( i ) = C_u( j ) ; C_u( j ) = atemp
                   GO TO 340
                 END IF
              END DO
              GO TO 390
            END IF
  340     CONTINUE
        END IF

!  furrther reorderings to separate equality and inequality constraints

  390   CONTINUE
        IF ( data%meq == 0 .OR. data%meq == m ) GO TO 700

!  reorder the linear constraints so that the equations occur before the
!  inequalities

        IF ( e_order == 1 ) THEN
          mend = data%mlin
          DO 420 i = 1, data%mlin
            IF ( i > mend ) GO TO 430
            ig = data%CGROUP( i )

!  constraint i is an inequality. Now, run backwards through the constraints
!  until an equation is encountered

            IF ( .NOT. EQUATN( i ) ) THEN
              DO j = mend, i, - 1
                jg = data%CGROUP( j )
                IF ( EQUATN( j ) ) THEN
                  mend = j - 1

!  interchange the data for constraints i and j

                  data%CGROUP( i ) = jg
                  data%CGROUP( j ) = ig
                  data%KNDOFC( ig ) = j
                  data%KNDOFC( jg ) = i
                  ltemp = LINEAR( i )
                  LINEAR( i ) = LINEAR( j )
                  LINEAR( j ) = ltemp
                  ltemp = EQUATN( i )
                  EQUATN( i ) = EQUATN( j )
                  EQUATN( j ) = ltemp
                  atemp = Y( i ) ; Y( i ) = Y( j ) ; Y( j ) = atemp
                  atemp = C_l( i ) ; C_l( i ) = C_l( j ) ; C_l( j ) = atemp
                  atemp = C_u( i ) ; C_u( i ) = C_u( j ) ; C_u( j ) = atemp
                  GO TO 420
                END IF
              END DO
              GO TO 430
            END IF
  420     CONTINUE

!  reorder the nonlinear constraints so that the equations occur before the
!  inequalities

  430     CONTINUE
          mend = m
          DO 450 i = data%mlin + 1, m
            IF ( i > mend ) GO TO 700
            ig = data%CGROUP( i )

!  constraint i is an inequality. Now, run backwards through the constraints
!  until an equation is encountered

            IF ( .NOT. EQUATN( i ) ) THEN
              DO j = mend, i, - 1
                jg = data%CGROUP( j )
                IF ( EQUATN( j ) ) THEN
                  mend = j - 1

!  interchange the data for constraints i and j

                  data%CGROUP( i ) = jg
                  data%CGROUP( j ) = ig
                  data%KNDOFC( ig ) = j
                  data%KNDOFC( jg ) = i
                  ltemp = LINEAR( i )
                  LINEAR( i ) = LINEAR( j )
                  LINEAR( j ) = ltemp
                  ltemp = EQUATN( i )
                  EQUATN( i ) = EQUATN( j )
                  EQUATN( j ) = ltemp
                  atemp = Y( i ) ; Y( i ) = Y( j ) ; Y( j ) = atemp
                  atemp = C_l( i ) ; C_l( i ) = C_l( j ) ; C_l( j ) = atemp
                  atemp = C_u( i ) ; C_u( i ) = C_u( j ) ; C_u( j ) = atemp
                  GO TO 450
                END IF
              END DO
              GO TO 700
            END IF
  450     CONTINUE

!  reorder the linear constraints so that the inequalities occur before the
!  equations

        ELSE IF ( e_order == 2 ) THEN
          mend = data%mlin
          DO 520 i = 1, data%mlin
            IF ( i > mend ) GO TO 530
            ig = data%CGROUP( i )

!  constraint i is an equation. Now, run backwards through the constraints
!  until an inequality is encountered

            IF ( EQUATN( i ) ) THEN
              DO j = mend, i, - 1
                jg = data%CGROUP( j )
                IF ( .NOT. EQUATN( j ) ) THEN
                  mend = j - 1

!  interchange the data for constraints i and j

                  data%CGROUP( i ) = jg
                  data%CGROUP( j ) = ig
                  data%KNDOFC( ig ) = j
                  data%KNDOFC( jg ) = i
                  ltemp = LINEAR( i )
                  LINEAR( i ) = LINEAR( j )
                  LINEAR( j ) = ltemp
                  ltemp = EQUATN( i )
                  EQUATN( i ) = EQUATN( j )
                  EQUATN( j ) = ltemp
                  atemp = Y( i ) ; Y( i ) = Y( j ) ; Y( j ) = atemp
                  atemp = C_l( i ) ; C_l( i ) = C_l( j ) ; C_l( j ) = atemp
                  atemp = C_u( i ) ; C_u( i ) = C_u( j ) ; C_u( j ) = atemp
                  GO TO 520
                END IF
              END DO
              GO TO 530
            END IF
  520     CONTINUE

!  reorder the nonlinear constraints so that the inequalties occur before the
!  equations

  530     CONTINUE
          mend = m
          DO 550 i = data%mlin + 1, m
            IF ( i > mend ) GO TO 700
            ig = data%CGROUP( i )

!  constraint i is an equation. Now, run backwards through the constraints
!  until an inequality is encountered

            IF ( EQUATN( i ) ) THEN
              DO j = mend, i, - 1
                jg = data%CGROUP( j )
                IF ( .NOT. EQUATN( j ) ) THEN
                  mend = j - 1

!  interchange the data for constraints i and j

                  data%CGROUP( i ) = jg
                  data%CGROUP( j ) = ig
                  data%KNDOFC( ig ) = j
                  data%KNDOFC( jg ) = i
                  ltemp = LINEAR( i )
                  LINEAR( i ) = LINEAR( j )
                  LINEAR( j ) = ltemp
                  ltemp = EQUATN( i )
                  EQUATN( i ) = EQUATN( j )
                  EQUATN( j ) = ltemp
                  atemp = Y( i ) ; Y( i ) = Y( j ) ; Y( j ) = atemp
                  atemp = C_l( i ) ; C_l( i ) = C_l( j ) ; C_l( j ) = atemp
                  atemp = C_u( i ) ; C_u( i ) = C_u( j ) ; C_u( j ) = atemp
                  GO TO 550
                END IF
              END DO
              GO TO 700
            END IF
  550     CONTINUE
        END IF
      ELSE
        IF ( data%meq == 0 .OR. data%meq == m ) GO TO 700

!  reorder the constraints so that the equations occur before the inequalities

        IF ( e_order == 1 ) THEN
          mend = m
          DO 620 i = 1, m
            IF ( i > mend ) GO TO 700
            ig = data%CGROUP( i )

!  constraint i is an inequality. Now, run backwards through the constraints
!  until an equation is encountered

            IF ( .NOT. EQUATN( i ) ) THEN
              DO j = mend, i, - 1
                jg = data%CGROUP( j )
                IF ( EQUATN( j ) ) THEN
                  mend = j - 1

!  interchange the data for constraints i and j

                  data%CGROUP( i ) = jg
                  data%CGROUP( j ) = ig
                  data%KNDOFC( ig ) = j
                  data%KNDOFC( jg ) = i
                  ltemp = LINEAR( i )
                  LINEAR( i ) = LINEAR( j )
                  LINEAR( j ) = ltemp
                  ltemp = EQUATN( i )
                  EQUATN( i ) = EQUATN( j )
                  EQUATN( j ) = ltemp
                  atemp = Y( i ) ; Y( i ) = Y( j ) ; Y( j ) = atemp
                  atemp = C_l( i ) ; C_l( i ) = C_l( j ) ; C_l( j ) = atemp
                  atemp = C_u( i ) ; C_u( i ) = C_u( j ) ; C_u( j ) = atemp
                  GO TO 620
                END IF
              END DO
              GO TO 700
            END IF
  620     CONTINUE

!  reorder the constraints so that the inequalities occur before the equations

        ELSE IF ( e_order == 2 ) THEN
          mend = m
          DO 650 i = 1, m
            IF ( i > mend ) GO TO 700
            ig = data%CGROUP( i )

!  constraint i is an equation. Now, run backwards through the constraints
!  until an inequality is encountered

            IF ( EQUATN( i ) ) THEN
              DO j = mend, i, - 1
                jg = data%CGROUP( j )
                IF ( .NOT. EQUATN( j ) ) THEN
                  mend = j - 1

!  interchange the data for constraints i and j

                  data%CGROUP( i ) = jg
                  data%CGROUP( j ) = ig
                  data%KNDOFC( ig ) = j
                  data%KNDOFC( jg ) = i
                  ltemp = LINEAR( i )
                  LINEAR( i ) = LINEAR( j )
                  LINEAR( j ) = ltemp
                  ltemp = EQUATN( i )
                  EQUATN( i ) = EQUATN( j )
                  EQUATN( j ) = ltemp
                  atemp = Y( i ) ; Y( i ) = Y( j ) ; Y( j ) = atemp
                  atemp = C_l( i ) ; C_l( i ) = C_l( j ) ; C_l( j ) = atemp
                  atemp = C_u( i ) ; C_u( i ) = C_u( j ) ; C_u( j ) = atemp
                  GO TO 650
                END IF
              END DO
              GO TO 700
            END IF
  650     CONTINUE
        END IF
      END IF

!  initialize the performance counters and variables

  700 CONTINUE
      work%nc2of = 0 ; work%nc2og = 0 ; work%nc2oh = 0
      work%nc2cf = 0 ; work%nc2cg = 0 ; work%nc2ch = 0 ; work%nhvpr = 0
      work%njvpr = 0 ; work%pnc = m

      CALL CPU_TIME( data%st_cpu_time )
      data%su_cpu_time = data%st_cpu_time - data%su_cpu_time
      CALL SYSTEM_CLOCK( count = count, count_rate = count_rate )
      data%st_time = REAL( count, KIND = sp ) / REAL( count_rate, KIND = sp )
      data%su_time = data%st_time - data%su_time

      status = 0
      RETURN

  910 CONTINUE
      status = 1
      IF ( out > 0 ) WRITE( out,                                               &
        "( /, ' ** SUBROUTINE CUTEST_csetup: allocation error for ', A,        &
       &   ' status = ', I0, /, ' Execution terminating ' )" )                 &
         bad_alloc, alloc_status
      RETURN

!  non-executable statements

 1010 FORMAT( ( 10I8 ) )
 1020 FORMAT( ( 1P, 4D16.8 ) )
 1030 FORMAT( ( 72L1 ) )
 1040 FORMAT( ( 8A10 ) )
 1080 FORMAT( 1P, 2D16.8 )
 1100 FORMAT( A10, 3I10 )
 1110 FORMAT( 1X, A6, /, ( 1X, 10I8 ) )
 1120 FORMAT( 1X, A6, /, ( 1X, 1P, 4D16.8 ) )
 1130 FORMAT( 1X, A6, /, ( 1X, 72L1 ) )
 1140 FORMAT( 1X, A6, /, ( 1X, 8A10 ) )
 1180 FORMAT( 1X, A6, /, 1P, 2D16.8 )
 2000 FORMAT( /, ' ** SUBROUTINE CUTEST_csetup: array length ', A,             &
                 ' too small.', /, ' -- Increase the dimension to at least ',  &
                 I0, ' and restart.' )

!  End of subroutine CUTEST_csetup_threadsafe_r

      END SUBROUTINE CUTEST_csetup_threadsafe_r
