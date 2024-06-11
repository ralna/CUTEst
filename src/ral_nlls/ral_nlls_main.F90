! THIS VERSION: CUTEST 2.3 - 2024-06-11 AT 11:40 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"
#ifdef REAL_32
#define RAL_NLLS_precision RAL_NLLS_single
#else
#define RAL_NLLS_precision RAL_NLLS_double
#endif

      PROGRAM RAL_NLLS_main

!  RAL_NLLS test driver for problems derived from SIF files

!  Nick Gould, October 2015

      USE CUTEST_KINDS_precision
      USE RAL_NLLS_precision

      IMPLICIT NONE

      TYPE, EXTENDS( params_base_type ) :: user_type
         ! still empty
      END TYPE user_type

      INTEGER ( KIND = ip_ ) :: status, i, m, n
      REAL( KIND = rpc_ ), DIMENSION( : ), ALLOCATABLE :: X, X_l, X_u
      REAL( KIND = rpc_ ), DIMENSION( : ), ALLOCATABLE :: Y, C_l, C_u, F
      TYPE( user_type ), target :: params
      TYPE( Nlls_inform ) :: inform
      TYPE( Nlls_options ) :: control
      LOGICAL, DIMENSION( : ), ALLOCATABLE  :: EQUATN, LINEAR
      CHARACTER ( LEN = 10 ) :: pname
      CHARACTER ( LEN = 20 ) :: summary_file = REPEAT( ' ', 20 )
      CHARACTER ( LEN = 20 ) :: iter_summary_file = REPEAT( ' ', 20 )
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: VNAMES, CNAMES
      REAL( KIND = rpc_ ), DIMENSION( 4 ) :: CPU
      REAL( KIND = rpc_ ), DIMENSION( 7 ) :: CALLS
      INTEGER ( KIND = ip_ ) :: io_buffer = 11
      INTEGER ( KIND = ip_ ) :: summary_unit, iter_summary_unit, iores
      INTEGER ( KIND = ip_ ), PARAMETER :: input = 55, indr = 46, out = 6
      LOGICAL :: filexx
      INTEGER ( KIND = ip_ ) :: fnevals, jacevals, hessevals, localiter

!  open the relevant file

      OPEN( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED', STATUS = 'OLD' )
      REWIND( input )

!  compute problem dimensions

      CALL CUTEST_cdimen_r( status, input, n, m )
      IF ( status /= 0 ) GO TO 910

!  allocate space

      ALLOCATE( X( n ), X_l( n ), X_u( n ), Y( m ), C_l( m ), C_u( m ),        &
                EQUATN( m ), LINEAR( m ), STAT = status )
      IF ( status /= 0 ) GO TO 990

!  initialize problem data structure

!  set up the data structures necessary to hold the problem functions.

      CALL CUTEST_csetup_r( status, input, out, io_buffer, n, m,               &
                            X, X_l, X_u, Y, C_l, C_u, EQUATN, LINEAR, 0, 0, 0 )
      IF ( status /= 0 ) GO TO 910
      CLOSE( input )

!  allocate more space

      DEALLOCATE( X_l, X_u, Y, C_l, C_u, EQUATN, LINEAR )

!  open the Spec file for the method

      OPEN( indr, FILE = 'RAL_NLLS.SPC', FORM = 'FORMATTED', STATUS = 'OLD')
      REWIND( indr )

!  set up algorithmic input data

!  error = unit for error messages
!  out = normal output unit for information
!  print_level = controls the amount of printing - set to 0-4, (<=0 = nothing)
!  nlls_method = method used (1=dogleg, 2=AINT, 3=More-Sorensen, 4=gltr)
!  model = model used (1=first order, 2=Newton)
!  maxit = maximum numnber of iterations permitted
!  tr_update_strategy = TR update strategy
!  relative_tr_radius = relative trust region? (1=relative)
!  initial_radius_scale = TR scaling parameter
!  initial_radius = initial TR radius
!  radius_increase = radius increase factor
!  radius_reduce = radius reduction factor
!  stop_g_absolute = absolute stopping tolerance
!  stop_g_relative = relative stopping tolerance
!  hybrid_tol = hybrid_tol
!  hybrid_switch_its = hybrid_switch_its
!  output_progress_vectors = output_progress_vectors
!  exact_second_derivatives = exact second derivatives
!  summary_unit = summary unit (not + = don't write)
!  summary_file = summary file (max 20 chars)
!  iter_summary_unit = iteration summary (not + = don't write)
!  iter_summary_file = iteration summary file (max 20 chars)

      READ( indr, "( I6, 7( /, I6 ), 7( /, E12.0 ), /, I6,                     &
     &               2 ( /, L20 ), /, I6, /, A, /, I6, /, A ) ")               &
           control%error,                                                      &
           control%out,                                                        &
           control%print_level,                                                &
           control%nlls_method,                                                &
           control%model,                                                      &
           control%maxit,                                                      &
           control%tr_update_strategy,                                         &
           control%relative_tr_radius,                                         &
           control%initial_radius_scale,                                       &
           control%initial_radius,                                             &
           control%radius_increase,                                            &
           control%radius_reduce,                                              &
           control%stop_g_absolute,                                            &
           control%stop_g_relative,                                            &
           control%hybrid_tol,                                                 &
           control%hybrid_switch_its,                                          &
           control%output_progress_vectors,                                    &
           control%exact_second_derivatives,                                   &
           summary_unit,                                                       &
           summary_file,                                                       &
           iter_summary_unit,                                                  &
           iter_summary_file
      CLOSE ( indr )

!write(6,*) summary_unit, summary_file
!write(6,*) iter_summary_unit, iter_summary_file
!write(6,*) iter_summary_unit, iter_summary_file

      IF ( summary_unit > 0 ) THEN
        INQUIRE( FILE = summary_file, EXIST = filexx )
        IF ( filexx ) THEN
           OPEN( summary_unit, FILE = summary_file, FORM = 'FORMATTED',        &
               STATUS = 'OLD', IOSTAT = iores , position="append")
        ELSE
           OPEN( summary_unit, FILE = summary_file, FORM = 'FORMATTED',        &
                STATUS = 'NEW', IOSTAT = iores )
        END IF
        IF ( iores /= 0 ) THEN
          write( out, "( ' IOSTAT = ', I0, ' when opening file ', A,           &
        &  '. Stopping ' )" ) iores, summary_file
          STOP
        END IF
        CALL CUTEST_probname_r( status, pname )
        WRITE( summary_unit, "( A10 )" ) pname
      END IF

      IF ( iter_summary_unit > 0 .and. control%output_progress_vectors ) THEN
        INQUIRE( FILE = iter_summary_file, EXIST = filexx )
        IF ( filexx ) THEN
           OPEN( iter_summary_unit, FILE=iter_summary_file, FORM='FORMATTED',  &
               STATUS = 'OLD', IOSTAT = iores , position="append")
        ELSE
           OPEN( iter_summary_unit, FILE=iter_summary_file, FORM='FORMATTED',  &
                STATUS = 'NEW', IOSTAT = iores )
        END IF
        IF ( iores /= 0 ) THEN
          write( out, "( ' IOSTAT = ', I0, ' when opening file ', A,           &
        &  '. Stopping ' )" ) iores, iter_summary_file
          STOP
        END IF
        CALL CUTEST_probname_r( status, pname )
        WRITE( iter_summary_unit, "( A10 )" ) pname
      END IF

      write(*,*) 'calling the minimizer...'

!  call the minimizer

      CALL NLLS_SOLVE( n, m, X, eval_F, eval_J, eval_HF,                       &
                       params, control, inform )

      WRITE( out , "( A, I0, A, I0)") 'status = ', inform%status,              &
          '       iter = ', inform%iter
      IF ( status /= 0 ) GO TO 910

!  output report

      CALL CUTEST_creport_r( status, CALLS, CPU )
      IF ( status /= 0 ) GO TO 910

      ALLOCATE( F( m ), VNAMES( n ), CNAMES( m ), STAT = status )
      CALL CUTEST_cnames_r( status, n, m, pname, VNAMES, CNAMES )
      CALL eval_F( status, n, m, X, F, params)

      WRITE( out, "( /, ' The variables:', /,                                  &
     &     '     i name          value',  /, ( I6, 1X, A10, 1P, D12.4 ) )" )   &
         ( i, VNAMES( i ), X( i ), i = 1, n )

!     WRITE( out, "( /, ' The constraints:', /, '     i name          value',  &
!   &     /, ( I6, 1X, A10, 1P, D12.4 ) )" )                                   &
!        ( i, CNAMES( i ), F( i ), i = 1, m )
      WRITE( out, "( /, 24('*'), ' CUTEst statistics ', 24('*') //,            &
     &    ' Package used            :  RAL_NLLS ',  /,                         &
     &    ' Problem                 :  ', A10,    /,                           &
     &    ' # variables             =  ', I0, /,                               &
     &    ' # residuals             =  ', I0, /,                               &
     &    ' Final f                 =', ES15.7 /,                              &
     &    ' # residual evaluations  =  ', I0, /,                               &
     &    ' # Jacobian evaluations  =  ', I0, /,                               &
     &    ' # Hessian evaluations   =  ', I0, /,                               &
     &    ' Set up time             =  ', 0P, F0.2, ' seconds' /,              &
     &    ' Solve time              =  ', 0P, F0.2, ' seconds' //,             &
     &     66('*') / )" ) pname, n, m, inform%obj, INT( CALLS( 5 ) ),          &
        INT( CALLS( 6 ) ), INT( CALLS( 7 ) ), CPU( 1 ), CPU( 2 )

!  write summary if required

      localiter = inform%iter
      fnevals = int( calls(5) )
      jacevals = int( calls(6) )
      hessevals = int( calls(7) )

      IF ( inform%status .ne. 0 ) THEN
         localiter = - localiter
         fnevals = - fnevals
         jacevals = - jacevals
         hessevals = - hessevals
      END IF

      IF ( summary_unit > 0 ) THEN
        BACKSPACE( summary_unit )
        WRITE( summary_unit, "( A10, 7I6, ES23.15E3, ES23.15E3, ES23.15E3 )" ) &
          pname, n, m, inform%status,localiter,                                &
          fnevals, jacevals, hessevals,                                        &
          inform%obj, inform%norm_g, inform%scaled_g
        CLOSE(  summary_unit )
      END IF

      IF ( iter_summary_unit > 0 .AND. control%output_progress_vectors .AND.   &
           ALLOCATED( inform%resvec ) .AND. ALLOCATED( inform%gradvec ) ) THEN
        BACKSPACE( iter_summary_unit )
        DO i = 1,inform%iter + 1
           WRITE( iter_summary_unit, "( ES23.15E3, ES23.15E3 )" )              &
                inform%resvec(i), inform%gradvec(i)
        END DO
        CLOSE( iter_summary_unit )
      END IF

!  clean-up data structures

      DEALLOCATE( X, F, VNAMES, CNAMES, STAT = status )
      IF ( status /= 0 ) GO TO 910
      CALL CUTEST_cterminate_r( status )
      STOP

!  error returns

  910 CONTINUE
      WRITE( 6, "( ' CUTEst error, status = ', i0, ', stopping' )") status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP

!  Non-executable statements

!  End of RAL_NLLS_main

    CONTAINS

      SUBROUTINE eval_F( status, n, m, X, f, params )
      USE CUTEST_KINDS_precision
      USE RAL_NLLS_precision, ONLY : params_base_type

      INTEGER ( KIND = ip_), INTENT( out ) :: status
      INTEGER ( KIND = ip_), INTENT( in ) :: n, m
      REAL ( KIND = rpc_ ), dimension(*), intent(in)  :: X
      REAL ( KIND = rpc_ ), dimension(*), intent(out) :: f
      CLASS ( params_base_type ), intent(in) :: params
      REAL ( KIND = rpc_ ) :: obj

!  evaluate the residuals F

      CALL CUTEST_cfn_r( status, n, m, X, obj, f )
      END SUBROUTINE eval_F

      SUBROUTINE eval_J( status, n, m, X, J, params)
      USE CUTEST_KINDS_precision
      USE RAL_NLLS_precision, ONLY : params_base_type

      INTEGER ( KIND = ipc_), INTENT( OUT ) :: status
      INTEGER ( KIND = ipc_ ), INTENT( IN ) :: n, m
      REAL ( KIND = rpc_ ), DIMENSION( * ), INTENT( IN ) :: X
      REAL ( KIND = rpc_ ), DIMENSION( * ), INTENT( OUT ) :: J
      CLASS ( params_base_type ), intent(in) :: params
      REAL ( KIND = rpc_ ), DIMENSION( n ) :: G
      REAL ( KIND = rpc_ ), DIMENSION( m ) :: Y
      REAL ( KIND = rpc_ ), DIMENSION( m , n ) :: Jmatrix

!  evaluate the residual Jacobian J

      CALL CUTEST_cgr_r( status, n, m, X, Y, .FALSE., G, .FALSE.,              &
                         m, n, Jmatrix )
      ! convert the Jacobian to a vector....
      J( 1 : m * n ) = RESHAPE( Jmatrix, (/ n * m /) )
      RETURN
      END SUBROUTINE eval_J

      SUBROUTINE eval_HF( status, n, m, X, F, H, params )
      USE CUTEST_KINDS_precision
      USE RAL_NLLS_precision, ONLY : params_base_type

      INTEGER ( KIND = ipc_ ), INTENT( OUT ) :: status
      INTEGER ( KIND = ipc_ ), INTENT( IN ) :: n, m
      REAL ( KIND = rpc_ ), DIMENSION( * ), INTENT( IN ) :: X
      REAL ( KIND = rpc_ ), DIMENSION( * ), INTENT( IN ) :: F
      REAL ( KIND = rpc_ ), DIMENSION( * ), INTENT( OUT ) :: H
      CLASS ( params_base_type ), INTENT( IN ) :: params
      REAL (  KIND = rpc_ ), DIMENSION( n, n ) :: Hmatrix

!  evaluate the product H = sum F_i Hessian F_i

      CALL CUTEST_cdhc_r( status, n, m, X, F, n, Hmatrix )
      H( 1 : n * n ) = RESHAPE( Hmatrix, (/ n * n /) )
      RETURN
      END SUBROUTINE eval_HF

    END PROGRAM RAL_NLLS_main





