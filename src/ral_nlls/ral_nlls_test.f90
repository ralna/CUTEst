!  Dummy RAL_NLLS for testing ral_nlls_main interface to CUTEst
!  Nick Gould, 6th October 2015

  MODULE ral_nlls_internal

  USE iso_c_binding

  implicit none

  private

  INTEGER, PARAMETER :: wp = KIND( 1.0d0 )
  INTEGER, PARAMETER :: long = SELECTED_INT_KIND( 8 )

  real (kind = wp), parameter :: tenm3 = 1.0e-3
  real (kind = wp), parameter :: tenm5 = 1.0e-5
  real (kind = wp), parameter :: tenm8 = 1.0e-8
  real (kind = wp), parameter :: epsmch = epsilon(1.0_wp)
  real (kind = wp), parameter :: hundred = 100.0
  real (kind = wp), parameter :: ten = 10.0
  real (kind = wp), parameter :: point9 = 0.9
  real (kind = wp), parameter :: zero = 0.0
  real (kind = wp), parameter :: one = 1.0
  real (kind = wp), parameter :: two = 2.0
  real (kind = wp), parameter :: half = 0.5
  real (kind = wp), parameter :: sixteenth = 0.0625

  TYPE, public :: Nlls_options
     INTEGER :: error = 6
     INTEGER :: out = 6
     INTEGER :: print_level = 0
     INTEGER :: maxit = 100
     INTEGER :: model = 1
     INTEGER :: nlls_method = 1
     INTEGER :: lls_solver = 1
     REAL ( KIND = wp ) :: stop_g_absolute = tenm5
     REAL ( KIND = wp ) :: stop_g_relative = tenm8
     INTEGER :: relative_tr_radius = 0
     REAL ( KIND = wp ) :: initial_radius_scale = 1.0!tenm3
     REAL ( KIND = wp ) :: initial_radius = hundred
     REAL ( KIND = wp ) :: maximum_radius = ten ** 8
     REAL ( KIND = wp ) :: eta_successful = ten ** ( - 8 )
     REAL ( KIND = wp ) :: eta_very_successful = point9
     REAL ( KIND = wp ) :: eta_too_successful = two
     REAL ( KIND = wp ) :: radius_increase = two
     REAL ( KIND = wp ) :: radius_reduce = half
     REAL ( KIND = wp ) :: radius_reduce_max = sixteenth
     integer :: tr_update_strategy = 1
     real ( kind = wp ) :: hybrid_switch = 0.1_wp
     logical :: exact_second_derivatives = .true.
     LOGICAL :: subproblem_eig_fact = .FALSE.
     integer  :: more_sorensen_maxits = 500
     real(wp) :: more_sorensen_shift = 1e-8
     real(wp) :: more_sorensen_tiny = 10.0 * epsmch
     real(wp) :: more_sorensen_tol = 1e-6

     real(wp) :: hybrid_tol = 0.02
     integer :: hybrid_switch_its = 3

     logical :: output_progress_vectors = .false.

  END TYPE Nlls_options

  TYPE, public :: NLLS_inform
     INTEGER :: status = 0
     INTEGER :: alloc_status = 0
     INTEGER :: iter = 0
     INTEGER :: f_eval = 0
     INTEGER :: g_eval = 0
     INTEGER :: h_eval = 0
     integer :: convergence_normf = 0
     integer :: convergence_normg = 0
     real(wp), allocatable :: resvec(:)
     real(wp), allocatable :: gradvec(:)
     REAL ( KIND = wp ) :: obj = HUGE( one )
     REAL ( KIND = wp ) :: norm_g = HUGE( one )
     REAL ( KIND = wp ) :: scaled_g = HUGE( one )
     INTEGER :: external_return = 0
     character ( len = 80 ) :: external_name = REPEAT( ' ', 80 )
!      REAL( c_double ) :: obj = HUGE( 1.0_c_double )
  END TYPE NLLS_inform

  type, public :: params_base_type
     ! deliberately empty
  end type params_base_type

  abstract interface
     subroutine eval_f_type(status, n, m, x, f, params)
       USE ISO_C_BINDING
       import :: params_base_type
       implicit none
       integer, intent(out) :: status
       integer, intent(in) :: n,m
       REAL ( c_double ), dimension(*), intent(in)  :: x
       REAL ( c_double ), dimension(*), intent(out) :: f
       class(params_base_type), intent(in) :: params
     end subroutine eval_f_type
  end interface

  abstract interface
     subroutine eval_j_type(status, n, m, x, J, params)
       USE ISO_C_BINDING
       import :: params_base_type
       implicit none
       integer, intent(out) :: status
       integer, intent(in) :: n,m
       REAL ( c_double ), dimension(*), intent(in)  :: x
       REAL ( c_double ), dimension(*), intent(out) :: J
       class(params_base_type), intent(in) :: params
     end subroutine eval_j_type
  end interface

  abstract interface
     subroutine eval_hf_type(status, n, m, x, f, h, params)
       USE ISO_C_BINDING
       import :: params_base_type
       implicit none
       integer, intent(out) :: status
       integer, intent(in) :: n,m
       REAL ( c_double ), dimension(*), intent(in)  :: x
       REAL ( c_double ), dimension(*), intent(in)  :: f
       REAL ( c_double ), dimension(*), intent(out) :: h
       class(params_base_type), intent(in) :: params
     end subroutine eval_hf_type
  end interface


contains

     ! nothing!

   END MODULE ral_nlls_internal

   MODULE RAL_NLLS_DOUBLE

     USE ISO_C_BINDING
     use ral_nlls_internal

     IMPLICIT none

     private

!!$     INTEGER, PARAMETER :: wp = KIND( 1.0d0 )
!!$     INTEGER, PARAMETER :: long = SELECTED_INT_KIND( 8 )
!!$
     INTEGER, PARAMETER :: error_dimensions = - 1
     INTEGER, PARAMETER :: error_workspace = - 2
     INTEGER, PARAMETER :: error_eval_F = - 3
     INTEGER, PARAMETER :: error_eval_J = - 4
     INTEGER, PARAMETER :: error_eval_HF = - 5
!!$
!!$     real (kind = wp), parameter :: tenm3 = 1.0e-3
!!$     real (kind = wp), parameter :: tenm5 = 1.0e-5
!!$     real (kind = wp), parameter :: tenm8 = 1.0e-8
!!$     real (kind = wp), parameter :: epsmch = epsilon(1.0_wp)
!!$     real (kind = wp), parameter :: hundred = 100.0
!!$     real (kind = wp), parameter :: ten = 10.0
!!$     real (kind = wp), parameter :: point9 = 0.9
!!$     real (kind = wp), parameter :: zero = 0.0
!!$     real (kind = wp), parameter :: one = 1.0
!!$     real (kind = wp), parameter :: two = 2.0
!!$     real (kind = wp), parameter :: half = 0.5
!!$     real (kind = wp), parameter :: sixteenth = 0.0625


  ABSTRACT INTERFACE
     SUBROUTINE eval_F_type( status, n, m, X, F , params )
       USE ISO_C_BINDING
       import :: params_base_type
       implicit none
       INTEGER, INTENT( OUT ) :: status
       INTEGER, INTENT( IN ) :: n, m
       REAL ( c_double ), DIMENSION( * ), INTENT( IN ) :: X
       REAL ( c_double ), DIMENSION( * ), INTENT( OUT ) :: F
       class( params_base_type ), intent( in ) :: params
     END SUBROUTINE eval_F_type
  END INTERFACE

  ABSTRACT INTERFACE
     SUBROUTINE eval_j_type( status, n, m, X, J, params )
       USE ISO_C_BINDING
       import :: params_base_type
       INTEGER ( c_int ), INTENT( OUT ) :: status
       INTEGER ( c_int ), INTENT( IN ) :: n, m
       REAL ( c_double ), DIMENSION( * ), INTENT( IN ) :: X
       REAL ( c_double ), DIMENSION( * ), INTENT( OUT ) :: J
       class( params_base_type ), intent( in ) :: params
     END SUBROUTINE eval_j_type
  END INTERFACE

  ABSTRACT INTERFACE
     SUBROUTINE eval_HF_type( status, n, m, X, F, H, params )
       USE ISO_C_BINDING
       import :: params_base_type
       INTEGER ( c_int ), INTENT( OUT ) :: status
       INTEGER ( c_int ), INTENT( IN ) :: n, m
       REAL ( c_double ), DIMENSION( * ), INTENT( IN ) :: X
       REAL ( c_double ), DIMENSION( * ), INTENT( IN ) :: F
       REAL ( c_double ), DIMENSION( * ), INTENT( OUT ) :: H
       class( params_base_type ), intent( in ) :: params
     END SUBROUTINE eval_HF_type
  END INTERFACE

  public :: nlls_solve
  public :: nlls_options, nlls_inform
  public :: params_base_type


CONTAINS

     SUBROUTINE NLLS_SOLVE( n, m, X, eval_F, eval_J, eval_HF,                  &
                            params, options, inform )

!  -----------------------------------------------------------------------------
!  RAL_NLLS, a fortran subroutine for finding a first-order critical
!   point (most likely, a local minimizer) of the nonlinear least-squares
!   objective function 1/2 ||F(x)||_2^2.

!  Authors: RAL NA Group (Iain Duff, Nick Gould, Jonathan Hogg, Tyrone Rees,
!                         Jennifer Scott)
!  -----------------------------------------------------------------------------

!   Dummy arguments

     INTEGER( c_int ), INTENT( IN ) :: n, m
     REAL( c_double ), DIMENSION( n ), INTENT( INOUT ) :: X
     TYPE( Nlls_inform ), INTENT( OUT ) :: inform
     TYPE( Nlls_options ), INTENT( IN ) :: options
     class( params_base_type ) :: params
     procedure( eval_f_type ) :: eval_F
     procedure( eval_j_type ) :: eval_J
     procedure( eval_hf_type ) :: eval_HF

!  Interface blocks

!  Local variables

     INTEGER :: status, start_f, end_f, start_j, start_h, w_end
!    INTEGER :: len_work_int, len_work_real
 !   INTEGER( c_int ), allocatable :: Work_int( : )
     REAL( c_double ), allocatable :: Work_real( : )

!  check input dimensions

     IF ( m <= 0 .OR. n <= 0 ) THEN
       status = error_dimensions
       GO TO 990
     END IF

     start_f = 1
     start_j = start_f + m
     end_f = start_j - 1
     start_h = start_j + n * m
     w_end = start_h + n * n - 1

!  partition the workspace
!    allocate(Work_int(10))
     allocate(Work_real(w_end))

!  evaluate F

     CALL eval_F( status, n, m, X, WORK_real( start_f ), params )
     IF ( status /= 0 ) THEN
       status = error_eval_F
       GO TO 990
     END IF
     inform%obj = 0.5_c_double * DOT_PRODUCT( WORK_real( start_f : end_f ),    &
                                              WORK_real( start_f : end_f ) )

!  evaluate J

     CALL eval_J( status, n, m, X, WORK_real( start_j ), params )
     IF ( status /= 0 ) THEN
       status = error_eval_J
       GO TO 990
     END IF

!  evaluate HF

     CALL eval_HF( status, n, m, X, WORK_real( start_f ),                      &
                   WORK_real( start_h ), params )
     IF ( status /= 0 ) THEN
       status = error_eval_HF
       GO TO 990
     END IF

 990 CONTINUE
     inform%status = status
     RETURN
   END SUBROUTINE NLLS_SOLVE

   END MODULE RAL_NLLS_DOUBLE


