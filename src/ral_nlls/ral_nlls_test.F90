! THIS VERSION: CUTEST 2.3 - 2024-06-11 AT 11:40 GMT.

#include "cutest_modules.h"
#ifdef REAL_32
#define RAL_NLLS_precision RAL_NLLS_single
#else
#define RAL_NLLS_precision RAL_NLLS_double
#endif

!  Dummy RAL_NLLS for testing ral_nlls_main interface to CUTEst
!  Nick Gould, 6th October 2015

  MODULE RAL_NLLS_internal

    USE CUTEST_KINDS_precision

    IMPLICIT NONE

    PRIVATE

    INTEGER, PARAMETER :: long = SELECTED_INT_KIND( 8 )

    REAL ( KIND = rp_ ), PARAMETER :: zero = 0.0_rp_
    REAL ( KIND = rp_ ), PARAMETER :: one = 1.0_rp_
    REAL ( KIND = rp_ ), PARAMETER :: two = 2.0_rp_
    REAL ( KIND = rp_ ), PARAMETER :: half = 0.5_rp_
    REAL ( KIND = rp_ ), PARAMETER :: sixteenth = 0.0625_rp_
    REAL ( KIND = rp_ ), PARAMETER :: point9 = 0.9_rp_
    REAL ( KIND = rp_ ), PARAMETER :: ten = 10.0_rp_
    REAL ( KIND = rp_ ), PARAMETER :: tenm3 = ten ** ( - 3 )
    REAL ( KIND = rp_ ), PARAMETER :: tenm5 = ten ** ( - 5 )
    REAL ( KIND = rp_ ), PARAMETER :: tenm8 = ten ** ( - 8 )
    REAL ( KIND = rp_ ), PARAMETER :: epsmch = EPSILON( one )
    REAL ( KIND = rp_ ), PARAMETER :: hundred = 100.0_rp_

    TYPE, public :: Nlls_options
       INTEGER ( KIND = ip_ ) :: error = 6
       INTEGER ( KIND = ip_ ) :: out = 6
       INTEGER ( KIND = ip_ ) :: print_level = 0
       INTEGER ( KIND = ip_ ) :: maxit = 100
       INTEGER ( KIND = ip_ ) :: model = 1
       INTEGER ( KIND = ip_ ) :: nlls_method = 1
       INTEGER ( KIND = ip_ ) :: lls_solver = 1
       REAL ( KIND = rp_ ) :: stop_g_absolute = tenm5
       REAL ( KIND = rp_ ) :: stop_g_relative = tenm8
       INTEGER ( KIND = ip_ ) :: relative_tr_radius = 0
       REAL ( KIND = rp_ ) :: initial_radius_scale = 1.0!tenm3
       REAL ( KIND = rp_ ) :: initial_radius = hundred
       REAL ( KIND = rp_ ) :: maximum_radius = ten ** 8
       REAL ( KIND = rp_ ) :: eta_successful = ten ** ( - 8 )
       REAL ( KIND = rp_ ) :: eta_very_successful = point9
       REAL ( KIND = rp_ ) :: eta_too_successful = two
       REAL ( KIND = rp_ ) :: radius_increase = two
       REAL ( KIND = rp_ ) :: radius_reduce = half
       REAL ( KIND = rp_ ) :: radius_reduce_max = sixteenth
       INTEGER ( KIND = ip_ ) :: tr_update_strategy = 1
       REAL ( KIND = rp_ ) :: hybrid_switch = 0.1_rp_
       logical :: exact_second_derivatives = .true.
       LOGICAL :: subproblem_eig_fact = .FALSE.
       INTEGER ( KIND = ip_ )  :: more_sorensen_maxits = 500
       REAL ( KIND = rp_) :: more_sorensen_shift = ten ** ( - 8 )
       REAL ( KIND = rp_) :: more_sorensen_tiny = ten * epsmch
       REAL ( KIND = rp_) :: more_sorensen_tol = ten ** ( - 6 )

       REAL ( KIND = rp_) :: hybrid_tol = 0.02_rp_
       INTEGER ( KIND = ip_ ) :: hybrid_switch_its = 3

       logical :: output_progress_vectors = .false.

    END TYPE Nlls_options

    TYPE, public :: NLLS_inform
       INTEGER ( KIND = ip_ ) :: status = 0
       INTEGER ( KIND = ip_ ) :: alloc_status = 0
       INTEGER ( KIND = ip_ ) :: iter = 0
       INTEGER ( KIND = ip_ ) :: f_eval = 0
       INTEGER ( KIND = ip_ ) :: g_eval = 0
       INTEGER ( KIND = ip_ ) :: h_eval = 0
       INTEGER ( KIND = ip_ ) :: convergence_normf = 0
       INTEGER ( KIND = ip_ ) :: convergence_normg = 0
       REAL( KIND = rp_ ), allocatable :: resvec(:)
       REAL( KIND = rp_ ), allocatable :: gradvec(:)
       REAL ( KIND = rp_ ) :: obj = HUGE( one )
       REAL ( KIND = rp_ ) :: norm_g = HUGE( one )
       REAL ( KIND = rp_ ) :: scaled_g = HUGE( one )
       INTEGER ( KIND = ip_ ) :: external_return = 0
       character ( len = 80 ) :: external_name = REPEAT( ' ', 80 )
!        REAL( KIND = rpc_) :: obj = HUGE( 1.0_KIND = rpc_)
    END TYPE NLLS_inform

    TYPE, PUBLIC :: params_base_type
       ! deliberately empty
    END TYPE PARAMS_base_type

    ABSTRACT INTERFACE
       SUBROUTINE eval_f_type(status, n, m, x, f, params)
         USE CUTEST_KINDS_precision
         import :: params_base_type
         implicit none
         INTEGER ( KIND = ip_ ), intent(out) :: status
         INTEGER ( KIND = ip_ ), intent(in) :: n,m
         REAL ( KIND = rpc_), dimension(*), intent(in)  :: x
         REAL ( KIND = rpc_), dimension(*), intent(out) :: f
         class(params_base_type), intent(in) :: params
       END SUBROUTINE eval_f_type
    END INTERFACE

    ABSTRACT INTERFACE
       SUBROUTINE eval_j_type(status, n, m, x, J, params)
         USE CUTEST_KINDS_precision
         import :: params_base_type
         implicit none
         INTEGER ( KIND = ip_ ), intent(out) :: status
         INTEGER ( KIND = ip_ ), intent(in) :: n,m
         REAL ( KIND = rpc_), dimension(*), intent(in)  :: x
         REAL ( KIND = rpc_), dimension(*), intent(out) :: J
         class(params_base_type), intent(in) :: params
       END SUBROUTINE eval_j_type
    END INTERFACE

    ABSTRACT INTERFACE
       SUBROUTINE eval_hf_type(status, n, m, x, f, h, params)
         USE CUTEST_KINDS_precision
         import :: params_base_type
         implicit none
         INTEGER ( KIND = ip_ ), intent(out) :: status
         INTEGER ( KIND = ip_ ), intent(in) :: n,m
         REAL ( KIND = rpc_), dimension(*), intent(in)  :: x
         REAL ( KIND = rpc_), dimension(*), intent(in)  :: f
         REAL ( KIND = rpc_), dimension(*), intent(out) :: h
         class(params_base_type), intent(in) :: params
       END SUBROUTINE eval_hf_type
    END INTERFACE

   CONTAINS

     ! nothing!

   END MODULE RAL_NLLS_internal

   MODULE RAL_NLLS_precision

     USE CUTEST_KINDS_precision
     USE RAL_NLLS_internal

     IMPLICIT NONE

     PRIVATE

     INTEGER ( KIND = ip_ ), PARAMETER :: error_dimensions = - 1
     INTEGER ( KIND = ip_ ), PARAMETER :: error_workspace = - 2
     INTEGER ( KIND = ip_ ), PARAMETER :: error_eval_F = - 3
     INTEGER ( KIND = ip_ ), PARAMETER :: error_eval_J = - 4
     INTEGER ( KIND = ip_ ), PARAMETER :: error_eval_HF = - 5

     ABSTRACT INTERFACE
        SUBROUTINE eval_F_type( status, n, m, X, F , params )
          USE CUTEST_KINDS_precision
          IMPORT :: params_base_type
          INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
          INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
          REAL ( KIND = rpc_), DIMENSION( * ), INTENT( IN ) :: X
          REAL ( KIND = rpc_ ), DIMENSION( * ), INTENT( OUT ) :: F
          class( params_base_type ), intent( in ) :: params
        END SUBROUTINE eval_F_type
     END INTERFACE

     ABSTRACT INTERFACE
        SUBROUTINE eval_j_type( status, n, m, X, J, params )
          USE CUTEST_KINDS_precision
          IMPORT :: params_base_type
          INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
          INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
          REAL ( KIND = rpc_ ), DIMENSION( * ), INTENT( IN ) :: X
          REAL ( KIND = rpc_ ), DIMENSION( * ), INTENT( OUT ) :: J
          class( params_base_type ), intent( in ) :: params
        END SUBROUTINE eval_j_type
     END INTERFACE

     ABSTRACT INTERFACE
        SUBROUTINE eval_HF_type( status, n, m, X, F, H, params )
          USE CUTEST_KINDS_precision
          IMPORT :: params_base_type
          INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
          INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
          REAL ( KIND = rpc_ ), DIMENSION( * ), INTENT( IN ) :: X
          REAL ( KIND = rpc_ ), DIMENSION( * ), INTENT( IN ) :: F
          REAL ( KIND = rpc_ ), DIMENSION( * ), INTENT( OUT ) :: H
          class( params_base_type ), intent( in ) :: params
        END SUBROUTINE eval_HF_type
     END INTERFACE

     PUBLIC :: NLLS_solve
     PUBLIC :: NLLS_options, NLLS_inform
     PUBLIC :: params_base_type

   CONTAINS

     SUBROUTINE NLLS_solve( n, m, X, eval_F, eval_J, eval_HF,                  &
                            params, options, inform )

!  -----------------------------------------------------------------------------
!  RAL_NLLS, a fortran subroutine for finding a first-order critical
!   point (most likely, a local minimizer) of the nonlinear least-squares
!   objective function 1/2 ||F(x)||_2^2.

!  Authors: RAL NA Group (Iain Duff, Nick Gould, Jonathan Hogg, Tyrone Rees,
!                         Jennifer Scott)
!  -----------------------------------------------------------------------------

!   Dummy arguments

     INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, m
     REAL( KIND = rpc_ ), DIMENSION( n ), INTENT( INOUT ) :: X
     TYPE( Nlls_inform ), INTENT( OUT ) :: inform
     TYPE( Nlls_options ), INTENT( IN ) :: options
     class( params_base_type ) :: params
     PROCEDURE( eval_f_type ) :: eval_F
     PROCEDURE( eval_j_type ) :: eval_J
     PROCEDURE( eval_hf_type ) :: eval_HF

!  Interface blocks

!  Local variables

     INTEGER ( KIND = ip_ ) :: status, start_f, end_f, start_j, start_h, w_end
!    INTEGER ( KIND = ip_ ) :: len_work_int, len_work_real
!    INTEGER ( KIND = ip_ ) ALLOCATABLE :: Work_int( : )
     REAL( KIND = rpc_ ), ALLOCATABLE :: Work_real( : )

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
     INFORM%obj = 0.5_rpc_ * DOT_PRODUCT( WORK_real( start_f : end_f ),    &
                                              WORK_real( start_f : end_F ) )

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
       status = ERROR_eval_HF
       GO TO 990
     END IF

 990 CONTINUE
     inform%status = status
     RETURN
     END SUBROUTINE NLLS_solve

   END MODULE RAL_NLLS_precision


