! THIS VERSION: CUTEST 2.2 - 2024-08-27 AT 08:15 GMT.

!-**-*-*-*-*-*-  S P A R S E _ H S L _ K I N D S   M O D U L E S -*-*-*-*-*-*-*-

!  Fortran and C integer and real kinds used in CUTEst

!  Principal author: Nick Gould

!  History -
!   originally released, CUTEST Version 2.2, November 2nd 2023

!  For full documentation, see
!   https://github.com/ralna/CUTEst/

! ..............................................................................
!
!                 S T A N D A R D   I N T E G E R   M O D U L E S
!
! ..............................................................................

!-*-*-*-*-*-*-  C U T E S T _  K I N D S _ I N T    M O D U L E   -*-*-*-*-*-*-

  MODULE CUTEST_KINDS_int

    USE ISO_C_BINDING
    USE ISO_FORTRAN_ENV
    PRIVATE
    PUBLIC :: ip_, ipc_, long_

!  Integer kinds (standard 32 bit integer)

    INTEGER, PARAMETER :: long_ = INT64
    INTEGER, PARAMETER :: ip_ = INT32
    INTEGER, PARAMETER :: ipc_ = C_INT32_T

  END MODULE CUTEST_KINDS_int

!-*-*-*-*-*-*-  C U T E S T _  K I N D S _ 6 4    M O D U L E   -*-*-*-*-*-*-

  MODULE CUTEST_KINDS_64

    USE ISO_C_BINDING
    USE ISO_FORTRAN_ENV
    PRIVATE
    PUBLIC :: ip_, ipc_, long_

!  Integer kinds (long 64 bit integer)

    INTEGER, PARAMETER :: long_ = INT64
    INTEGER, PARAMETER :: ip_ = INT64
    INTEGER, PARAMETER :: ipc_ = C_INT64_T

  END MODULE CUTEST_KINDS_64

! ..............................................................................
!
!                  S T A N D A R D   R E A L   M O D U L E S
!
! ..............................................................................

!-*-*-*-*-*-*-  C U T E S T _  K I N D S _ R E A L   M O D U L E   -*-*-*-*-*-*-

  MODULE CUTEST_KINDS_real

    USE ISO_C_BINDING
    USE ISO_FORTRAN_ENV
    PRIVATE
    PUBLIC :: sp_, dp_

   INTEGER, PARAMETER :: sp_ = REAL32
   INTEGER, PARAMETER :: dp_ = REAL64

  END MODULE CUTEST_KINDS_real

!-*-*-*-*-*-*-  C U T E S T _  K I N D S _ H A L F   M O D U L E   -*-*-*-*-*-*-

  MODULE CUTEST_KINDS_r2

    USE ISO_C_BINDING
    USE ISO_FORTRAN_ENV
    USE CUTEST_KINDS_real
    PRIVATE
    PUBLIC :: real_bytes_, rp_, rpc_, cp_, sp_, dp_

!  Real and complex kinds (half precision)

    INTEGER, PARAMETER :: real_bytes_ = 2
#ifdef CUTEST_2btye_reals_exist
    INTEGER, PARAMETER :: rp_ = REAL16!! selected_real_kind( 3 )
! INTEGER, PARAMETER :: rpc_ = C_FLOAT16!! does this exist??
    INTEGER, PARAMETER :: rpc_ = C_FLOAT
#else
    INTEGER, PARAMETER :: rp_ = REAL32
    INTEGER, PARAMETER :: rpc_ = C_FLOAT
#endif
    INTEGER, PARAMETER :: cp_ = KIND( ( 1.0_rp_, 1.0_rp_ ) )

  END MODULE CUTEST_KINDS_r2

!-*-*-*-*-*-  C U T E S T _  K I N D S _ S I N G L E   M O D U L E   -*-*-*-*-*-

  MODULE CUTEST_KINDS_r4

    USE ISO_C_BINDING
    USE ISO_FORTRAN_ENV
    USE CUTEST_KINDS_real
    PRIVATE
    PUBLIC :: real_bytes_, rp_, rpc_, cp_, sp_, dp_

!  Real and complex kinds (single precision)

    INTEGER, PARAMETER :: real_bytes_ = 4
    INTEGER, PARAMETER :: rp_ = REAL32
    INTEGER, PARAMETER :: rpc_ = C_FLOAT
    INTEGER, PARAMETER :: cp_ = KIND( ( 1.0_rp_, 1.0_rp_ ) )

  END MODULE CUTEST_KINDS_r4

!-*-*-*-  C U T E S T _  K I N D S _ D O U B L E   M O D U L E   -*-*-*-

  MODULE CUTEST_KINDS_r8

    USE ISO_C_BINDING
    USE ISO_FORTRAN_ENV
    USE CUTEST_KINDS_real
    PRIVATE
    PUBLIC :: real_bytes_, rp_, rpc_, cp_, sp_, dp_

!  Real and complex kinds (double precision)

    INTEGER, PARAMETER :: real_bytes_ = 8
    INTEGER, PARAMETER :: rp_ = REAL64
    INTEGER, PARAMETER :: rpc_ = C_DOUBLE
    INTEGER, PARAMETER :: cp_ = KIND( ( 1.0_rp_, 1.0_rp_ ) )

  END MODULE CUTEST_KINDS_r8

!-*-  C U T E S T _  K I N D S _ Q U A D R U P L E   M O D U L E   -*-

  MODULE CUTEST_KINDS_r16

    USE ISO_C_BINDING
    USE ISO_FORTRAN_ENV
    USE CUTEST_KINDS_real
    PRIVATE
    PUBLIC :: real_bytes_, rp_, rpc_, cp_, sp_, dp_

!  Real and complex kinds (quadruple precision)

    INTEGER, PARAMETER :: real_bytes_ = 16
#ifdef CUTEST_16btye_reals_exist
    INTEGER, PARAMETER :: rp_ = REAL128 !! selected_real_kind( 20 )
    INTEGER, PARAMETER :: rpc_ = C_FLOAT128 !! does this exist??
#else
    INTEGER, PARAMETER :: rp_ = REAL64
    INTEGER, PARAMETER :: rpc_ = C_DOUBLE
#endif
    INTEGER, PARAMETER :: cp_ = KIND( ( 1.0_rp_, 1.0_rp_ ) )

  END MODULE CUTEST_KINDS_r16

! ..............................................................................
!
!                         M I X E D   M O D U L E S
!
! ..............................................................................


!-*-*-*-*-*-*-  C U T E S T _  K I N D S _ H A L F   M O D U L E   -*-*-*-*-*-

  MODULE CUTEST_KINDS_half
    USE CUTEST_KINDS_int
    USE CUTEST_KINDS_r2
    PUBLIC
  END MODULE CUTEST_KINDS_half

!-*-*-*-*-*-  C U T E S T _  K I N D S _ S I N G L E   M O D U L E   -*-*-*-*-*-

  MODULE CUTEST_KINDS_single
    USE CUTEST_KINDS_int
    USE CUTEST_KINDS_r4
    PUBLIC
  END MODULE CUTEST_KINDS_single

!-*-*-*-*-*-  C U T E S T _  K I N D S _ D O U B L E   M O D U L E   -*-*-*-*-*-

  MODULE CUTEST_KINDS_double
    USE CUTEST_KINDS_int
    USE CUTEST_KINDS_r8
    PUBLIC
  END MODULE CUTEST_KINDS_double

!-*-*-*-  C U T E S T _  K I N D S _ Q U A D R U P L E   M O D U L E   -*-*-*-

  MODULE CUTEST_KINDS_quadruple
    USE CUTEST_KINDS_int
    USE CUTEST_KINDS_r16
    PUBLIC
  END MODULE CUTEST_KINDS_quadruple

!-*-*-*-  C U T E S T _  K I N D S _ H A L F _ 6 4 B I T   M O D U L E  -*-*-*-

  MODULE CUTEST_KINDS_half_64
    USE CUTEST_KINDS_64
    USE CUTEST_KINDS_r2
    PUBLIC
  END MODULE CUTEST_KINDS_half_64

!-*-*-  C U T E S T _  K I N D S _ S I N G L E _ 6 4 B I T   M O D U L E -*-*-

  MODULE CUTEST_KINDS_single_64
    USE CUTEST_KINDS_64
    USE CUTEST_KINDS_r4
    PUBLIC
  END MODULE CUTEST_KINDS_single_64

!-*-*-  C U T E S T _  K I N D S _ D O U B L E _ 6 4 B I T   M O D U L E -*-*-

  MODULE CUTEST_KINDS_double_64
    USE CUTEST_KINDS_64
    USE CUTEST_KINDS_r8
    PUBLIC
  END MODULE CUTEST_KINDS_double_64

!-*- C U T E S T _  K I N D S _ Q U A D R U P L E _ 6 4 B I T  M O D U L E -*-

  MODULE CUTEST_KINDS_quadruple_64
    USE CUTEST_KINDS_64
    USE CUTEST_KINDS_r16
    PUBLIC
  END MODULE CUTEST_KINDS_quadruple_64
