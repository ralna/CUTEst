! THIS VERSION: CUTEST 2.5 - 2024-02-14 AT 10:30 GMT.

!-*-*-*-*-*-*-  C U T E S T    T R A M P O L I N E     M O D U L E   -*-*-*-*-*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Alexis Montoison

!  History -
!   released as part of CUTEst, 14th April 2025

#include "cutest_modules.h"

#ifdef REAL_32
#define LOAD_ROUTINES_NAME "cutest_load_routines_s_"
#define UNLOAD_ROUTINES_NAME "cutest_unload_routines_s_"
#define ELFUN_BIND_NAME "elfun_s_"
#define GROUP_BIND_NAME "group_s_"
#define RANGE_BIND_NAME "range_s_"
#elif REAL_128
#define LOAD_ROUTINES_NAME "cutest_load_routines_q_"
#define UNLOAD_ROUTINES_NAME "cutest_unload_routines_q_"
#define ELFUN_BIND_NAME "elfun_q_"
#define GROUP_BIND_NAME "group_q_"
#define RANGE_BIND_NAME "range_q_"
#else
#define LOAD_ROUTINES_NAME "cutest_load_routines_"
#define UNLOAD_ROUTINES_NAME "cutest_unload_routines_"
#define ELFUN_BIND_NAME "elfun_"
#define GROUP_BIND_NAME "group_"
#define RANGE_BIND_NAME "range_"
#endif

#ifdef WINDOWS
#define DLOPEN_BIND_NAME "LoadLibraryA"
#define DLSYM_BIND_NAME "GetProcAddress"
#define DLCLOSE_BIND_NAME "FreeLibrary"
#else
#define DLOPEN_BIND_NAME "dlopen"
#define DLSYM_BIND_NAME "dlsym"
#define DLCLOSE_BIND_NAME "dlclose"
#endif

module CUTEST_TRAMPOLINE_precision
  use, intrinsic :: iso_c_binding
  implicit none

  ! Interface for dlopen / LoadLibrary
  interface
    function cutest_dlopen(name, mode) bind(C, name=DLOPEN_BIND_NAME)
      use iso_c_binding, only: c_ptr, c_int, c_char
      type(c_ptr) :: cutest_dlopen
      character(kind=c_char), dimension(*) :: name
      integer(kind=c_int), value :: mode
    end function cutest_dlopen
  end interface

  ! Interface for dlsym / GetProcAddress
  interface
    function cutest_dlsym(handle, symbol) bind(C, name=DLSYM_BIND_NAME)
      use iso_c_binding, only: c_funptr, c_ptr, c_char
      type(c_funptr) :: cutest_dlsym
      type(c_ptr), value :: handle
      character(kind=c_char), dimension(*) :: symbol
    end function cutest_dlsym
  end interface

  ! Interface for dlclose / FreeLibrary
  interface
    subroutine cutest_dlclose(handle) bind(C, name=DLCLOSE_BIND_NAME)
      use iso_c_binding, only: c_ptr
      ! type(c_int) :: cutest_dlclose
      type(c_ptr), value :: handle
    end subroutine cutest_dlclose
  end interface

  ! Interface for elfun
  interface
    subroutine elfun_interface( FUVALS, XVALUE, EPVALU, ncalcf, ITYPEE, ISTAEV,&
                                IELVAR, INTVAR, ISTADH, ISTEPA, ICALCF, ltypee,&
                                lstaev, lelvar, lntvar, lstadh, lstepa, lcalcf,&
                                lfuval, lxvalu, lepvlu, ifflag, ifstat )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: ncalcf, ifflag, lxvalu, lepvlu
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: ltypee, lstaev, lelvar, lntvar
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: lstadh, lstepa, lcalcf, lfuval
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: ifstat
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: ITYPEE(ltypee), ISTAEV(lstaev)
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: IELVAR(lelvar), ISTEPA(lstepa)
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: INTVAR(lntvar), ISTADH(lstadh)
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: ICALCF(lcalcf)
      INTEGER ( KIND = rp_ ), INTENT( IN ) :: XVALUE(lxvalu), EPVALU(lepvlu)
      INTEGER ( KIND = rp_ ), INTENT( INOUT ) :: FUVALS(lfuval)
    end subroutine elfun_interface
  end interface

  ! Interface for group
  interface
    subroutine group_interface( GVALUE, lgvalu, FVALUE, GPVALU, ncalcg, ITYPEG,&
                                ISTGPA, ICALCG, ltypeg, lstgpa, lcalcg, lfvalu,&
                                lgpvlu, derivs, igstat )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: lgvalu, ncalcg, lgpvlu
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: ltypeg, lstgpa, lcalcg, lfvalu
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: igstat
      LOGICAL, INTENT( IN ) :: derivs
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION ( ltypeg ) :: ITYPEG
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION ( lstgpa ) :: ISTGPA
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION ( lcalcg ) :: ICALCG
      INTEGER ( KIND = rp_ ), INTENT( IN ), DIMENSION ( lfvalu ) :: FVALUE
      INTEGER ( KIND = rp_ ), INTENT( IN ), DIMENSION ( lgpvlu ) :: GPVALU
      INTEGER ( KIND = rp_ ), INTENT( INOUT ), DIMENSION (lgvalu, 3) :: GVALUE
    end subroutine group_interface
  end interface

  ! Interface for range
  interface
    subroutine range_interface( ielemn, transp, W1, W2, nelvar, ninvar, ieltyp,&
                                lw1, lw2 )
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: ielemn, nelvar, ninvar, ieltyp
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: lw1, lw2
      LOGICAL, INTENT( IN ) :: transp
      INTEGER ( KIND = rp_ ), INTENT( IN ) :: W1(lw1)
      INTEGER ( KIND = rp_ ), INTENT( OUT ) :: W2(lw2)
    end subroutine range_interface
  end interface

  ! Constant for the library open mode (lazy binding)
  integer(kind=c_int), parameter :: RTLD_LAZY = 1

  ! Handles for external functions
  type(c_ptr) :: lib_handle = c_null_ptr
  type(c_funptr) :: ptr_elfun = c_null_funptr
  type(c_funptr) :: ptr_group = c_null_funptr
  type(c_funptr) :: ptr_range = c_null_funptr

  ! Procedure pointers for external functions
  procedure(elfun_interface), pointer :: fun_elfun => null()
  procedure(group_interface), pointer :: fun_group => null()
  procedure(range_interface), pointer :: fun_range => null()

contains
  ! Subroutine to load the routines from the given shared library
  subroutine cutest_load_routines(libname) bind(C, name=LOAD_ROUTINES_NAME)
    character(kind=c_char), dimension(*), intent(in) :: libname

    ! Load the dynamic library
    lib_handle = cutest_dlopen(libname, RTLD_LAZY)

    if (.not. c_associated(lib_handle)) then
      print *, "Unable to load shared library."
      return
    end if

    ! Resolve function symbols
    ptr_elfun = cutest_dlsym(lib_handle, ELFUN_BIND_NAME // c_null_char)
    ptr_group = cutest_dlsym(lib_handle, GROUP_BIND_NAME // c_null_char)
    ptr_range = cutest_dlsym(lib_handle, RANGE_BIND_NAME // c_null_char)

    ! Associate procedure pointers
    call c_f_procpointer(ptr_elfun, fun_elfun)
    call c_f_procpointer(ptr_group, fun_group)
    call c_f_procpointer(ptr_range, fun_range)
  end subroutine cutest_load_routines

  ! Unload the routines and reset internal state
  subroutine cutest_unload_routines() bind(C, name=UNLOAD_ROUTINES_NAME)
    if (c_associated(lib_handle)) then
      call cutest_dlclose(lib_handle)
      lib_handle = c_null_ptr
    end if

    ptr_elfun = c_null_funptr
    ptr_group = c_null_funptr
    ptr_range = c_null_funptr

    nullify(fun_elfun)
    nullify(fun_group)
    nullify(fun_range)
  end subroutine cutest_unload_routines

  ! Redirection subroutine for calling the 'elfun' function from the dynamic library
  subroutine elfun( FUVALS, XVALUE, EPVALU, ncalcf, ITYPEE, ISTAEV, IELVAR,    &
                    INTVAR, ISTADH, ISTEPA, ICALCF, ltypee, lstaev, lelvar,    &
                    lntvar, lstadh, lstepa, lcalcf, lfuval, lxvalu, lepvlu,    &
                    ifflag, ifstat ) bind(C, name=ELFUN_BIND_NAME)
    USE CUTEST_KINDS_precision
    INTEGER ( KIND = ip_ ), INTENT( IN ) :: ncalcf, ifflag, lxvalu, lepvlu
    INTEGER ( KIND = ip_ ), INTENT( IN ) :: ltypee, lstaev, lelvar, lntvar
    INTEGER ( KIND = ip_ ), INTENT( IN ) :: lstadh, lstepa, lcalcf, lfuval
    INTEGER ( KIND = ip_ ), INTENT( OUT ) :: ifstat
    INTEGER ( KIND = ip_ ), INTENT( IN ) :: ITYPEE(ltypee), ISTAEV(lstaev)
    INTEGER ( KIND = ip_ ), INTENT( IN ) :: IELVAR(lelvar), ISTEPA(lstepa)
    INTEGER ( KIND = ip_ ), INTENT( IN ) :: INTVAR(lntvar), ISTADH(lstadh)
    INTEGER ( KIND = ip_ ), INTENT( IN ) :: ICALCF(lcalcf)
    INTEGER ( KIND = rp_ ), INTENT( IN ) :: XVALUE(lxvalu), EPVALU(lepvlu)
    INTEGER ( KIND = rp_ ), INTENT( INOUT ) :: FUVALS(lfuval)
    if (associated(fun_elfun)) then
      call fun_elfun( FUVALS, XVALUE, EPVALU, ncalcf, ITYPEE, ISTAEV, IELVAR,  &
                      INTVAR, ISTADH, ISTEPA, ICALCF, ltypee, lstaev, lelvar,  &
                      lntvar, lstadh, lstepa, lcalcf, lfuval, lxvalu, lepvlu,  &
                      ifflag, ifstat )
    else
      print *, "Error: fun_elfun is not associated."
    end if
  end subroutine elfun

  ! Redirection subroutine for calling the 'group' function from the dynamic library
  subroutine group( GVALUE, lgvalu, FVALUE, GPVALU, ncalcg, ITYPEG, ISTGPA,    &
                    ICALCG, ltypeg, lstgpa, lcalcg, lfvalu, lgpvlu, derivs,    &
                    igstat) bind(C, name=GROUP_BIND_NAME)
    USE CUTEST_KINDS_precision
    INTEGER ( KIND = ip_ ), INTENT( IN ) :: lgvalu, ncalcg, lgpvlu
    INTEGER ( KIND = ip_ ), INTENT( IN ) :: ltypeg, lstgpa, lcalcg, lfvalu
    INTEGER ( KIND = ip_ ), INTENT( OUT ) :: igstat
    LOGICAL, INTENT( IN ) :: derivs
    INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION ( ltypeg ) :: ITYPEG
    INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION ( lstgpa ) :: ISTGPA
    INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION ( lcalcg ) :: ICALCG
    INTEGER ( KIND = rp_ ), INTENT( IN ), DIMENSION ( lfvalu ) :: FVALUE
    INTEGER ( KIND = rp_ ), INTENT( IN ), DIMENSION ( lgpvlu ) :: GPVALU
    INTEGER ( KIND = rp_ ), INTENT( INOUT ), DIMENSION (lgvalu, 3) :: GVALUE
    if (associated(fun_group)) then
      call fun_group( GVALUE, lgvalu, FVALUE, GPVALU, ncalcg, ITYPEG, ISTGPA,  &
                      ICALCG, ltypeg, lstgpa, lcalcg, lfvalu, lgpvlu, derivs,  &
                      igstat )
    else
      print *, "Error: fun_group is not associated."
    end if
  end subroutine group

  ! Redirection subroutine for calling the 'range' function from the dynamic library
  subroutine range( ielemn, transp, W1, W2, nelvar, ninvar, ieltyp,            &
                    lw1, lw2 ) bind(C, name=RANGE_BIND_NAME)
    USE CUTEST_KINDS_precision
    INTEGER ( KIND = ip_ ), INTENT( IN ) :: ielemn, nelvar, ninvar, ieltyp
    INTEGER ( KIND = ip_ ), INTENT( IN ) :: lw1, lw2
    LOGICAL, INTENT( IN ) :: transp
    INTEGER ( KIND = rp_ ), INTENT( IN ) :: W1(lw1)
    INTEGER ( KIND = rp_ ), INTENT( OUT ) :: W2(lw2)
    if (associated(fun_range)) then
      call fun_range( ielemn, transp, W1, W2, nelvar, ninvar, ieltyp, lw1, lw2 )
    else
      print *, "Error: fun_range is not associated."
    end if
  end subroutine range

end module CUTEST_TRAMPOLINE_precision
