! THIS VERSION: CUTEST 2.5 - 2024-02-14 AT 10:30 GMT.

!-*-*-*-*-*-*-  C U T E S T    T R A M P O L I N E     M O D U L E   -*-*-*-*-*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Alexis Montoison

!  History -
!   released as part of CUTEst, 14th April 2025

#include "cutest_modules.h"

#ifdef REAL_32
#define LOAD_ROUTINES_NAME "load_routines_s_"
#define UNLOAD_ROUTINES_NAME "unload_routines_s_"
#define SHOW_LOADED_LIBRARY_NAME "show_loaded_library_s_"
#define ELFUN_BIND_NAME "elfun_s_"
#define GROUP_BIND_NAME "group_s_"
#define RANGE_BIND_NAME "range_s_"
#elif REAL_128
#define LOAD_ROUTINES_NAME "load_routines_q_"
#define UNLOAD_ROUTINES_NAME "unload_routines_q_"
#define SHOW_LOADED_LIBRARY_NAME "show_loaded_library_q_"
#define ELFUN_BIND_NAME "elfun_q_"
#define GROUP_BIND_NAME "group_q_"
#define RANGE_BIND_NAME "range_q_"
#else
#define LOAD_ROUTINES_NAME "load_routines_"
#define UNLOAD_ROUTINES_NAME "unload_routines_"
#define SHOW_LOADED_LIBRARY_NAME "show_loaded_library_"
#define ELFUN_BIND_NAME "elfun_"
#define GROUP_BIND_NAME "group_"
#define RANGE_BIND_NAME "range_"
#endif

#ifdef WINDOWS
#define cutest_dlopen LoadLibrary
#define DLOPEN_BIND_NAME "LoadLibraryA"
#define cutest_dlsym GetProcAddress
#define DLSYM_BIND_NAME "GetProcAddress"
#define cutest_dlclose FreeLibrary
#define DLCLOSE_BIND_NAME "FreeLibrary"
#else
#define cutest_dlopen dlopen
#define DLOPEN_BIND_NAME "dlopen"
#define cutest_dlsym dlsym
#define DLSYM_BIND_NAME "dlsym"
#define cutest_dlclose dlclose
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
      integer(kind=c_int) :: mode
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
      type(c_ptr), value :: handle
    end subroutine cutest_dlclose
  end interface

  ! Constant for the library open mode (lazy binding)
  integer(kind=c_int), parameter :: RTLD_LAZY = 1

  ! Handles for external functions
  type(c_ptr) :: lib_handle = c_null_ptr
  type(c_funptr) :: ptr_elfun = c_null_funptr
  type(c_funptr) :: ptr_group = c_null_funptr
  type(c_funptr) :: ptr_range = c_null_funptr

  ! Variable for the library path
  ! character(len=:), allocatable :: library_path

  ! Procedure pointers for external functions
  procedure(), pointer :: fun_elfun => null()
  procedure(), pointer :: fun_group => null()
  procedure(), pointer :: fun_range => null()

contains
  ! Subroutine to load the routines from the given shared library
  subroutine load_routines(libname) bind(C, name=LOAD_ROUTINES_NAME)
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
  end subroutine load_routines

  ! Unload the routines and reset internal state
  subroutine unload_routines() bind(C, name=UNLOAD_ROUTINES_NAME)
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
  end subroutine unload_routines

  ! Show the currently loaded library
  ! subroutine show_loaded_library() bind(C, name=SHOW_LOADED_LIBRARY_NAME)
  !   if (allocated(library_path)) then
  !     print *, "Currently loaded library: ", library_path
  !   else
  !     print *, "No library loaded."
  !   end if
  ! end subroutine show_loaded_library

  ! Redirection subroutine for calling the 'elfun' function from the dynamic library
  subroutine elfun() bind(C, name=ELFUN_BIND_NAME)
    if (associated(fun_elfun)) then
      call fun_elfun()
    else
      print *, "Error: fun_elfun is not associated."
    end if
  end subroutine elfun

  ! Redirection subroutine for calling the 'group' function from the dynamic library
  subroutine group() bind(C, name=GROUP_BIND_NAME)
    if (associated(fun_group)) then
      call fun_group()
    else
      print *, "Error: fun_group is not associated."
    end if
  end subroutine group

  ! Redirection subroutine for calling the 'range' function from the dynamic library
  subroutine range() bind(C, name=RANGE_BIND_NAME)
    if (associated(fun_range)) then
      call fun_range()
    else
      print *, "Error: fun_range is not associated."
    end if
  end subroutine range

end module CUTEST_TRAMPOLINE_precision
