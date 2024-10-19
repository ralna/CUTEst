! THIS VERSION: CUTEST 2.3 - 2024-10-15 AT 10:30 GMT.

!-*-*-*-*-*-*-  C U T E S T    D E L E G A T E     M O D U L E   -*-*-*-*-*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Alexis Montoison

!  History -
!   released as part of CUTEst, 17th June 2024

#ifdef REAL_32
#define cutest_delegate_r cutest_delegate_s
#define LOAD_ROUTINE_NAME "load_routines_s_"
#define ELFUN_BIND_NAME "elfun_s_"
#define GROUP_BIND_NAME "group_s_"
#define RANGE_BIND_NAME "range_s_"
#elif REAL_128
#define cutest_delegate_r cutest_delegate_q
#define LOAD_ROUTINE_NAME "load_routines_q"
#define ELFUN_BIND_NAME "elfun_q_"
#define GROUP_BIND_NAME "group_q_"
#define RANGE_BIND_NAME "range_q_"
#else
#define cutest_delegate_r cutest_delegate_d
#define LOAD_ROUTINE_NAME "load_routines_"
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

module cutest_delegate_r
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

  ! Constantes pour les modes d'ouverture de bibliothèques
  integer, parameter :: RTLD_LAZY = 1

  ! Handles pour les fonctions externes
  type(c_ptr) :: lib_handle
  type(c_funptr) :: ptr_elfun
  type(c_funptr) :: ptr_group
  type(c_funptr) :: ptr_range

  ! Pointeurs de procédure pour les fonctions externes
  procedure(), pointer :: fun_elfun => null()
  procedure(), pointer :: fun_group => null()
  procedure(), pointer :: fun_range => null()

contains

  ! Routine pour charger les routines depuis la bibliothèque dynamique
  subroutine load_routines(libname) bind(C, name=LOAD_ROUTINE_NAME)
    use iso_c_binding
    implicit none
    character(kind=c_char), dimension(*), intent(in) :: libname

    ! Charge la bibliothèque dynamique
    lib_handle = cutest_dlopen(libname, RTLD_LAZY)
    if (.not. c_associated(lib_handle)) then
      stop "Unable to load library"
    end if

    ! Récupère les adresses des fonctions
    ptr_elfun = cutest_dlsym(lib_handle, ELFUN_BIND_NAME//c_null_char)
    ptr_group = cutest_dlsym(lib_handle, GROUP_BIND_NAME//c_null_char)
    ptr_range = cutest_dlsym(lib_handle, RANGE_BIND_NAME//c_null_char)

    ! Associe les pointeurs de procédure Fortran avec les adresses obtenues
    call c_f_procpointer(ptr_elfun, fun_elfun)
    call c_f_procpointer(ptr_group, fun_group)
    call c_f_procpointer(ptr_range, fun_range)
  end subroutine load_routines

  subroutine elfun() bind(C, name=ELFUN_BIND_NAME)
    if (associated(fun_elfun)) then
      call fun_elfun()
    else
      print *, "Error: fun_elfun is not associated."
    end if
  end subroutine elfun

  subroutine group() bind(C, name=GROUP_BIND_NAME)
    if (associated(fun_group)) then
      call fun_group()
    else
      print *, "Error: fun_group is not associated."
    end if
  end subroutine group

  subroutine range() bind(C, name=RANGE_BIND_NAME)
    if (associated(fun_range)) then
      call fun_range()
    else
      print *, "Error: fun_range is not associated."
    end if
  end subroutine range

end module cutest_delegate_r
