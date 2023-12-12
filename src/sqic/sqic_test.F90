! THIS VERSION: CUTEST 2.2 - 2023-11-25 AT 14:40 GMT.

#include "cutest_modules.h"

!  slimline CUTEst interface sqic_main to SQIC
!  Nick Gould, February 2014

!=============================================================================
! abreviated header from cuter.f90
! Philip Gill and Elizabeth Wong
! 15 Feb 2010: First version.
!=============================================================================

 subroutine crd2spr ( n, ne, cval, irow, jcol, val, ind, loc )
  use CUTEST_KINDS_precision
  implicit none
  integer(kind=ip_), intent(in)  :: n, ne, irow(ne), jcol(ne)
  real(kind=rp_),    intent(in)  :: cval(ne)
  integer(kind=ip_), intent(out) :: ind(ne), loc(n+1)
  real(kind=rp_),    intent(out) :: val(ne)
 end subroutine crd2spr

 subroutine ctSQIC ( Start, INFO, iPrint, iSumm, iSpecs, name,                &
                     m, n, nnH, iObj, ObjAdd,                                 &
                     neA, indA, locA, valA, bl, bu, ncObj, cObj,              &
                     nNames, Names, hEtype, hs, x, pi, rc,                    &
                     neH, indH, locH, valH,  nS, nInf, sInf, Obj )
  use CUTEST_KINDS_precision
  implicit none
  character*(*), intent(in)    :: Start
  integer(kind=ip_),   intent(in)    :: iPrint, iSumm, iSpecs
  integer(kind=ip_),   intent(inout) :: nInf
  integer(kind=ip_),   intent(out)   :: INFO, nS
  real(kind=rp_),      intent(out)   :: sInf, Obj
  character(8), intent(in)     :: name
  integer(kind=ip_),  intent(in) :: m, n, nnH, ncObj, neA, iObj, nNames, neH
  real(kind=rp_),     intent(in) :: ObjAdd
  character(8), target :: Names(nNames)
  integer(kind=ip_),  target   :: locA(n+1), indA(neA), locH(nnH+1), indH(neH)
  integer(kind=ip_),  target   :: hEtype(n+m), hs(n+m)
  real(kind=rp_),     target   :: cObj(ncObj), bl(n+m), bu(n+m), valA(neA)
  real(kind=rp_),     target   :: valH(neH), x(n+m), pi(m), rc(n+m)
  Obj = 0.0_rp_
 end subroutine ctSQIC

