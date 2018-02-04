!     ( Last modified on 28 Feb 2014 at 09:50:00 )

!  slimline CUTEst interface sqic_main to SQIC
!  Nick Gould, February 2014

!=============================================================================
! abreviated header from cuter.f90
! Philip Gill and Elizabeth Wong
! 15 Feb 2010: First version.
!=============================================================================

 subroutine crd2spr ( n, ne, cval, irow, jcol, val, ind, loc )
  implicit none
  integer, parameter :: ip = kind( 1 ), rp = kind( 1.0D+0 )
  integer(ip), intent(in)  :: n, ne, irow(ne), jcol(ne)
  real(rp),    intent(in)  :: cval(ne)
  integer(ip), intent(out) :: ind(ne), loc(n+1)
  real(rp),    intent(out) :: val(ne)
 end subroutine crd2spr

 subroutine ctSQIC ( Start, INFO, iPrint, iSumm, iSpecs, name,                &
                     m, n, nnH, iObj, ObjAdd,                                 &
                     neA, indA, locA, valA, bl, bu, ncObj, cObj,              &
                     nNames, Names, hEtype, hs, x, pi, rc,                    &
                     neH, indH, locH, valH,  nS, nInf, sInf, Obj )
  implicit none
  integer, parameter :: ip = kind( 1 ), rp = kind( 1.0D+0 )
  character*(*), intent(in)    :: Start
  integer(ip),   intent(in)    :: iPrint, iSumm, iSpecs
  integer(ip),   intent(inout) :: nInf
  integer(ip),   intent(out)   :: INFO, nS
  real(rp),      intent(out)   :: sInf, Obj
  character(8), intent(in)     :: name
  integer(ip),  intent(in)     :: m, n, nnH, ncObj, neA, iObj, nNames, neH
  real(rp),     intent(in)     :: ObjAdd
  character(8), target         :: Names(nNames)
  integer(ip),  target         :: locA(n+1), indA(neA), locH(nnH+1), indH(neH)
  integer(ip),  target         :: hEtype(n+m), hs(n+m)
  real(rp),     target         :: cObj(ncObj), bl(n+m), bu(n+m), valA(neA)
  real(rp),     target         :: valH(neH), x(n+m), pi(m), rc(n+m)
 end subroutine ctSQIC

