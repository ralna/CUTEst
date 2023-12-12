! THIS VERSION: CUTEST 2.2 - 2023-11-25 AT 14:40 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

  program sqic_main

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!     Driver for running SQIC on CUTEst problems.

!     Derived from SQIC program sqicma.f90 written by
!     Philip Gill and Elizabeth Wong
!     CUTEst evolution February 2013, Nick Gould
!     updates February 2014, Elizabeth Wong

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  use CUTEST_KINDS_precision
  use CUTEST_INTERFACE_precision

  implicit none

  integer(kind=ip_), parameter :: iCutest = 55, iOut = 6, io_buffer = 11
  integer(kind=ip_), parameter :: iPrint = 9, iSumm  = 6, iSpecs = 4
  integer(kind=ip_)   :: INFO, n, m, nm, nnH, ncObj, neA, lenA, neH, neH2
  integer(kind=ip_)   :: iObj, nName, status, alloc_stat, j, nInf, lenH, nS
  real(kind=rp_)      :: Obj, ObjAdd, sInf
  real(kind=rp_)      :: CPU( 4 ), CALLS( 7 )
  character(8)  :: Names(1)
  character(10) :: Prob
  character(20) :: filename

  integer(kind=ip_), pointer :: hs(:), hEtype(:), indA(:), locA(:), jcol2(:)
  integer(kind=ip_), pointer :: indH(:), locH(:), irow(:), jcol(:), irow2(:)
  real(kind=rp_),    pointer :: bl(:), bu(:), x(:), pi(:), rc(:), cObj(:)
  real(kind=rp_),    pointer :: valA(:), valH(:), zero(:)
  real(kind=rp_),    pointer :: cval(:), cval2(:), b(:)
  logical,       pointer :: equation(:), linear(:)
  character(10), pointer :: vname(:), gname(:)

  open ( iCutest, file = 'OUTSDIF.d', form = 'FORMATTED', status = 'old' )
  rewind( iCutest )

  ! Get dimensions and allocate space.
  call CUTEST_cdimen_r( status, iCutest, n, m )
  if ( status /= 0 ) go to 910

  !-----------------------------------------------------------------------------
  ! Initial point and bounds ( n, m, x, bl, bu )
  ! Problem name ( Prob )
  ! Constraints

  if ( m > 0 ) then
     nm = n+m

     allocate ( bl(nm), bu(nm), cObj(n), x(nm), hs(nm), pi(m), rc(nm),         &
                hEtype(nm), b(m), zero(n), equation(m), linear(m),             &
                stat = alloc_stat )
     if ( alloc_stat /= 0 ) GO TO 990
     hEtype = 0
     zero   = 0.0
     x      = 0.0

     call CUTEST_csetup_r( status, iCutest, iOut, io_buffer,                   &
                           n, m, x(1:n), bl(1:n), bu(1:n), pi,                 &
                           bl(n+1:nm), bu(n+1:nm), equation, linear, 0, 2, 1 )
     if ( status /= 0 ) go to 910
     deallocate ( equation, linear )

     allocate ( vname(n), gname(m), stat = alloc_stat )
     if ( alloc_stat /= 0 ) GO TO 990

     call CUTEST_cnames_r( status, n, m, Prob, vname, gname )
     if ( status /= 0 ) go to 910
     deallocate ( vname, gname )

     call CUTEST_cdimsj_r( status, lenA )
     if ( status /= 0 ) go to 910
     lenA = lenA - n
     allocate ( cval(lenA), irow(lenA), jcol(lenA), stat = alloc_stat )
     if ( alloc_stat /= 0 ) GO TO 990

     call CUTEST_ccfsg_r( status, n, m, zero, b, neA, lenA, cval, jcol, irow,  &
                          .true. )
     if ( status /= 0 ) go to 910

     bl(n+1:nm) = bl(n+1:nm) - b
     bu(n+1:nm) = bu(n+1:nm) - b
     deallocate ( b )

     ! Convert coordinate form to sparse-by-col form.
     allocate ( indA(neA), locA(n+1), valA(neA), stat = alloc_stat )
     if ( alloc_stat /= 0 ) GO TO 990
     call crd2spr ( n, neA, cval, irow, jcol, valA, indA, locA )
     deallocate ( irow, jcol, cval )

     ! Objective
     ! cofg returns ObjAdd and cObj.
     call CUTEST_cofg_r( status, n, zero, ObjAdd, cObj, .true. )
     if ( status /= 0 ) go to 910

     ! Hessian of the objective
     ! Stored in the module variables neH, valH, locH, indH.
     call CUTEST_cdimsh_r ( status, lenH )
     if ( status /= 0 ) go to 910

     if ( lenH > 0 ) then
        allocate ( cval(lenH), irow(lenH), jcol(lenH), stat = alloc_stat )
        if ( alloc_stat /= 0 ) GO TO 990

        call CUTEST_cish_r ( status, n, zero, 0, neH, lenH, cval, irow, jcol )
        if ( status /= 0 ) go to 910

        allocate ( cval2(2*lenH), irow2(2*lenH), jcol2(2*lenH) )

        neH2 = 0
        do j = 1, neH
           neH2 = neH2 + 1
           cval2(neH2) = cval(j)
           irow2(neH2) = irow(j)
           jcol2(neH2) = jcol(j)

           if ( irow(j) /= jcol(j) ) then
              neH2 = neH2 + 1
              cval2(neH2) = cval(j)
              irow2(neH2) = jcol(j)
              jcol2(neH2) = irow(j)
           end if
        end do

        deallocate ( zero )

        nnH = maxval ( jcol(1:neH) )

        allocate ( indH(neH2), locH(nnH+1), valH(neH2), stat = alloc_stat )
        if ( alloc_stat /= 0 ) GO TO 990

        call crd2spr ( nnH, neH2, cval2, irow2, jcol2, valH, indH, locH )
        deallocate ( irow, jcol, cval )
        deallocate ( irow2, jcol2, cval2 )
     else
        nnH = 0
     end if

  else

     m  = 1
     nm = n+m

     allocate ( bl(nm), bu(nm), cObj(n), x(nm), hs(nm), pi(m), rc(nm),         &
                hEtype(nm), b(m), zero(n), stat = alloc_stat )
     if ( alloc_stat /= 0 ) GO TO 990
     hEtype = 0
     zero   = 0.0
     x      = 0.0

     call CUTEST_usetup_r ( status, iCutest, iOut, io_buffer,                  &
                            n, x(1:n), bl(1:n), bu(1:n) )
     if ( status /= 0 ) go to 910

     allocate ( vname(n), stat = alloc_stat )
     if ( alloc_stat /= 0 ) GO TO 990
     call CUTEST_unames_r ( status, n, Prob, vname )
     if ( status /= 0 ) go to 910
     deallocate ( vname )

     ! dummy row in A
     neA = n
     allocate ( indA(neA), locA(n+1), valA(neA), stat = alloc_stat )
     if ( alloc_stat /= 0 ) GO TO 990

     do j = 1, n
        indA(j) = 1
        valA(j) = 1.0_rp_
        locA(j) = j
     end do
     locA(n+1) = neA+1

     x(n+1)    =  0.0
     bl(n+1)   = -1.0E+20_rp_
     bu(n+1)   =  1.0E+20_rp_

     ! Objective
     ! uofg returns ObjAdd and cObj.
     call CUTEST_uofg_r( status, n, zero, ObjAdd, cObj, .true. )
     if ( status /= 0 ) go to 910

     ! Hessian of the objective
     call CUTEST_udimsh_r( status, lenH )
     if ( status /= 0 ) go to 910

     if ( lenH > 0 ) then
        allocate ( cval(lenH), irow(lenH), jcol(lenH), stat = alloc_stat )
        if ( alloc_stat /= 0 ) GO TO 990
        call CUTEST_ush_r( status, n, zero, neH, lenH, cval, irow, jcol )
        if ( status /= 0 ) go to 910

        allocate ( cval2(2*lenH), irow2(2*lenH), jcol2(2*lenH) )

        neH2 = 0
        do j = 1, neH
           neH2 = neH2 + 1
           cval2(neH2) = cval(j)
           irow2(neH2) = irow(j)
           jcol2(neH2) = jcol(j)

           if ( irow(j) /= jcol(j) ) then
              neH2 = neH2 + 1
              cval2(neH2) = cval(j)
              irow2(neH2) = jcol(j)
              jcol2(neH2) = irow(j)
           end if
        end do

        deallocate ( zero )

        nnH = maxval ( jcol(1:neH) )

        allocate ( indH(neH2), locH(nnH+1), valH(neH2), stat = alloc_stat )
        if ( alloc_stat /= 0 ) GO TO 990

        call crd2spr ( n, neH2, cval2, irow2, jcol2, valH, indH, locH )
        deallocate ( irow, jcol, cval )
        deallocate ( irow2, jcol2, cval2 )

     else
        nnH = 0
     end if
  end if


  !-----------------------------------------------------------------------------
  ! Ok, we're done with CUTEst stuff.
  !-----------------------------------------------------------------------------

  filename = trim(Prob)//'.out'
  open ( iPrint, file=filename, status='unknown' )
  open ( iSpecs, file='SQIC.SPC', form='formatted', status='old' )

  iObj  = 0
  ncObj = n
  nName = 1
  hs    = 0

  call ctSqic ( 'Cold', INFO, iPrint, iSumm, iSpecs, Prob,                     &
                 m, n, nnH, iObj, ObjAdd,                                      &
                 neA, indA, locA, valA, bl, bu, ncObj, cObj,                   &
                 nName, Names, hEtype, hs, x, pi, rc,                          &
                 neH, indH, locH, valH,                                        &
                 nS, nInf, sInf, Obj )

  call CUTEST_creport_r( status, CALLS, CPU )
  WRITE ( iOut, 2000 ) Prob, n, m, CALLS( 1 ), CALLS( 2 ),                     &
                      CALLS( 5 ), CALLS( 6 ), info, Obj, CPU( 1 ), CPU( 2 )

  deallocate ( bl, bu, cObj, x, pi, rc )
  deallocate ( hEtype, hs )
  deallocate ( indA, locA, valA )

  if ( nnH > 0 ) deallocate ( indH, locH, valH )

  close ( iSpecs )
  close ( iPrint )
  close ( iCutest )

  call CUTEST_cterminate_r( status )
  stop

  910 CONTINUE
  WRITE( iOut, "( ' CUTEst error, status = ', i0, ', stopping' )") status
  stop

  990 CONTINUE
  WRITE( iOut, "( ' Allocation error, status = ', i0 )" ) status
  stop

 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //                    &
          ,' Package used            :  SQIC',    /                            &
          ,' Problem                 :  ', A10,    /                           &
          ,' # variables             =      ', I10 /                           &
          ,' # constraints           =      ', I10 /                           &
          ,' # objective functions   =        ', F8.2 /                        &
          ,' # objective gradients   =        ', F8.2 /                        &
          ,' # constraints functions =        ', F8.2 /                        &
          ,' # constraints gradients =        ', F8.2 /                        &
          ,' Exit code               =      ', I10 /                           &
          ,' Final f                 = ', E15.7 /                              &
          ,' Set up time             =      ', 0P, F10.2, ' seconds' /         &
          ,' Solve time              =      ', 0P, F10.2, ' seconds' //        &
           66('*') / )

  end program sqic_main
