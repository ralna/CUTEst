! THIS VERSION: CUTEST 2.2 - 2023-11-23 AT 10:00 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

Program STATS_main

  USE CUTEST_KINDS_precision

  Implicit None

!  Statistics-collecting package for gathering information on
!  variables and constraints from SIF problems.

!  D. Orban, August 2005
!  CUTEst evolution Nick Gould January 2013

! Information pertaining to variables
  Type :: CUTEst_var_type
     Integer ( KIND = ip_ ) :: nvar
     Integer ( KIND = ip_ ) :: nfixed, nbelow, nabove, n2sided, nfree, nbnds
  End Type CUTEst_var_type

! Information pertaining to constraints
  Type :: CUTEst_con_type
     Integer ( KIND = ip_ ) :: ncon
     Integer ( KIND = ip_ ) :: nlle, nlge, nlrange, nleq, nlin
     Integer ( KIND = ip_ ) :: nnlle, nnlge, nnlrange, nnleq, nnlin
  End Type CUTEst_con_type

  Type :: CUTEst_db_type
! # free, fixed, 1-sided, 2-sided bounds
     Integer ( KIND = ip_ ) :: nfr, nfx, n1s, n2s
! # 1 and 2-sided ineq, equalities (linear)
     Integer ( KIND = ip_ ) :: m1sl, m2sl, meql
! # 1 and 2-sided ineq, equalities (general)
     Integer ( KIND = ip_ ) :: m1sg, m2sg, meqg
     Character( Len = 86 ) :: var_str
     Character( Len = 76 ) :: lcon_str, gcon_str
     Character( Len = 99 ) :: classf_db_str
  End Type CUTEst_db_type

! Dynamic allocation flag
  Integer ( KIND = ip_ ) :: alloc_stat, status

  Integer ( KIND = ip_ ), Parameter :: input = 47, out = 6
  INTEGER ( KIND = ip_ ) :: io_buffer = 11
  Integer ( KIND = ip_ ) :: i

  Real( Kind = rp_ ), Dimension( : ), Allocatable :: x, bl, bu, v, cl, cu
  Real( Kind = rp_ ), PARAMETER :: infty = REAL( 9.0D+19, KIND = rp_ )
  Character( len = 10 ) :: pname
  Integer ( KIND = ip_ ) :: e_order, l_order, v_order
  Logical, Dimension( : ), Allocatable :: equatn, linear
  Logical :: constrained

  Type( CUTEst_var_type ) :: vars
  Type( CUTEst_con_type ) :: cons
  Type( CUTEst_db_type  ) :: db

!  Open the relevant problem file.

  Open ( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED', STATUS = 'OLD' )
  Rewind input

!  Get problem dimensions and determine which tools to use

  Call CUTEST_cdimen_r( status, input, vars%nvar, cons%ncon )
  If( cons%ncon < 0 ) Then
     Close( input )
     Write( out, '(A)' ) 'Error reading OUTSDIF.d'
     Write( out, '(A21,1X,I6,1X,A3)' ) &
          'Number of constraints', cons%ncon, '< 0'
     Stop
  Endif
  constrained = (cons%ncon > 0)

!  Set up parameters

  e_order = 0 ; l_order = 1 ; v_order = 0

!  Allocate arrays to hold problem data

  Allocate( x( vars%nvar ),  STAT = alloc_stat )
  If( alloc_stat /= 0 ) Then
     Write( out, 3000 ) 'X', vars%nvar
     Goto 900
  Endif
  Allocate( bl( vars%nvar ), STAT = alloc_stat )
  If( alloc_stat /= 0 ) Then
     Write( out, 3000 ) 'BL', vars%nvar
     Goto 900
  Endif
  Allocate( bu( vars%nvar ), STAT = alloc_stat )
  If( alloc_stat /= 0 ) Then
     Write( out, 3000 ) 'BU', vars%nvar
     Goto 900
  Endif
  If( constrained ) Then
     Allocate( v( cons%ncon ), STAT = alloc_stat )
     If( alloc_stat /= 0 ) Then
        Write( out, 3000 ) 'V', cons%ncon
        Goto 900
     End If
     Allocate( cl( cons%ncon ), STAT = alloc_stat )
     If( alloc_stat /= 0 ) Then
        Write( out, 3000 ) 'CL', cons%ncon
        Goto 900
     End If
     Allocate( cu( cons%ncon ), STAT = alloc_stat )
     If( alloc_stat /= 0 ) Then
        Write( out, 3000 ) 'CU', cons%ncon
        Goto 900
     End If
     Allocate( equatn( cons%ncon ), STAT = alloc_stat )
     If( alloc_stat /= 0 ) Then
        Write( out, 3000 ) 'EQUATN', cons%ncon
        Goto 900
     End If
     Allocate( linear( cons%ncon ), STAT = alloc_stat )
     If( alloc_stat /= 0 ) Then
        Write( out, 3000 ) 'LINEAR', cons%ncon
        Goto 900
     End If

!  If all ok, initialize problem data

     Call CUTEST_csetup_r( status, input, out, io_buffer,                      &
                           vars%nvar, cons%ncon,                               &
                           x, bl, bu, v, cl, cu, equatn, linear,               &
                           e_order, l_order, v_order )
  Else
     Allocate( equatn( 1 ), STAT = alloc_stat )
     If( alloc_stat /= 0 ) Then
        Write( out, 3000 ) 'EQUATN', 1
        Goto 900
     End If
     Allocate( linear( 1 ), STAT = alloc_stat )
     If( alloc_stat /= 0 ) Then
        Write( out, 3000 ) 'LINEAR', 1
        Goto 900
     End If
     Call CUTEST_usetup_r( status, input, out, io_buffer, vars%nvar, x, bl, bu )
  Endif

!  Obtain problem name.

  Call CUTEST_probname_r( status, pname )

!  Initialize data on variables

  vars%nfixed  = 0 ; vars%nbelow  = 0
  vars%nabove  = 0 ; vars%n2sided = 0
  vars%nbnds   = 0 ; vars%nfree   = 0 

!  Initialize data on constraints

  cons%nlle    = 0 ; cons%nlge     = 0
  cons%nleq    = 0 ; cons%nlin     = 0
  cons%nnlle   = 0 ; cons%nnlge    = 0
  cons%nnleq   = 0 ; cons%nnlin    = 0
  cons%nlrange = 0 ; cons%nnlrange = 0

!  Obtain info on the variables

  Do i = 1, vars%nvar
     If( (bl(i) > -INFTY .Or. bu(i) < INFTY) .And. bl(i) /= bu(i) ) &
          vars%nbnds = vars%nbnds + 1
     If( bl(i) > -INFTY .And. bu(i) >= INFTY ) Then
        vars%nbelow = vars%nbelow + 1
     Else If( bl(i) <= -INFTY .And. bu(i) < INFTY ) Then
        vars%nabove = vars%nabove + 1
     Else If( bl(i) > -INFTY .And. bu(i) < INFTY ) Then
        If( bl(i) == bu(i) ) Then
           vars%nfixed = vars%nfixed + 1
        Else
           vars%n2sided = vars%n2sided + 1
        End If
     Else
        vars%nfree = vars%nfree + 1
     End If
  End Do

!  Obtain info on the constraints

  Do i = 1, cons%ncon
     If( linear(i) ) Then
      ! Process linear constraint
        If( equatn(i) ) Then
           cons%nleq = cons%nleq + 1
        Else
           If( cl(i) > -INFTY .And. cu(i) >= INFTY ) Then
              cons%nlge = cons%nlge + 1
           Else If( cl(i) <= -INFTY .And. cu(i) < INFTY ) Then
              cons%nlle = cons%nlle + 1
           Else If( cl(i) > -INFTY .And. cu(i) < INFTY ) Then
              cons%nlrange = cons%nlrange + 1
           Else
              Write( out, * ) 'Oops!'
              Goto 900
           End If
        End If
     Else
      ! Process nonlinear constraint
        If( equatn(i) ) Then
           cons%nnleq = cons%nnleq + 1
        Else
            If( cl(i) > -INFTY .And. cu(i) >= INFTY ) Then
              cons%nnlge = cons%nnlge + 1
           Else If( cl(i) <= -INFTY .And. cu(i) < INFTY ) Then
              cons%nnlle = cons%nnlle + 1
           Else If( cl(i) > -INFTY .And. cu(i) < INFTY ) Then
              cons%nnlrange = cons%nnlrange + 1
           Else
              Write( out, * ) 'Oops!'
              Goto 900
           End If
        End If
     End If
  End Do

!  Assemble classification strings
!  We use the scheme

!  * CLASSIFICATION  probname XXXn-XX-n-m
!  * CLASSIFICATION  probname VARIABLES           nfx  n1s  n2s
!  * CLASSIFICATION  probname LINEAR CONSTRAINTS  m1sl m2sl meql
!  * CLASSIFICATION  probname GENERAL CONSTRAINTS m1sg m2sg meqg
  db%nfr = vars%nfree
  db%nfx = vars%nfixed
  db%n1s = vars%nbelow + vars%nabove
  db%n2s = vars%n2sided
  Write( db%var_str, '(A18,A8,A21,I9,3(1X,I9))' )                              &
       '* CLASSIFICATION  ', pname(1:8), ' VARIABLES           ',              &
       db%nfr, db%n1s, db%n2s, db%nfx

  db%m1sl = cons%nlle + cons%nlge
  db%m2sl = cons%nlrange
  db%meql = cons%nleq
  Write( db%lcon_str, '(A18,A8,A21,I9,1X,I9,1X,I9)' )                          &
       '* CLASSIFICATION  ', pname(1:8), ' LINEAR CONSTRAINTS  ',              &
       db%m1sl, db%m2sl, db%meql

  db%m1sg = cons%nnlle + cons%nnlge
  db%m2sg = cons%nnlrange
  db%meqg = cons%nnleq
  Write( db%gcon_str, '(A18,A8,A21,I9,1X,I9,1X,I9)' )                          &
       '* CLASSIFICATION  ', pname(1:8), ' GENERAL CONSTRAINTS ',              &
       db%m1sg, db%m2sg, db%meqg
  Write( db%classf_db_str, '(I9,9(1X,I9))' )                                   &
       db%nfr,  db%n1s,  db%n2s, db%nfx, db%m1sl, db%m2sl, db%meql,            &
       db%m1sg, db%m2sg, db%meqg
  
!Write( out, * ) 'Database classification strings:'
!Write( out, '(A86)' ) db%var_str
!Write( out, '(A76)' ) db%lcon_str
!Write( out, '(A76)' ) db%gcon_str
! Write( out, '(A99)' ) db%classf_db_str
! Goto 900

!  Display collected data

  Write ( out, 2000 ) pname, vars%nvar, vars%nbnds, vars%nbelow,              &
       vars%nabove, vars%n2sided, vars%nfixed, vars%nfree,                     &
       cons%ncon,                                                              &
       cons%nlin, cons%nleq, cons%nlle, cons%nlge, cons%nlrange,               &
       cons%nnlin, cons%nnleq, cons%nnlle, cons%nnlge, cons%nnlrange,          &
       cons%nleq + cons%nnleq,                                                 &
       cons%nlle+cons%nlge+2*cons%nlrange+cons%nnlle+cons%nnlge+2*cons%nnlrange

!  Terminate

900 Continue

!  Close the problem file

  Close( input )
  CALL CUTEST_cterminate_r( status )

!  Free allocated memory

  If( Allocated( x ) ) Deallocate( x )
  If( Allocated( bl ) ) Deallocate( bl )
  If( Allocated( bu ) ) Deallocate( bu )
  If( Allocated( equatn ) ) Deallocate( equatn )
  If( Allocated( linear ) ) Deallocate( linear )
  If( constrained ) Then
     If( Allocated( v ) ) Deallocate( v )
     If( Allocated( cl ) ) Deallocate( cl )
     If( Allocated( cu ) ) Deallocate( cu )
  End If

!  Exit

  Stop

  CALL CUTEST_uterminate_r( status )
  STOP

!  Non-executable statements.

!  The following is the complete standard statistics output format: select
!  the items that are relevant to the type of problems solved and adapt the
!  name of the code. It is broken in two to comply with compilers
!  which want to see no more than 19 continuation lines.

2000 Format( /, 24('='), ' Problem statistics ', 24('=') //, &
          ' Code used                :       STATS',    /, &
          ' Problem                  :      ', A10,     /, &
          ' # variables              =      ', I10 /, &
          ' #   bounded              =      ', I10 /, &
          ' #     below only         =      ', I10 /, &
          ' #     above only         =      ', I10 /, &
          ' #     below and above    =      ', I10 /, &
          ' #   fixed                =      ', I10 /, &
          ' #   free                 =      ', I10 /, &
          ' # constraints            =      ', I10 /, &
          ' #   linear               =      ', I10 /, &
          ' #     equalities         =      ', I10 /, &
          ' #     <= inequalities    =      ', I10 /, &
          ' #     >= inequalities    =      ', I10 /, &
          ' #     range              =      ', I10 /, &
          ' #   nonlinear            =      ', I10 /, &
          ' #     equalities         =      ', I10 /, &
          ' #     <= inequalities    =      ', I10 /, &
          ' #     >= inequalities    =      ', I10 /, &
          ' #     range              =      ', I10 /, &
          ' # equality constraints   =      ', I10 /, &
          ' # inequality constraints =      ', I10 /, &
          '   (ranges count as two)',              /, &
          68('=') )
3000 Format( /, 'Error allocating array ', A6, ', dim = ', I6 )

End Program STATS_main

