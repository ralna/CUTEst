!     ( Last modified on 3 Jan 2013 at 14:40:00 )
!  Main CUTEst driver for DFO 2.0.0.
!  Original version by K. C. Dang, 2008
!  Fortran 90 version by D. Orban, 2009
!  Revised for CUTEst, Nick Gould, January 2013

Program DFO_main
! Use CUTEst_precis
! Use CUTEst_interfaces
  Implicit None

!  Variable declarations

  Integer :: N, M, NCLIN, NCNLN, NLIN, NEQ, NBNDS, status
  INTEGER :: io_buffer = 11
  Integer, Parameter :: INDR = 46
  INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )
  Real(Kind = wp), Dimension(:), Allocatable :: X0, BL, BU, V, CL, CU
  Logical, Dimension(:), Allocatable :: EQUATN, LINEAR
  Logical :: IFINIV, constrained
  Character(Len = 10) :: pname
  Character(Len = 10), Dimension(:), Allocatable :: VNAMES, GNAMES
  Character(Len = 256) :: pname_256
  Character(Len = 256), Dimension(:), Allocatable :: VNAMES_256, GNAMES_256

!     - Variables for I/O
  Integer, Parameter :: out=6, INPUT=47, INSPEC=198, REPRTOUT=1812
!     - Variables for algorithm parameter
  Integer :: NX, MAXIT, MAXNF,STPCRTR, IPRINT, SCALE
!      LOGICAL IFINTV
  Real(Kind = wp) :: DELMIN, DELTA, CNSTOL, PP, STPTHR
!     - Variables for CUTEst report
  Real(Kind = wp) :: CPU(4), CALLS(7)
!     - Variables for working space
  Integer :: LDA
  Integer :: IT, NF, INFO
  Integer :: I
  Real(Kind = wp) :: F0
  Real(Kind = wp), Dimension(:), Allocatable :: X, FX, C, CONX, LB, UB, ALIN

!  Open data file.

  Open(INPUT, FILE = 'OUTSDIF.d', FORM = 'FORMATTED', STATUS = 'OLD')
  Rewind INPUT

!  Allocate working vectors.

  constrained = .False.
  Call CUTEST_cdimen( status, input, n, m )
  IF ( status /= 0 ) GO TO 910
  If( M > 0 ) Then
     constrained = .True.
  Else If( M < 0 ) Then
     Write( 6, '(A)' ) 'Error reading OUTSDIF.d'
     Stop
  Endif

!  Set up SIF data.

  Allocate(X0(N), BL(N), BU(N),  STAT = status )
  IF ( status /= 0 ) GO TO 990
  If( constrained ) Then
     Allocate(C(M), V(M), CL(M), CU(M), EQUATN(M), LINEAR(M), STAT = status )
     IF ( status /= 0 ) GO TO 990
     Call CUTEST_csetup( status, INPUT, out, io_buffer,N, M, X0, BL, BU,      &
                         V, CL, CU, EQUATN, LINEAR, 0, 1, 0 )
  Else
     Allocate(C(0), EQUATN(0), LINEAR(0), STAT = status )
     IF ( status /= 0 ) GO TO 990
     Call CUTEST_usetup( status, INPUT, out, io_buffer, N, X0, BL, BU )
  Endif
  IF ( status /= 0 ) GO TO 910

!  Obtain problem, variables and constraint names.

  Allocate( VNAMES( n ), VNAMES_256( n ), STAT = status)
  IF ( status /= 0 ) GO TO 990
  If( constrained ) Then
    Allocate( GNAMES( m ),  GNAMES_256( m ), STAT = status)
    IF ( status /= 0 ) GO TO 990
    Call CUTEST_cnames( status, n, m, pname, VNAMES, GNAMES )
    DO i = 1, 10
      GNAMES_256( 1 : m ) ( i : i ) = GNAMES( 1 : m )( i : i )
    END DO
  Else
    Allocate( GNAMES_256( 0 ), STAT = status)
    Call CUTEST_unames( status, n, pname, VNAMES )
  Endif
  DO i = 1, 10
    pname_256( i : i ) = pname( i : i )
    VNAMES_256( 1 : n ) ( i : i ) = VNAMES( 1 : n )( i : i )
  END DO

  IF ( status /= 0 ) GO TO 910

!  Obtain info on the problem

  NLIN  = 0 ; NEQ   = 0 ; NBNDS = 0
  If( constrained ) Then
     Call GETINFO( N, M, BL, BU, EQUATN, LINEAR, NLIN, NEQ, NBNDS )
  Else
     Call GETINFO( N, 0, BL, BU, EQUATN, LINEAR, NLIN, NEQ, NBNDS )
  Endif

!  Treat all constraints as derivative free (ignore linear constraints)

  NCLIN = 0
  NCNLN = 0
  LDA = 1
  Allocate(LB(N + NCLIN + NCNLN + M))
  Allocate(UB(N + NCLIN + NCNLN + M))
  LB( 1 : n ) = BL( 1 : n )
  UB( 1 : n ) = BU( 1 : n )
  If( constrained ) Then
     LB( n + 1 : n + m ) = CL( 1 : m )
     UB( n + 1 : n + m ) = CU( 1 : m )
  Endif

!  Read algorithm specification

  Open(INSPEC, FILE = 'DFO.SPC', FORM = 'FORMATTED', STATUS='OLD')
  Rewind INSPEC
  Read(INSPEC, 1000) NX, MAXIT, MAXNF, STPCRTR, DELMIN, STPTHR, CNSTOL, &
       DELTA, PP, SCALE, IPRINT
  Close(INSPEC)
  If( NX >= 2 ) Then
     Write(out,*) 'NX >= 2 not currently supported; Check spec file.'
     Goto 999
  Endif

!  Allocate space for initial points supplied

  Allocate(FX(NX))
  Allocate(X(N*NX))
  If( constrained ) THEN
    Allocate(CONX(M*NX))
  ELSE
    Allocate(CONX(0))
  END IF
  Allocate(ALIN(n))   ! Not used
  IFINIV = (NX >= 2)

!  Evaluate initial objective and constraint values

  If( constrained ) Then
     Call CUTEST_cfn( status, n, m, X0, f0, C )
  Else
     Call CUTEST_ufn( status, n, X0, f0 )
  Endif
  IF ( status /= 0 ) GO TO 910
  If( NX == 1 ) Then
     X( 1 : n ) = X0( 1 : n )
     FX(1) = F0
     If( constrained ) CONX( 1 : m ) = C( 1 : m )
  Else
     If( constrained ) Then
        Do I = 1, NX
           Call CUTEST_cfn( status, n, m, X((I-1)*N + 1:I*N), FX(I),           &
                            CONX((I-1)*M + 1:I*M))
           IF ( status /= 0 ) GO TO 910
        End Do
     Else
        Do I = 1, NX
           Call CUTEST_ufn( status,N, X((I-1)*N + 1:I*N), FX(I))
           IF ( status /= 0 ) GO TO 910
        End Do
     Endif
  Endif

!  Call main DFO routine

  Call DFO(N, NX, X, N, FX, CONX, IFINIV, M, C ,NCLIN, NCNLN, LB, UB ,         &
       ALIN , LDA , VNAMES_256, PNAME_256, GNAMES_256,                         &
       IT, NF, INFO, MAXIT,  MAXNF,                                            &
       STPCRTR, DELMIN, STPTHR, CNSTOL, DELTA, PP, SCALE, out, IPRINT)

!  Write out statistics

  Call CUTEST_creport( status,CALLS, CPU)
  IF ( status /= 0 ) GO TO 910
  Write(out, 2000) PNAME, N, M, NLIN, NEQ, M-NEQ, NBNDS, CALLS(1), CALLS(5)
  Write(out, 2001) INFO, FX(1), CPU(1), CPU(2)

!  Write select statistics to file

  Open(REPRTOUT, FILE = 'cutest.log', FORM = 'FORMATTED')
  Write(REPRTOUT, 2002) PNAME, N, M, IT, Int(CALLS(1)), Int(CALLS(5)), &
       INFO, FX(1), F0, CPU(1), CPU(2)
  Close(REPRTOUT)
999 Continue
  Close(INPUT)

!  Free allocated space

  Deallocate(X0)
  Deallocate(BL)
  Deallocate(BU)
  Deallocate(VNAMES)
  If( constrained ) Then
     Deallocate(EQUATN)
     Deallocate(LINEAR)
     Deallocate(V)
     Deallocate(CL)
     Deallocate(CU)
     Deallocate(GNAMES)
     Deallocate(C)
     Deallocate(CONX)
  Endif
  Deallocate(LB)
  Deallocate(UB)
  Deallocate(X)
  Deallocate(FX)
  Deallocate(ALIN)
  CALL CUTEST_uterminate( status )
  STOP

  910 CONTINUE
      WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )") status
      STOP

  990 CONTINUE
      WRITE( out, "( ' Allocation error, status = ', I0 )" ) status
      STOP


!  Non-executable statements

1000 Format(4(I10, /), 5(D10.3, /), 1(I10, /), I10)
2000 Format( /, 24('*'), ' CUTEst statistics ', 24('*') // &
          ,' Package used             :  DFO',    / &
          ,' Problem                  :  ', A10,    / &
          ,' # variables              =      ', I10 / &
          ,' # constraints            =      ', I10 / &
          ,' # linear constraints     =      ', I10 / &
          ,' # equality constraints   =      ', I10 / &
          ,' # inequality constraints =      ', I10 / &
          ,' # bounds                 =      ', I10 / &
          ,' # objective functions    =        ', F8.2 / &
          ,' # constraints functions  =        ', F8.2 )

2001 Format(                                          &
          ' Exit code                =      ', I10 / &
          ,' Final f                  = ', D15.7 / &
          ,' Set up time              =      ', 0P, F10.2, ' seconds'/ &
          ' Solve time               =      ', 0P, F10.2, ' seconds'// &
          66('*') / )

2002 Format(' Package used        : RNAME    : C : DFO',/ &
       ,' Problem                 : PNAME    : C : ', A15 ,/ &
       ,' # variables             : NVAR     : I : ', I15 ,/ &
       ,' # constraints           : NCON     : I : ', I15 ,/  &
       ,' # iterations            : NITER    : I : ', I15 ,/  &
       ,' # objective functions   : NFEVAL   : I : ', I15 ,/ &
       ,' # objective gradients   : NCEVAL   : I : ', I15 ,/  &
       ,' Exit code               : EXITCODE : I : ', I15 ,/ &
       ,' Final f                 : FVAL     : F : ', E15.7 ,/ &
       ,' Initial f               : FZERO    : F : ', E15.7 ,/ &
       ,' Set up time (in second) : PTIME    : F : ', 0P,F15.7 ,/ &
       ,' Solve time (in second)  : STIME    : F : ', 0P,F15.7 ,/)
End Program DFO_main

!==============================================================================

Subroutine FUN(N, M, X, F, C, IFERR)

!  Evaluate objective and constraint values at X

  Implicit None
  INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )
  Integer, Intent(In) :: N, M
  Real(Kind=wp), Dimension(N), Intent(In) :: X
  Real(Kind=wp), Dimension(M), Intent(Out) :: C
  Real(Kind=wp), Intent(Out) :: F
  Logical, Intent(Out) :: IFERR
!Intrinsic :: ISNAN         ! Only in GFortran >= 4.3

  Integer :: i, status

  IFERR = .False.

  If( M > 0 ) Then
     Call CUTEST_cfn( status, n, m, X, f, C )
  Else
     Call CUTEST_ufn( status, n, X, f)
  Endif
  IF ( status /= 0 ) THEN
    Write(6,*) 'CUTEst : evaluation failed with status = ', status
    IFERR = .True.
    GO TO 3000
  END IF
  If( F /= F ) Then           ! If( ISNAN(F) ) Then
     IFERR = .True.
     Write(6,*) 'CUTEst : Function value is NaN!'
     Write(6,*) 'X = ', X
     Goto 3000
  Endif
  Do i = 1, M
      If( C(i) /= C(i) ) Then ! If( ISNAN(C(i)) ) Then
        IFERR = .True.
        Write(6,*) 'CUTEst : Constraint value is NaN!'
        Write(6,*) 'X = ', X
        Goto 3000
     Endif
  End Do
3000 Continue
  Return
End Subroutine FUN

!==============================================================================

  Subroutine GETINFO(N, M, BL, BU, EQUATN, LINEAR, NLIN, NEQ, NBNDS)

! Input/Output variables

    Implicit None

    INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )
    Real( Kind = wp ), Parameter :: INFTY = 1.0D+19
    Integer, Intent( IN  ) :: N, M
    Integer, Intent( OUT ) :: NLIN, NEQ, NBNDS
    Real( Kind = wp ), Dimension( N ), Intent( IN ) :: BL, BU
    Logical, Dimension( M ), Intent( IN ) :: EQUATN, LINEAR

!     Local variables

    Integer :: I

    NLIN  = 0 ; NEQ   = 0 ; NBNDS = 0

    Do I = 1, M
       If( EQUATN( I ) ) NEQ  = NEQ  + 1
       If( LINEAR( I ) ) NLIN = NLIN + 1
    End Do

    Do I = 1, N
       If( BL(I) > -INFTY .And. BU(I) < INFTY ) Then
          NBNDS = NBNDS + 2
       Else If( BL(I) > -INFTY .Or. BU(I) < INFTY ) Then
          NBNDS = NBNDS + 1
       Endif
    End Do
  End Subroutine GETINFO

!==============================================================================
