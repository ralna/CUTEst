!     ( Last modified on 3 Jan 2013 at 16:20:00 )

PROGRAM GEN90_main

  USE Generic_Driver

!  Generic package driver (example) for applying package GEN90 to problems
!  from SIF files. This driver also demonstrates how to dynamically
!  allocate arrays to be used with CUTEst.
!
!  D. Orban, August 2002, strongly inspired by Philippe's original driver.
!  CUTEst evolution, Nick Gould January 2013
  Implicit None
  INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )
  INTEGER :: n, m, nlin, neq, nbnds, exitcode, status
  INTEGER :: io_buffer = 11
  INTEGER, PARAMETER :: inspec = 46, input = 47, out = 6
  REAL ( KIND = wp ) :: DUMMY
  REAL ( KIND = wp ), Dimension( : ), Allocatable :: X, BL, BU, V, CL, CU, C
  REAL ( KIND = wp ), DIMENSION( 2 ) :: CPU( 4 )
  REAL ( KIND = wp ), DIMENSION( 7 ) :: CALLS( 7 )
  CHARACTER( LEN = 10 ) ::  PNAME
  CHARACTER( LEN = 10 ), Dimension( : ), Allocatable :: VNAMES, GNAMES
  LOGICAL, DIMENSION( : ), ALLOCATABLE :: EQUATN, LINEAR
  LOGICAL ::  constrained
!
!  Open the Spec file for the method (typically called METHOD.SPC)
!
  Call GENSPC( inspec, 'GEN.SPC' )
!
!  Open the relevant problem file.
!
  OPEN( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED', STATUS = 'OLD' )
  REWIND input
!
!  Get problem dimensions and determine which tools to use
!
  constrained = .FALSE.
  CALL CUTEST_cdimen( status, input, n, m )
  IF ( status /= 0 ) GO TO 910
  If ( m > 0 ) Then
    constrained = .TRUE.
  ELSE IF ( m < 0 ) THEN
    Write( 6, '(A)' ) 'Error reading OUTSDIF.d'
    Stop
  END IF

!  Set up SIF data from the problem file

  ALLOCATE( X( n ), BL( n ), BU( n ) )
  If( CONSTRAINED ) Then
     ALLOCATE( V( m+1 ), CL( m+1 ), CU( m+1 ), EQUATN( m+1 ), LINEAR( m+1 ) )
     Call CUTEST_csetup( status, input, out, io_buffer, n, m, X, BL, BU,       &
          V, CL, CU, EQUATN, LINEAR, 1, 0, 0 )
  Else
     ALLOCATE( EQUATN( 0 ), LINEAR( 0 ) )
     Call CUTEST_usetup( status, input, out, io_buffer, n, X, BL, BU )
  Endif
  IF ( status /= 0 ) GO TO 910

!  Obtain problem/variables/constraints names.

  ALLOCATE( VNAMES( n ) )
  IF ( constrained ) THEN
     Allocate( GNAMES( m ) )
    CALL CUTEST_cnames( status, n, m, pname, VNAMES, GNAMES )
  ELSE
    CALL CUTEST_unames( status, n, pname, VNAMES )
  END IF
  IF ( status /= 0 ) GO TO 910

!  Obtain info on the problem

  nlin  = 0 ; neq   = 0 ; nbnds = 0
  If ( constrained ) Then
    CALL GETINFO( n, m, BL, BU, EQUATN, LINEAR, nlin, neq, nbnds )
  Else
!    EQUATN( 1 ) = .False.
!    LINEAR( 1 ) = .False.
    CALL GETINFO( n, 0, BL, BU, EQUATN, LINEAR, nlin, neq, nbnds )
  Endif

!  Call the "optimizer".

  CALL GEN( dummy )
  exitcode = 0

!  Get the function value at a trial point

  X = 0.0_wp ; x( 1 ) = 1.0_wp
  dummy = 0.0D+0
  If ( constrained ) Then
    ALLOCATE( C( m ) )
    CALL CUTEST_cfn( status, n, m, X, dummy, C )
    Write(6,*) ' CUTEST_cfn: F(x0) = ', dummy
    Write(6,*) ' CUTEST_cfn: C(x0) = ', C
    DEALLOCATE( C )
  ELSE
    CALL CUTEST_ufn( status, n, X, dummy )
    Write(6,*) ' CUTEST_ufn: F(x0) = ', dummy
  END IF
  IF ( status /= 0 ) GO TO 910

!  Close the problem file

  Close( INPUT )

!  Write the standard statistics (of which some may be irrelevant)

!    CALLS( 1 ): number of calls to the objective function
!    CALLS( 2 ): number of calls to the objective gradient
!    CALLS( 3 ): number of calls to the objective Hessian
!    CALLS( 4 ): number of Hessian times vector products
!           --constrained problems only--
!    CALLS( 5 ): number of calls to the constraint functions
!    CALLS( 6 ): number of calls to the constraint gradients
!    CALLS( 7 ): number of calls to the constraint Hessians
!           -----------------------------

!    CPU( 1 ) : CPU time (in seconds) for USETUP or CSETUP
!    CPU( 2 ) : CPU time ( in seconds) since the end of USETUP or CSETUP

!  Note that each constraint function is counted separately.
!  Evaluating all the constraints thus results in PNC evaluations, where
!  PNC is the number of constraints in the problem.  Note that PNC does not
!  include repetitions for constraints having full ranges.
  
!  (N, is the dimension of the problem, M is the number of constraints,
!   DUMMY is the final value of the objective function)

  IF ( constrained ) THEN
     CALL CUTEST_creport( status, CALLS, CPU )      
  ELSE
     CALL CUTEST_ureport( status, CALLS, CPU )
  ENDIF
  IF ( status /= 0 ) GO TO 910
  WRITE ( out, 2000 ) pname, n, m, nlin, neq, m-neq, nbnds,                    &
     CALLS( 1 ), CALLS( 2 ), CALLS( 3 )
  IF ( constrained ) WRITE( out, 2010 ) CALLS( 5 ), CALLS( 6 ), CALLS( 7 )
  WRITE ( out, 2020 ) exitcode, dummy, CPU( 1 ), CPU( 2 ) 
!
!  Free allocated memory
!
  Deallocate( X, BU, BL, VNAMES, EQUATN, LINEAR )
  If( CONSTRAINED ) Deallocate( V, CL, CU, GNAMES )
!
!  Exit
!
   CALL CUTEST_uterminate( status )
   STOP

 910 CONTINUE
   WRITE( out, "( ' CUTEst error, status = ', i0, ', stopping' )" )  status
   STOP
!
!  Non-executable statements.
!
!  The following is the complete standard statistics output format: select
!  the items that are relevant to the type of problems solved and adapt the
!  name of the code. It is broken in two to comply with compilers
!  which want to see no more than 19 continuation lines.
!
2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //,                    &
          ' Package used             :  GEN90',    /,                          &
          ' Variant                  :  name of a variant, if needed',/,       &
          ' Problem                  :  ', A10,    /,                          &
          ' # variables              =      ', I10 /,                          &
          ' # constraints            =      ', I10 /,                          &
          ' # linear constraints     =      ', I10 /,                          &
          ' # equality constraints   =      ', I10 /,                          &
          ' # inequality constraints =      ', I10 /,                          &
          ' # bounds                 =      ', I10 /,                          &
          ' # objective functions    =        ', F8.2 /,                       &
          ' # objective gradients    =        ', F8.2 /,                       &
          ' # objective Hessians     =        ', F8.2 )
2010 FORMAT( ' # constraints functions  =        ', F8.2 /                     &
          ,' # constraints gradients  =        ', F8.2 /                       &
          ,' # constraints Hessians   =        ', F8.2 )
2020 FORMAT( ' Exit code                =      ', I10 /,                       &
             ' Final f                  = ', E15.7 /,                          &
             ' Set up time              =      ', 0P, F10.2, ' seconds'/       &
             ' Solve time               =      ', 0P, F10.2, ' seconds'//      &
          66('*') / )
END PROGRAM GEN90_main

