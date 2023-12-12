! THIS VERSION: CUTEST 2.2 - 2023-11-23 AT 10:15 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

      PROGRAM GENMA

      USE CUTEST_KINDS_precision
C      
C  Generic package driver (example) for applying package GEN to problems
C  from SIF files.
C
C  Ph. Toint, December 2000 / D. Orban, August 2002 / Nick Gould January 2013
C
      IMPLICIT NONE
      INTEGER ( KIND = ip_ ) :: n, m, status
      INTEGER ( KIND = ip_ ) :: io_buffer = 11
      INTEGER ( KIND = ip_ ), PARAMETER :: inspec = 46, input = 47
      INTEGER ( KIND = ip_ ), PARAMETER :: iout = 6
      INTEGER ( KIND = ip_ ) :: nlin, neq, nbnds, exitcode
      LOGICAL :: constrained
      CHARACTER ( LEN = 10 ) :: PNAME
      REAL ( KIND = rp_ ) dummy, CPU( 4 ), CALLS( 7 )
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: X, BL, BU
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: V, CL, CU
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: VNAMES
      CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : )  :: GNAMES
      LOGICAL, ALLOCATABLE, DIMENSION( : ) :: EQUATN, LINEAR
C
C  Open the Spec file for the method (typically called METHOD.SPC)
C
      CALL GENSPC( INSPEC, 'GEN.SPC' )
C
C  Open the relevant problem file.
C
      OPEN ( INPUT, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     *       STATUS = 'OLD' )
      REWIND INPUT
C
C  Get problem dimensions and determine which tools to use
C
      CALL CUTEST_cdimen_r( status, input, n, m )
      IF ( status /= 0 ) GO TO 910

      ALLOCATE( X( n ), BL( n ), BU( n ), V( m ), CL( m ), 
     *          CU( m ), EQUATN( m ), LINEAR( m ), VNAMES( n ), 
     *          GNAMES( m ), STAT = status )
      IF ( status /= 0 ) GO TO 990

      IF ( m == 0 ) THEN
        constrained = .FALSE.
      ELSE IF ( m > 0 ) THEN
        constrained = .TRUE.
      ELSE
        WRITE( 6, '(A)' ) 'Error reading OUTSDIF.d'
        STOP
      END IF
C
C  Set up SIF data from the problem file
C
      IF ( constrained ) THEN
        CALL CUTEST_csetup_r( status, input, iout, io_buffer, n, m,
     *        X, BL, BU, V, CL, CU, EQUATN, LINEAR, 1, 0, 0 )
      ELSE
        CALL CUTEST_usetup_r( status, input, iout, io_buffer, n, 
     *        X, BL, BU )
      ENDIF
      IF ( status /= 0 ) GO TO 910
C
C  Obtain problem/variables/constraints names.
C
      IF ( constrained ) THEN
         CALL CUTEST_cnames_r( status, n, m, pname, VNAMES, GNAMES )
      ELSE
         CALL CUTEST_unames_r( status, n, pname, VNAMES )
      ENDIF
      IF ( status /= 0 ) GO TO 910
C
C  Obtain info on the problem
C
      nlin  = 0
      neq   = 0
      nbnds = 0
      IF ( constrained ) THEN
         CALL GETINFO( n, m, BL, BU, EQUATN, LINEAR, nlin, neq, nbnds )
      ELSE
C         EQUATN( 1 ) = .FALSE.
C         LINEAR( 1 ) = .FALSE.
         CALL GETINFO( n, 0, BL, BU, EQUATN, LINEAR, nlin, neq, nbnds )
      ENDIF
C
C  Call the optimizer.
C
      CALL GEN( dummy )
      exitcode = 0
C
C  Close the problem file
C
      CLOSE( input  )
C
C  Write the standard statistics (of which some may be irrelevant)
C
C    CALLS( 1 ): number of calls to the objective function
C    CALLS( 2 ): number of calls to the objective gradient
C    CALLS( 3 ): number of calls to the objective Hessian
C    CALLS( 4 ): number of Hessian times vector products
C           --constrained problems only--
C    CALLS( 5 ): number of calls to the constraint functions
C    CALLS( 6 ): number of calls to the constraint gradients
C    CALLS( 7 ): number of calls to the constraint Hessians
C           -----------------------------
C
C    CPU( 1 ) : CPU time (in seconds) for USETUP or CSETUP
C    CPU( 2 ) : CPU time ( in seconds) since the end of USETUP or CSETUP
C
C  Note that each constraint function is counted separately.
C  Evaluating all the constraints thus results in PNC evaluations, where
C  PNC is the number of constraints in the problem.  Note that PNC does not
C  include repetitions for constraints having full ranges.
      
C  (N, is the dimension of the problem, M is the number of constraints,
C   DUMMY is the final value of the objective function)
C
      IF ( constrained ) THEN
        CALL CUTEST_creport_r( status, CALLS, CPU )      
      ELSE
        CALL CUTEST_ureport_r( status, CALLS, CPU )
      ENDIF
      IF ( status /= 0 ) GO TO 910
      WRITE ( iout, 2000 ) pname, n, m, nlin, neq, m-neq, nbnds,
     *     CALLS( 1 ), CALLS( 2 ), CALLS( 3 )
      IF ( constrained ) WRITE( iout, 2010 ) 
     *     CALLS( 5 ), CALLS( 6 ), CALLS( 7 )
      WRITE ( iout, 2020 ) exitcode, dummy, CPU( 1 ), CPU( 2 ) 
C
C  Exit
C
      STOP

  910 CONTINUE
      WRITE( iout, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     *   status
      STOP

  990 CONTINUE
      WRITE( iout, "( ' Allocation error, status = ', I0 )" ) status
      STOP
C
C  Non-executable statements.
C
C  The following is the complete standard statistics output format: select
C  the items that are relevant to the type of problems solved and adapt the
C  name of the code.
C
C  The only reason for breaking the format in two is for compilers
C  which do not accept more than 19 continuation lines.
C
 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     *    ,' Package used             :  GEN',    /
     *    ,' Variant                  :  name of a variant, if needed',/
     *    ,' Problem                  :  ', A10,    /
     *    ,' # variables              =      ', I10 /
     *    ,' # constraints            =      ', I10 /
     *    ,' # linear constraints     =      ', I10 /
     *    ,' # equality constraints   =      ', I10 /
     *    ,' # inequality constraints =      ', I10 /
     *    ,' # bounds                 =      ', I10 /
     *    ,' # objective functions    =        ', F8.2 /
     *    ,' # objective gradients    =        ', F8.2 / 
     *    ,' # objective Hessians     =        ', F8.2 )
 2010 FORMAT( ' # constraints functions  =        ', F8.2 /
     *    ,' # constraints gradients  =        ', F8.2 /
     *    ,' # constraints Hessians   =        ', F8.2 )
 2020 FORMAT(
     *     ' Exit code                =      ', I10 /
     *    ,' Final f                  = ', E15.7 /
     *    ,' Set up time              =      ', 0P, F10.2, ' seconds'/
     *     ' Solve time               =      ', 0P, F10.2, ' seconds'//
     *     66('*') / )
      END

