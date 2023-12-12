! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

C  this is based on an amalgam of driver.f and user.f from the 

C  filterSD source distribution

      PROGRAM filtersd_main
      
C     --------------------------------------------------------------
C     Solve NLP problems of the form
C     
C          minimize    f(x)
C          subject to  l_j <= c_j(x) <= u_j  ,    j = 1 , ... , m
C                      l_i <=   x_i  <= u_i  ,    i = 1 , ... , n
C
C     The problems solved are defined using CUTEst.
C     --------------------------------------------------------------

C  CUTEr interface by Roger Fletcher (U. Dundee)
C  Revised for CUTEst, Nick Gould, January 2013

      USE CUTEST_KINDS_precision
      IMPLICIT REAL ( KIND = rp_ ) ( a-h, o-z )

      INTEGER ( KIND = ip_ ) :: status, m, n, mxm1, mxmc, mxgr, mxf
      INTEGER ( KIND = ip_ ) :: mxiws, mxws, nout, len_iws, len_ws, mc
      INTEGER ( KIND = ip_ ) :: iprint, kmax, maxf, max_iter, mlp
      INTEGER ( KIND = ip_ ) :: maxgr, maxsc, maxu, maxiu, maxla, maxg
      INTEGER ( KIND = ip_ ) :: maxa, nv, ipeq, ll, kkk, lll
      INTEGER ( KIND = ip_ ) :: nnzj, ifail, nout1, itn, nft, ngt, kk
      INTEGER ( KIND = ip_ ) :: iter, npv, ngr, ninf, k
      INTEGER ( KIND = ip_ ), PARAMETER :: input = 7
      INTEGER ( KIND = ip_ ), PARAMETER :: mbar = 5
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: iws
      REAL ( KIND = rp_ ) :: rho, htol, rgtol, ainfty, fmin, f, h, ubd
      REAL ( KIND = rp_ ) :: dnorm, rgnorm, hJt, eps, tol, emin
      REAL ( KIND = rp_ ) :: hJ, vstep
      REAL ( KIND = rp_ ) :: v( mbar )
      REAL ( KIND = rp_ ) :: CPU( 4 ), CALLS( 7 )
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: x, bl, bu, al
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: ws
      CHARACTER, ALLOCATABLE, DIMENSION( : ) :: cstype
      CHARACTER ( len = 10 ) :: pname
C     CHARACTER :: ch

      COMMON / epsc / eps, tol, emin
      COMMON / defaultc / ainfty, ubd, mlp, mxf
      COMMON / wsc / kk, ll, kkk, lll, mxws, mxiws
      COMMON / statsc / dnorm, h, hJt, hJ, ipeq, k, itn, nft, ngt
      COMMON / refactorc / mc, mxmc
      COMMON / infoc / rgnorm, vstep, iter, npv, ngr, ninf
      COMMON / pnamec / pname
      COMMON / ngrc / mxgr
      COMMON / maxac / maxa
      COMMON / mxm1c / mxm1

C  open the relevant data input file

      OPEN( input, FILE = 'OUTSDIF.d', FORM = 'FORMATTED',
     &      STATUS = 'OLD' )
      REWIND( input )

C  compute problem dimensions

      CALL CUTEST_cdimen_r( status, input, n, m )
      IF ( status /= 0 ) GO TO 910
      mxm1 = min( m + 1,  n )

C  allocate space 

      ALLOCATE( x( n + m ), bl( n + m ), bu( n + m ), al( n + m ),
     &          cstype( m ), STAT = status )
      if ( status /= 0 ) GO TO 990

C   read FILTERSD.SPC parameter file (or use defaults)

      CALL READPAR_SD( iprint, kmax, maxf, max_iter, mlp, len_iws, 
     &       len_ws, nout, rho, htol, rgtol, maxgr, maxsc, ainfty )

      IF ( kmax < 0 ) kmax = n
      mxmc = maxsc
      mxgr = maxgr
      mxf = maxf

C  allocate further space 

      mxiws = len_iws
      mxws = len_ws
      ALLOCATE( iws( len_iws ), ws( len_ws ), STAT = status )
      if ( status /= 0 ) GO TO 990

C  compute initial values and bounds

      CALL INITIALIZE( n, m, nnzj, x, bl, bu, ws, iws, len_iws )

      maxu = n
      maxiu = 2 * maxa
      maxla = maxa + m + 3
      maxg = min( n, mbar ) + 1
      nv = 1
      v( 1 ) = 1.D0
      fmin = - ainfty

C  call the optimization package

   10 CONTINUE
      CALL filterSD( n, m, x, al, f, fmin, cstype, bl, bu, ws, iws, v, 
     &               nv, maxa, maxla, maxu, maxiu, kmax, maxg, rho, 
     &               htol, rgtol, max_iter, iprint, nout, ifail )

      IF ( ifail .eq. 4 .AND. h .gt. ubd ) THEN
        ubd = 11.0E-1_rp_ * h
        GO TO 10
      END IF

      CALL CUTEST_creport_r( status, CALLS, CPU )

C     OPEN( 99, STATUS = 'old', ERR = 998 )
C 997 CONTINUE
C     READ( 99, *, END = 999 ) ch
C     GO TO 997
C 998 CONTINUE
C     OPEN( 99 )
C 999 CONTINUE

C     nout1 = 99
      nout1 = 0
      IF ( nout1 > 0 ) THEN
        IF ( ifail .eq. 0 ) THEN
          IF ( ABS( f ) .lt. 1.D5 .and. ABS( f ) .ge. 1.D0 ) THEN
            WRITE( nout1, 1111) 
     &        pname, n, m, f, h, rgnorm, k, itn, nft, ngt
          ELSE
            WRITE( nout1, 2222 ) 
     &        pname, n, m, f, h, rgnorm, k, itn, nft, ngt
          END IF
        ELSE IF ( ifail .EQ. 3 )THEN
          WRITE( nout1, 3333 ) 
     &      pname, n, m, hJt, h, rgnorm, k, itn, nft, ngt, ifail
        ELSE
          WRITE( nout1, 3333 ) 
     &      pname, n, m, f, h, rgnorm, k, itn, nft, ngt, ifail
        END IF
      END IF

      IF ( nout .GT. 0 )
     &  WRITE ( nout, 2000 ) pname, n, m, CALLS( 1 ), CALLS( 2 ), 
     &   CALLS( 5 ), CALLS( 6 ), ifail, f, CPU( 1 ), CPU( 2 )

      DEALLOCATE( x, bl, bu, al, CSTYPE, iws, ws, STAT = status )
      CALL CUTEST_cterminate_r( status )
      STOP

  910 CONTINUE
      WRITE( 6,  "( ' CUTEst error, status = ', i0, ', stopping' )") 
     &   status
      STOP

  990 CONTINUE
      WRITE( 6, "( ' Allocation error, status = ', I0 )" ) status
      STOP

C  Non-executable statements

 2000 FORMAT( /, 24('*'), ' CUTEst statistics ', 24('*') //
     &    ,' Package used            :  filterSD',    /
     &    ,' Problem                 :  ', A10,    /
     &    ,' # variables             =      ', I10 /
     &    ,' # constraints           =      ', I10 /
     &    ,' # objective functions   =        ', F8.2 /
     &    ,' # objective gradients   =        ', F8.2 / 
     &    ,' # constraints functions =        ', F8.2 /
     &    ,' # constraints gradients =        ', F8.2 /
     &    ,' Exit code               =      ', I10 /
     &    ,' Final f                 = ', E15.7 /
     &    ,' Set up time             =      ', 0P, F10.2, ' seconds' /
     &    ,' Solve time              =      ', 0P, F10.2, ' seconds' //
     &     66('*') / )

 1111 FORMAT( A9, I4, I5, ' ', G14.7, ' ', E9.3, E10.3, 2I4, 2I6 )
 2222 FORMAT( A9, I4, I5, ' ', E14.6, ' ', E9.3, E10.3, 2I4, 2I6 )
 3333 FORMAT( A9, I4, I5, ' ', E14.6, ' ', E9.3, E10.3, 2I4, 2I6, 
     &       ' fail', I2 )
      END

C  initialization subroutine

      SUBROUTINE INITIALIZE( n, m, nnzj, x, bl, bu, ws, iws, len_iws )
      USE CUTEST_KINDS_precision
      IMPLICIT REAL ( KIND = rp_ ) (a-h, o-z)
      INTEGER ( KIND = ip_ ) :: n, m, nnzj, len_iws
      INTEGER ( KIND = ip_ ) :: iws(len_iws)
      REAL ( KIND = rp_ ) :: x(*), bl(*), bu(*), ws(*)
      CHARACTER ( len = 10 ) pname
      LOGICAL :: equatn(m),  linear(m)
      INTEGER ( KIND = ip_ ), PARAMETER :: input = 7
      INTEGER ( KIND = ip_ ), PARAMETER :: io_buffer = 11
      INTEGER ( KIND = ip_ ) :: status, maxa, ip, i, j, k, lj
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: iuser
      COMMON / pnamec / pname
      COMMON / maxac / maxa

      CALL CUTEST_csetup_r(status, input, 6, io_buffer, n, m, x, bl, bu,
     &                   ws,bl(n+1), bu(n+1), equatn, linear,
     &                   0, 0, 0 )
      IF ( status /= 0 ) GO TO 910

      CALL CUTEST_probname_r( status, pname )
      IF ( status /= 0 ) GO TO 910

C  compute the numbers of nonzeros in the constraint Jacobian

      CALL CUTEST_cdimsj_r( status, lj )
      IF ( status /= 0 ) GO TO 910

      ALLOCATE( iuser( lj ), STAT = status )
      if ( status /= 0 ) GO TO 990

C  call CUTEst's coordinate format into sparse vector format.
C  Note nonzero column indices are in ascending order
C  followed by n zero column indices (objective function)

      CALL CUTEST_csgr_r( status, n, m, x, x(n+1), .FALSE.,
     &                  nnzj, lj, ws, iuser, iws( 1 ) )
      IF ( status /= 0 ) GO TO 910

C  iws partitions -
C  iws( 1 : nnzj ) = Jac rows
C  iws( nnzj + 1 : 2 * nnzj ) unset
C  iws( 2 * nnzj + 1 ) = nnzj + 1
C  iws( 2 * nnzj + 2 : 2 * nnzj + n + 1 ) = gradient index
C  iws( 2 * nnzj + n + 2 : 3 * nnzj + 1 ) = Jac col index
C  iws( 3 * nnzj + 2 ) = 1
C  iws( 3 * nnzj + 3 ) = n + 1
C  iws( 3 * nnzj + 4 : 3 * nnzj + m + 3 ) = n + row pointer start

      IF ( 3 * nnzj + m + 3 .gt. len_iws ) THEN
        PRINT *,'not enough space for CUTEst Jacobian indices'
        PRINT *,'increase iws to have length at least', 3 * nnzj + m + 3
        STOP
      END IF

      maxa = nnzj
      iws( 2 * maxa + 1 ) = maxa + 1
      ip = 3 * maxa + 3
      iws( ip - 1 ) = 1
      iws( ip ) = n + 1
      k = 1
      DO j = 1, m
   10   CONTINUE
        IF ( iws( k ) .eq. j ) THEN
          k = k + 1
          GO TO 10
        END IF
        iws( ip + j ) =  k + n
      END DO

      DO i = maxa - n + 1, maxa
        iws( maxa + n + 1 + i ) = iuser( i )
      END DO
      DO i = 1, maxa - n
        iws( 2 * maxa + n + 1 + i ) = iuser( i )
      END DO

      DEALLOCATE( iuser, STAT = status )
      if ( status /= 0 ) GO TO 990
      RETURN

  910 CONTINUE
      WRITE( 6,  "( ' CUTEst error, status = ', i0, ', stopping' )") 
     &   status
      STOP

  990 CONTINUE
      WRITE( 6, "( ' Allocation error, status = ', I0 )" ) status
      STOP
      END

C  function and constraint evaluation routine

      SUBROUTINE FUNCTIONS( n, m, x, f, c, user, iuser )
      USE CUTEST_KINDS_precision
      IMPLICIT REAL ( KIND = rp_ ) (a-h, o-z)
      INTEGER ( KIND = ip_ ) :: n, m
      REAL ( KIND = rp_ ) :: f
      INTEGER ( KIND = ip_ ) :: iuser(*)
      REAL ( KIND = rp_ ) :: x(*), c(*), user(*)
      INTEGER ( KIND = ip_ ) :: status

      CALL CUTEST_cfn_r(status, n, m, x, f, c)
      IF ( status /= 0 ) THEN
        write( 6, "( ' CUTEst error, status = ',  i0, ',  stopping' )") 
     &     status
        STOP
      END IF
      RETURN
      END

C  function and constraint gradients evaluation routine

      SUBROUTINE GRADIENTS( n, m, x, a, user, iuser )
      USE CUTEST_KINDS_precision
      IMPLICIT REAL ( KIND = rp_ )( a-h, o-z )
      INTEGER ( KIND = ip_ ) :: n, m
      INTEGER ( KIND = ip_ ) :: iuser(*)
      REAL ( KIND = rp_ ) :: x(*), a(*), user(*)
      INTEGER ( KIND = ip_ ) :: status, i, nnzj, maxa
      COMMON / maxac / maxa

      CALL CUTEST_csgr_r( status, n, m, x, x( n + 1 ), .false., 
     &                  nnzj, maxa, a, iuser, iuser( maxa + 1 ) )
      IF ( status /= 0 ) THEN
        write( 6, "( ' CUTEst error, status = ', i0, ', stopping' )") 
     &     status
        STOP
      END IF
C     print 4, 'a(ij) =', (a(k), k=1, nnzj)
      DO i = 1, n
        user(i) = a( maxa - n + i )
      END DO
      DO i = maxa - n, 1, - 1
        a( n + i ) = a( i )
      END DO
      DO i = 1, n
        a( i ) = user( i )
      END DO
C     print 4, 'new a(ij) =', (a(k), k = 1, nnzj)
C   4 format(A/(5E15.7))
      RETURN
      END

C  extracted and modified from filterSQP

      SUBROUTINE READPAR_SD( iprint, kmax, maxf, maxiter, mlp, mxiws, 
     &             mxws, nout, rho, htol, rgtol, maxgr, maxsc, ainfty )

      USE CUTEST_KINDS_precision
      IMPLICIT NONE

C     ... declaration of passed parameters
      INTEGER ( KIND = ip_ ) :: iprint, kmax, maxf, maxiter, mlp, mxiws
      INTEGER ( KIND = ip_ ) :: maxgr, maxsc, mxws, nout
      REAL ( KIND = rp_ ) :: rho, htol, rgtol, ainfty

C     ... declaration of internal variables
      INTEGER ( KIND = ip_ ), parameter :: nin = 29
      REAL ( KIND = rp_ ) :: value
      CHARACTER ( len = 8 ) :: option

C     ========================  procedure body  =========================

C     ... open options file (if possible)
      open( nin, file = 'FILTERSD.SPC', status = 'OLD', err = 999 )

 100  CONTINUE
         read(nin,*,end=99) option, value
         IF (option.eq.'iprint  ') THEN
            iprint  = int( value )
         ELSE IF (option.eq.'kmax    ') THEN
            kmax    = int( value )
         ELSE IF (option.eq.'maxf    ') THEN
            maxf    = int( value )
         ELSE IF (option.eq.'maxiter ') THEN
            maxiter = int( value )
         ELSE IF (option.eq.'mlp     ') THEN
            mlp     = int( value )
         ELSE IF (option.eq.'mxiws   ') THEN
            mxiws   = int( value )
         ELSE IF (option.eq.'mxws    ') THEN
            mxws    = int( value )
         ELSE IF (option.eq.'nout    ') THEN
            nout    = int( value )
         ELSE IF (option.eq.'rho     ') THEN
            rho     =      value
         ELSE IF (option.eq.'htol    ') THEN
            htol    =      value
         ELSE IF (option.eq.'rgtol   ') THEN
            rgtol   =      value
         ELSE IF (option.eq.'maxgr   ') THEN
            maxgr   = int( value )
         ELSE IF (option.eq.'maxsc   ') THEN
            maxsc   = int( value )
         ELSE IF (option.eq.'ainfty  ') THEN
            ainfty  =      value
         ELSE
            print *,'WARNING: wrong FILTERSD.SPC option ',option, value
         END IF
      GO TO 100
      
 999  CONTINUE
      print *,'WARNING: no spec.par file: use defaults'
 99   CONTINUE
      IF ( iprint .ge. 2 ) THEN
         print *,'iprint   = ', iprint
         print *,'kmax     = ', kmax
         print *,'maxf     = ', maxf
         print *,'maxiter  = ', maxiter
         print *,'mlp      = ', mlp
         print *,'mxiws    = ', mxiws
         print *,'mxws     = ', mxws
         print *,'nout     = ', nout
         print *,'rho      = ', rho
         print *,'htol     = ', htol
         print *,'rgtol    = ', rgtol 
         print *,'maxgr    = ', maxgr
         print *,'maxsc    = ', maxsc
         print *,'ainfty   = ', ainfty
      END IF

      CLOSE( nin )

      RETURN
      END


