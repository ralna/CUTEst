C  THIS VERSION: CUTEST 2.2 - 2023-11-29 AT 16:00 GMT.

#include "cutest_modules.h"

C  Dummy SEARCH etc testing pds_main interface to CUTEst
C  Nick Gould, 5th January 2013

      SUBROUTINE SEARCH(N,OUT,MAX,SCHEME,INDEX,LIST,UNIQUE,FACTOR,ERROR)
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: N, MAX, OUT, UNIQUE, FACTOR, ERROR
      INTEGER ( KIND = ip_ ) :: SCHEME(-1:N,-N:((MAX-N*N-3*N-2)/(N+2)))
      INTEGER ( KIND = ip_ ) :: INDEX(-N:((MAX-N*N-3*N-2)/(N+2)))
      INTEGER ( KIND = ip_ ) :: LIST((MAX-N*N-3*N-2)/(N+2))
      unique = 1
      factor = 1
      error = 0
      RETURN
      END

      SUBROUTINE GETSS(N,IN,SSS,SCHEME,FACTOR,RESIZE,ERROR)
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: N, IN, SSS, SCHEME(-1:N,SSS)
      INTEGER ( KIND = ip_ ) :: RESIZE, ERROR
      REAL ( KIND = rp_ ) :: FACTOR
      error = 0
      RETURN
      END

      SUBROUTINE PDS(N,LPR,TYPE,SCALE,DEBUG,TOL,MAXITR,SSS,FCN,FACTOR,
     *             SCHEME,RESIZE,S,INDEX,FBEST,LENGTH,COUNT,EDGE,C,PLUS)
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: N, LPR, TYPE, DEBUG, MAXITR, SSS, COUNT
      INTEGER ( KIND = ip_ ) :: SCHEME(-1:N,SSS), RESIZE, INDEX(N+1)
      REAL ( KIND = rp_ ) SCALE, TOL, FACTOR, S(N,N+1), FBEST, LENGTH
      REAL ( KIND = rp_ ) EDGE(N), C(-3:N+1), PLUS(-3:N+1)
      EXTERNAL :: FCN
      CALL FCN( n, S(1,1), FBEST )
      RETURN
      END 

      SUBROUTINE RESULT(N,COUNT,S,FBEST,INDEX,OUT)
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) :: N, COUNT, INDEX(N+1), OUT
      REAL ( KIND = rp_ ) :: S(N,N+1), FBEST
      RETURN
      END 
