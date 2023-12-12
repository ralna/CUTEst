! THIS VERSION: CUTEST 2.2 - 2023-12-01 AT 11:50 GMT.

#include "cutest_modules.h"

C  Dummy LA04AD for testing la04_main interface to CUTEst

C  Nick Gould, 6th January 2013

      SUBROUTINE LA04AD(A,LA,IRN,IP,M,N,B,C,BND,KB,LB,JOB,CNTL,IX,JX,X,
     +                  Z,G,RINFO,WS,LWS,IWS,LIWS)
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) LA,KB,LB,M,N,LWS,LIWS
      REAL ( KIND = rp_ ) A(LA),B(M),BND(2,KB),C(N),CNTL(15),G(N)
      REAL ( KIND = rp_ ) RINFO(40),WS(LWS),X(N+M),Z(N)
      INTEGER ( KIND = ip_ ) IP(N+1),IRN(LA),IWS(LIWS),IX(M),JOB,JX(KB)
      INTEGER ( KIND = ip_ ) :: i
      IF ( job .EQ. 7 ) THEN
        DO 10 i = 1, m
          WS( i ) = 1.0_rp_
   10   CONTINUE
      ELSE
        DO 20 i = 1, n
          Z( i ) = 2.0_rp_
   20   CONTINUE
        DO 30 i = 1, kb
          JX( i ) = 0
   30   CONTINUE
        DO 40 i = 1, m
          WS( i ) = 1.0_rp_
          IX( i ) = i
   40   CONTINUE
        job = 0
      END IF
      RINFO( 1 ) = 0.0_rp_
      RETURN
      END

      SUBROUTINE LA04ID(CNTL)
      USE CUTEST_KINDS_precision
      REAL ( KIND = rp_ ) CNTL(15)
      RETURN
      END

      SUBROUTINE MC49AD(IND,NC,NR,NNZ,IRN,JCN,YESA,LA,A,LIP,IP,LIW,IW,
     +                  IFLAG)
      USE CUTEST_KINDS_precision
      INTEGER ( KIND = ip_ ) IFLAG,IND,LA,LIP,LIW,NC,NNZ,NR
      LOGICAL YESA
      REAL ( KIND = rp_ ) A(LA)
      INTEGER ( KIND = ip_ ) IP(LIP),IRN(NNZ),IW(LIW),JCN(NNZ)
      INTEGER ( KIND = ip_ ) :: i
      DO 10 i = 1, lip
        IP( i ) = 1
   10 CONTINUE
      RETURN
      END
