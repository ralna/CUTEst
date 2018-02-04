C     ( Last modified on 6 Jan 2013 at 16:00:00 )

C  Dummy LA04AD for testing la04_main interface to CUTEst
C  Nick Gould, 6th January 2013

      SUBROUTINE LA04AD(A,LA,IRN,IP,M,N,B,C,BND,KB,LB,JOB,CNTL,IX,JX,X,
     +                  Z,G,RINFO,WS,LWS,IWS,LIWS)
      INTEGER LA,KB,LB,M,N,LWS,LIWS
      DOUBLE PRECISION A(LA),B(M),BND(2,KB),C(N),CNTL(15),G(N),
     +                RINFO(40),WS(LWS),X(N+M),Z(N)
      INTEGER IP(N+1),IRN(LA),IWS(LIWS),IX(M),JOB,JX(KB)
      INTEGER :: i
      IF ( job .EQ. 7 ) THEN
        DO 10 i = 1, m
          WS( i ) = 1.0D0
   10   CONTINUE
      ELSE
        DO 20 i = 1, n
          Z( i ) = 2.0D0
   20   CONTINUE
        DO 30 i = 1, kb
          JX( i ) = 0
   30   CONTINUE
        DO 40 i = 1, m
          WS( i ) = 1.0D0
          IX( i ) = i
   40   CONTINUE
        job = 0
      END IF
      RINFO( 1 ) = 0.0D0
      RETURN
      END

      SUBROUTINE LA04ID(CNTL)
      DOUBLE PRECISION CNTL(15)
      RETURN
      END

      SUBROUTINE MC49AD(IND,NC,NR,NNZ,IRN,JCN,YESA,LA,A,LIP,IP,LIW,IW,
     +                  IFLAG)
      INTEGER IFLAG,IND,LA,LIP,LIW,NC,NNZ,NR
      LOGICAL YESA
      DOUBLE PRECISION A(LA)
      INTEGER IP(LIP),IRN(NNZ),IW(LIW),JCN(NNZ)
      INTEGER :: i
      DO 10 i = 1, lip
        IP( i ) = 1
   10 CONTINUE
      RETURN
      END
