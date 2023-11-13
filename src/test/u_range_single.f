      SUBROUTINE RANGE_s( IELEMN, TRANSP, W1, W2, NELVAR, NINVAR,
     *                  ITYPE, LW1, LW2 )
      INTEGER IELEMN, NELVAR, NINVAR, ITYPE, LW1, LW2
      LOGICAL TRANSP
      REAL W1( LW1 ), W2( LW2 )
C
C  PROBLEM NAME : ALLINITU
C
C  TRANSP = .FALSE. <=> W2 = U * W1
C  TRANSP = .TRUE.  <=> W2 = U(TRANSPOSE) * W1
C
      INTEGER I
      GO TO (99998,    2,99998,99998
     *                                                        ), ITYPE
C
C  ELEMENT TYPE : SQR2      
C
    2 CONTINUE
      IF ( TRANSP ) THEN
         W2(     1 ) =   W1(     1 ) 
         W2(     2 ) =   W1(     1 ) 
      ELSE
         W2(     1 ) =   W1(     1 ) 
     *                 + W1(     2 ) 
      END IF
      RETURN
C
C  ELEMENTS WITHOUT INTERNAL VARIABLES.
C
99998 CONTINUE
      DO 99999 I = 1, NELVAR
         W2( I ) = W1( I )
99999 CONTINUE
      RETURN
      END
