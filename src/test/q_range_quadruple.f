      SUBROUTINE RANGE_q( IELEMN, TRANSP, W1, W2, nelvar, ninvar,
     *                  itype, LW1, LW2 )
      USE ISO_FORTRAN_ENV
      INTEGER IELEMN, nelvar, ninvar, itype, LW1, LW2
      LOGICAL TRANSP
      REAL(REAL128) W1( LW1 ), W2( LW2 )
C
C  Problem name : ALLINQP 
C
C  -- produced by SIFdecode 1.0
C
C  TRANSP = .FALSE. <=> W2 = U * W1
C  TRANSP = .TRUE.  <=> W2 = U(transpose) * W1
C
      RETURN
      END
