      SUBROUTINE GROUP_q ( GVALUE, LGVALU, FVALUE, GPVALU, NCALCG, 
     *                    ITYPEG, ISTGPA, ICALCG, LTYPEG, LSTGPA, 
     *                    LCALCG, LFVALU, LGPVLU, DERIVS, IGSTAT )
      USE ISO_FORTRAN_ENV
      INTEGER LGVALU, NCALCG, LTYPEG, LSTGPA
      INTEGER LCALCG, LFVALU, LGPVLU, IGSTAT
      LOGICAL DERIVS
      INTEGER ITYPEG(LTYPEG), ISTGPA(LSTGPA), ICALCG(LCALCG)
      REAL(REAL128) GVALUE(LGVALU,3), FVALUE(LFVALU), GPVALU(LGPVLU)
C
C  Problem name : ALLINQP 
C
C  -- produced by SIFdecode 1.0
C
      IGSTAT = 0
      RETURN
      END
