      SUBROUTINE ELFUN_q( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, LTYPEE, LSTAEV, LELVAR, LNTVAR, 
     *                   LSTADH, LSTEPA, LCALCF, LFVALU, LXVALU, 
     *                   LEPVLU, IFFLAG, IFSTAT )
      USE ISO_FORTRAN_ENV
      INTEGER NCALCF, IFFLAG, LTYPEE, LSTAEV, LELVAR, LNTVAR
      INTEGER LSTADH, LSTEPA, LCALCF, LFVALU, LXVALU, LEPVLU
      INTEGER IFSTAT
      INTEGER ITYPEE(LTYPEE), ISTAEV(LSTAEV), IELVAR(LELVAR)
      INTEGER INTVAR(LNTVAR), ISTADH(LSTADH), ISTEPA(LSTEPA)
      INTEGER ICALCF(LCALCF)
      REAL(REAL128) FUVALS(LFVALU), XVALUE(LXVALU), EPVALU(LEPVLU)
C
C  Problem name : ALLINQP 
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      REAL(REAL128) X     , Y     
      IFSTAT = 0
      DO     3 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2
     *                                                        ), IELTYP
C
C  Element type : 123456789S
C
    1  CONTINUE
       X = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= 5.0D-1 * X * X
       ELSE
        FUVALS(IGSTRT+     1)= X
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)= 1.0D+0
        END IF
       END IF
       GO TO     3
C
C  Element type : 123456789P
C
    2  CONTINUE
       X = XVALUE(IELVAR(ILSTRT+     1))
       Y = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X * Y
       ELSE
        FUVALS(IGSTRT+     1)= Y
        FUVALS(IGSTRT+     2)= X
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)= 0.0D+0
         FUVALS(IHSTRT+     2)= 1.0D+0
         FUVALS(IHSTRT+     3)= 0.0D+0
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
