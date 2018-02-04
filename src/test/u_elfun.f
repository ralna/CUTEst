      SUBROUTINE ELFUN ( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE,
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA,
     *                   ICALCF, LTYPEE, LSTAEV, LELVAR, LNTVAR,
     *                   LSTADH, LSTEPA, LCALCF, LFVALU, LXVALU,
     *                   LEPVLU, IFFLAG, IFSTAT )
      INTEGER NCALCF, IFFLAG, LTYPEE, LSTAEV, LELVAR, LNTVAR
      INTEGER LSTADH, LSTEPA, LCALCF, LFVALU, LXVALU, LEPVLU
      INTEGER IFSTAT
      INTEGER ITYPEE(LTYPEE), ISTAEV(LSTAEV), IELVAR(LELVAR)
      INTEGER INTVAR(LNTVAR), ISTADH(LSTADH), ISTEPA(LSTEPA)
      INTEGER ICALCF(LCALCF)
      DOUBLE PRECISION FUVALS(LFVALU), XVALUE(LXVALU), EPVALU(LEPVLU)
C
C  PROBLEM NAME : ALLINITU
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , Y     , Z     , SINX  , COSX
      DOUBLE PRECISION XX    , YY
      INTRINSIC SIN   , COS
      IFSTAT = 0
      DO     5 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF)
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3,    4
     *                                                        ), IELTYP
C
C  ELEMENT TYPE : SQR
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X * X
       ELSE
        FUVALS(IGSTRT+     1)= X + X
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : SQR2
C
    2  CONTINUE
       Y      = XVALUE(IELVAR(ILSTRT+     1))
       Z      = XVALUE(IELVAR(ILSTRT+     2))
       X      =   Y
     *          + Z
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X * X
       ELSE
        FUVALS(IGSTRT+     1)= X + X
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : SINSQR
C
    3  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       SINX   = SIN( X )
       COSX   = COS( X )
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= SINX * SINX
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * SINX * COSX
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0 * ( COSX * COSX - SINX * SINX )
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : PRODSQR
C
    4  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       XX     = X * X
       YY     = Y * Y
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= XX * YY
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * X * YY
        FUVALS(IGSTRT+     2)= 2.0 * XX * Y
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0 * YY
         FUVALS(IHSTRT+     2)=4.0 * X * Y
         FUVALS(IHSTRT+     3)=2.0 * XX
        END IF
       END IF
    5 CONTINUE
      RETURN
      END
