! THIS VERSION: CUTEST 2.2 - 2023-11-24 AT 15:10 GMT.

#include "cutest_modules.h"

!  Dummy CURVI for testing curvi_main interface to CUTEst
!  Nick Gould, 9th April 2019

   SUBROUTINE curvif( fu, n, X0, fopt, eps, ibound, JBOUND, BL, BU, WA, nfu,   &
                      nit, idiff, kmax, ier )
   USE CUTEST_KINDS_precision
   EXTERNAL :: fu
   INTEGER ( KIND = ip_ ) :: n, itrid, ibound, nfu, nit, idiff, kmax, ier
   REAL ( KIND = rp_ ) :: fopt, eps
   INTEGER ( KIND = ip_ ) :: JBOUND( n )
   REAL ( KIND = rp_ ) :: X0( n ), WA( * ), BL( n ), BU( n )
   CALL fu( n, X0, fopt )
   WA( : n ) = 0.0
   ier = - 1
   RETURN
   END SUBROUTINE curvif

   SUBROUTINE curvig( fu, gradie, n, X0, fopt, eps, ibound, JBOUND, BL, BU,    &
                      WA, nfu, ngr, nit, ier )
   USE CUTEST_KINDS_precision
   EXTERNAL :: fu, gradie
   INTEGER ( KIND = ip_ ) :: n, itrid, ibound, nfu, ngr, nit, ier
   REAL ( KIND = rp_ ) :: fopt, eps
   INTEGER ( KIND = ip_ ) :: JBOUND( n )
   REAL ( KIND = rp_ ) :: X0( n ), WA( * ), BL( n ), BU( n )
   INTEGER  ( KIND = ip_ ) :: g
   g = n + n * n
   CALL fu( n, X0, fopt )
   CALL gradie( n, X0, WA( g + 1 : g + n ) )
   ier = - 2
   RETURN
   END SUBROUTINE curvig

   SUBROUTINE curvih( fu, gradie, hessia, n, X0, fopt, eps, itrid, ibound,     &
                      JBOUND, BL, BU, WA, nfu, ngr, nhes, nit, ier )
   USE CUTEST_KINDS_precision
   EXTERNAL :: fu, gradie, hessia
   INTEGER ( KIND = ip_ ) :: n, itrid, ibound, nfu, ngr, nhes, nit, ier
   REAL ( KIND = rp_ ) :: fopt, eps
   INTEGER ( KIND = ip_ ) :: JBOUND( n )
   REAL ( KIND = rp_ ) :: X0( n ), WA( * ), BL( n ), BU( n )
   INTEGER  ( KIND = ip_ ) :: g, h
   g = n + n * n
   h = g + 8 * n
   CALL fu( n, X0, fopt )
   CALL gradie( n, X0, WA )
   CALL gradie( n, X0, WA( g + 1 : g + n ) )
   CALL hessia( n, X0, WA( h + 1 : h + n * ( n + 1 ) / 2 ) )
   ier = - 3
   RETURN
   END SUBROUTINE curvih
