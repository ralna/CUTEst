!     ( Last modified on 9 Apr 2019 at 14:10:00 )

!  Dummy CURVI for testing curvi_main interface to CUTEst
!  Nick Gould, 9th April 2019

   SUBROUTINE curvif( fu, n, X0, fopt, eps, ibound, JBOUND, BL, BU, WA, nfu,   &
                      nit, idiff, kmax, ier )
   external :: fu
   integer :: n, itrid, ibound, nfu, nit, idiff, kmax, ier
   double precision :: fopt, eps
   integer :: JBOUND( n )
   double precision :: X0( n ), WA( * ), BL( n ), BU( n )
   call fu( n, X0, fopt )
   WA( : n ) = 0.0
   ier = - 1
   END SUBROUTINE curvif

   SUBROUTINE curvig( fu, gradie, n, X0, fopt, eps, ibound, JBOUND, BL, BU,    &
                      WA, nfu, ngr, nit, ier )
   external :: fu, gradie
   integer :: n, itrid, ibound, nfu, ngr, nit, ier
   double precision :: fopt, eps
   integer :: JBOUND( n )
   double precision :: X0( n ), WA( * ), BL( n ), BU( n )
   call fu( n, X0, fopt )
   call gradie( n, X0, WA )
   WA( : n ) = 0.0
   ier = - 2
   END SUBROUTINE curvig

   SUBROUTINE curvih( fu, gradie, hessia, n, X0, fopt, eps, itrid, ibound,     &
                      JBOUND, BL, BU, WA, nfu, ngr, nhes, nit, ier )
   external :: fu, gradie, hessia
   integer :: n, itrid, ibound, nfu, ngr, nhes, nit, ier
   double precision :: fopt, eps
   integer :: JBOUND( n )
   double precision :: X0( n ), WA( * ), BL( n ), BU( n )
   call fu( n, X0, fopt )
   call gradie( n, X0, WA )
   call hessia( n, X0, WA )
   WA( : n ) = 0.0
   ier = - 3
   END SUBROUTINE curvih
