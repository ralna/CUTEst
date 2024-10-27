! THIS VERSION: CUTEST 2.3 - 2024-10-27 AT 09:30 GMT.

#include "cutest_modules.h"
#include "cutest_routines.h"

!-*-*--*- F O R T R A N  O P E R A T I O N S   S U B R O U T I N E S  -*-*-*-*-

!  Copyright reserved, Bongartz/Conn/Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould (origin unknown)

!  History -
!   Released in CUTEr, April 2004
!   Updated fortran 2003 version released January 2013

!  For full documentation, see 
!   http://galahad.rl.ac.uk/galahad-www/specs.html

!-*-*-*-*-*-*- F O R T R A N  O P E N   S U B R O U T I N E  -*-*-*-*-*-*-*-*-

      SUBROUTINE FORTRAN_open_r( funit, cname, status )
      USE CUTEST_KINDS_precision
      USE ISO_C_BINDING

!  Open file fname using Fortran unit number funit. 
!  If successful, status = 0 is returned, otherwise status = 1.
!  FORTRAN_OPEN() is particularly intended to be called from C when a 
!  unit number is required

      IMPLICIT NONE
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: funit
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
      CHARACTER ( KIND = C_CHAR, LEN = 1 ), INTENT( IN ) :: cname( * )

!  local variables

      INTEGER ( KIND = ip_ ) :: i     
      INTEGER, PARAMETER :: len_fname = 256
      CHARACTER ( LEN = len_fname ) :: fname = REPEAT( ' ', len_fname )

      DO i = 1, len_fname
        IF ( cname( i ) == C_NULL_CHAR ) EXIT
        fname( i : i ) = cname( i )
      END DO
      
      OPEN( funit, FILE = TRIM( fname ), STATUS = 'UNKNOWN', ERR = 900 )
      status = 0
      RETURN

 900  CONTINUE
      status = 1
      RETURN
      END SUBROUTINE FORTRAN_open_r

!  old version 
!     SUBROUTINE FORTRAN_open_r( funit, fname, status )
!     USE CUTEST_KINDS_precision

!  Open file fname using Fortran unit number funit. 
!  If successful, status = 0 is returned, otherwise status = 1.
!  FORTRAN_OPEN() is particularly intended to be called from C when a 
!  unit number is required

!     IMPLICIT NONE
!     INTEGER ( KIND = ip_ ), INTENT( IN ) :: funit
!     INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status
!     CHARACTER ( LEN = 256 ), INTENT( IN ) :: fname
!     OPEN( funit, FILE = fname, STATUS = 'UNKNOWN', ERR = 900 )
!     status = 0
!     RETURN

!900  CONTINUE
!     status = 1
!     RETURN
!     END SUBROUTINE FORTRAN_open_r

!-*-*-*-*-*-*- F O R T R A N  C L O S E  S U B R O U T I N E  -*-*-*-*-*-*-*-*-

      SUBROUTINE FORTRAN_close_r( funit, status )
      USE CUTEST_KINDS_precision

! Close a stream unit previously opened by FORTRAN_OPEN.
! Exit value status: = 0 = successful return, 1 = error

      IMPLICIT NONE
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: funit
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status

      CLOSE( funit, ERR = 900 )
      status = 0
      RETURN

  900 CONTINUE
      status = 1
      RETURN
      END SUBROUTINE FORTRAN_close_r

