! THIS VERSION: CUTEST 1.0 - 04/01/2013 AT 13:00 GMT.

!-*-*--*- F O R T R A N  O P E R A T I O N S   S U B R O U T I N E S  -*-*-*-*-

!  Copyright reserved, Bongartz/Conn/Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould (origin unknown)

!  History -
!   Released in CUTEr, April 2004
!   Updated fortran 2003 version released January 2013

!  For full documentation, see 
!   http://galahad.rl.ac.uk/galahad-www/specs.html

!-*-*-*-*-*-*- F O R T R A N  O P E N   S U B R O U T I N E  -*-*-*-*-*-*-*-*-

      SUBROUTINE FORTRAN_open( funit, fname, status )

!  Open file fname using Fortran unit number funit. 
!  If successful, status = 0 is returned, otherwise status = 1.
!  FORTRAN_OPEN() is particularly intended to be called from C when a 
!  unit number is required

      IMPLICIT NONE
      INTEGER, INTENT( IN ) :: funit
      INTEGER, INTENT( OUT ) :: status
      CHARACTER ( LEN = 256 ), INTENT( IN ) :: fname
      
      OPEN( funit, FILE = fname, STATUS = 'UNKNOWN', ERR = 900 )
      status = 0
      RETURN

 900  CONTINUE
      status = 1
      RETURN
      END SUBROUTINE FORTRAN_open

!-*-*-*-*-*-*- F O R T R A N  C L O S E  S U B R O U T I N E  -*-*-*-*-*-*-*-*-

      SUBROUTINE FORTRAN_close( funit, status )

! Close a stream unit previously opened by FORTRAN_OPEN.
! Exit value status: = 0 = successful return, 1 = error

      IMPLICIT NONE
      INTEGER, INTENT( IN ) :: funit
      INTEGER, INTENT( OUT ) :: status

      CLOSE( funit, ERR = 900 )
      status = 0
      RETURN

  900 CONTINUE
      status = 1
      RETURN
      END SUBROUTINE FORTRAN_close

