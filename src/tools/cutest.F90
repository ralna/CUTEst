! THIS VERSION: CUTEST 2.2 - 2024-08-27 AT 08:05 GMT.

#include "cutest_modules.h"

!-*-*-*-*-*-*-*-*-*-*-*-*-*- C U T E S T   M O D U l E -*-*-*-*-*-*-*-*-*-*-*-*-

!  Copyright reserved, Bongartz/Conn/Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released as part of CUTE, December 1990
!   Became separate package CUTEr, April 2004
!   Updated fortran 2003 version released October 2012
!   multi-precision modern fortran version, November 2023

!  For full documentation, see
!   http://galahad.rl.ac.uk/galahad-www/specs.html

    MODULE CUTEST_precision

      USE CUTEST_KINDS_precision

      IMPLICIT NONE

      PRIVATE
      PUBLIC :: CUTEST_initialize_workspace, CUTEST_form_gradients,            &
                CUTEST_assemble_hessian, CUTEST_size_sparse_hessian,           &
                CUTEST_assemble_hessian_pattern,                               &
                CUTEST_assemble_element_hessian, CUTEST_size_element_hessian,  &
                CUTEST_hessian_times_vector, CUTEST_hessian_times_sp_vector,   &
                CUTEST_extend_array,                                           &
                CUTEST_allocate_array, CUTEST_initialize_thread,               &
                CUTEST_terminate_data, CUTEST_terminate_work

!----------------------
!   P a r a m e t e r s
!----------------------

!     INTEGER ( KIND = ip_ ), PARAMETER :: lmin = 1
      INTEGER ( KIND = ip_ ), PARAMETER :: lmin = 10000
      INTEGER ( KIND = ip_ ), PARAMETER :: lwtran_min = lmin
      INTEGER ( KIND = ip_ ), PARAMETER :: litran_min = lmin
      INTEGER ( KIND = ip_ ), PARAMETER :: llink_min = lmin
      INTEGER ( KIND = ip_ ), PARAMETER :: io_buffer = 11

!-------------------------------------
!   G e n e r i c  i n t e r f a c e s
!-------------------------------------

!  define generic interfaces to routines for allocating and extending
!  allocatable arrays

      INTERFACE CUTEST_allocate_array
        MODULE PROCEDURE CUTEST_allocate_array_integer,                        &
                         CUTEST_allocate_array_real
      END INTERFACE

      INTERFACE CUTEST_extend_array
        MODULE PROCEDURE CUTEST_extend_array_integer,                          &
                         CUTEST_extend_array_real
      END INTERFACE

!-------------------------------------------------
!  D e r i v e d   t y p e   d e f i n i t i o n s
!-------------------------------------------------

!  =================================
!  The CUTEST_data_type derived type
!  =================================

      TYPE, PUBLIC :: CUTEST_data_type
        INTEGER ( KIND = ip_ ) :: n, ng, ng1, nel, nel1, ntotel, nvrels
        INTEGER ( KIND = ip_ ) :: nnza, ngpvlu, nepvlu, ngng, out
        INTEGER ( KIND = ip_ ) :: nvargp, nvar2, nnonnz, nbprod, ntotin
        INTEGER ( KIND = ip_ ) :: lo, ch, lwork, la, lb, nobjgr, lu, ltypee
        INTEGER ( KIND = ip_ ) :: ltypeg, lintre, lft, lepvlu, lstep, lstgp
        INTEGER ( KIND = ip_ ) :: lstaev, lstadh, lntvar, lcalcf, leling, lnhuvl
        INTEGER ( KIND = ip_ ) :: lgxeqx, licna, lstada, lkndof, lgpvlu
        INTEGER ( KIND = ip_ ) :: lstadg, lgvals, lgscal, lescal, lvscal, lcalcg
        INTEGER ( KIND = ip_ ) :: l_link_e_u_v, lfuval, lelvar, maxsel, maxsin
        INTEGER ( KIND = ip_ ) :: lfxi, lgxi, lhxi, lggfx, ldx, lgrjac, lnguvl
        INTEGER ( KIND = ip_ ) :: meq, mlin, nnov, nnjv, numvar, numcon, threads
        REAL :: su_time, st_time, su_cpu_time, st_cpu_time
        LOGICAL :: alllin, altriv
        CHARACTER ( LEN = 10 ) :: pname
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ISTADG
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ISTGP
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ISTADA
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ISTAEV
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ISTEP
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ITYPEG
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: KNDOFC
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ITYPEE
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: IELING
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: IELVAR
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ICNA
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ISTADH
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: INTVAR
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: IVAR
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ITYPEV
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: CGROUP
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ISTAGV
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ISVGRP
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ISLGRP
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: IGCOLJ
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: IVALJR
        INTEGER ( KIND = ip_ ), ALLOCATABLE,                                   &
                                DIMENSION( : ) :: LINK_elem_uses_var
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: LIST_elements
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : , : ) :: ISYMMH
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: A
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: B
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: U
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: GPVALU
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: EPVALU
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: ESCALE
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: GSCALE
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: VSCALE
        LOGICAL, ALLOCATABLE, DIMENSION( : ) :: INTREP
        LOGICAL, ALLOCATABLE, DIMENSION( : ) :: GXEQX
        CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: GNAMES
        CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: VNAMES
      END TYPE CUTEST_data_type

!  =================================
!  The CUTEST_work_type derived type
!  =================================

      TYPE, PUBLIC :: CUTEST_work_type
        INTEGER ( KIND = ip_ ) :: nc2of, nc2og, nc2oh
        INTEGER ( KIND = ip_ ) :: nc2cf, nc2cg, nc2ch, nhvpr, njvpr, pnc
        INTEGER ( KIND = ip_ ) :: llink, lrowst, lpos, lused, lfilled, nbprod
        INTEGER ( KIND = ip_ ) :: nnzh = - 1
        INTEGER ( KIND = ip_ ) :: nnzohp = - 1
        INTEGER ( KIND = ip_ ) :: lh_row = lmin
        INTEGER ( KIND = ip_ ) :: lh_col = lmin
        INTEGER ( KIND = ip_ ) :: lh_val = lmin
        INTEGER ( KIND = ip_ ) :: io_buffer = io_buffer
        REAL :: time_ccf = 0.0
        REAL :: time_ccfg = 0.0
        REAL :: time_ccfsg = 0.0
        REAL :: time_cch = 0.0
        REAL :: time_cohprodsp = 0.0
        REAL :: time_cohprods = 0.0
        REAL :: time_cchprodsp = 0.0
        REAL :: time_cchprods = 0.0
        REAL :: time_ccifg = 0.0
        REAL :: time_ccifsg = 0.0
        REAL :: time_cdh = 0.0
        REAL :: time_cdhc = 0.0
        REAL :: time_cdhj = 0.0
        REAL :: time_cdimchp = 0.0
        REAL :: time_ceh = 0.0
        REAL :: time_cfn = 0.0
        REAL :: time_cgr = 0.0
        REAL :: time_cgrdh = 0.0
        REAL :: time_chcprod = 0.0
        REAL :: time_chprod = 0.0
        REAL :: time_chjprod = 0.0
        REAL :: time_cifn = 0.0
        REAL :: time_cigr = 0.0
        REAL :: time_cisgr = 0.0
        REAL :: time_cisgrp = 0.0
        REAL :: time_cidh = 0.0
        REAL :: time_cish = 0.0
        REAL :: time_cjprod = 0.0
        REAL :: time_clfg = 0.0
        REAL :: time_cofg = 0.0
        REAL :: time_cofsg = 0.0
        REAL :: time_csgr = 0.0
        REAL :: time_csgrp = 0.0
        REAL :: time_csjp = 0.0
        REAL :: time_csgreh = 0.0
        REAL :: time_csgrsh = 0.0
        REAL :: time_csgrshp = 0.0
        REAL :: time_csh = 0.0
        REAL :: time_cshc = 0.0
        REAL :: time_cshj = 0.0
        REAL :: time_cshcprod = 0.0
        REAL :: time_cshp = 0.0
        REAL :: time_cshprod = 0.0
        REAL :: time_csjprod = 0.0
        REAL :: time_cconst = 0.0
        REAL :: time_ubandh = 0.0
        REAL :: time_udh = 0.0
        REAL :: time_ueh = 0.0
        REAL :: time_ufn = 0.0
        REAL :: time_ugr = 0.0
        REAL :: time_ugrdh = 0.0
        REAL :: time_ugreh = 0.0
        REAL :: time_ugrsh = 0.0
        REAL :: time_uhprod = 0.0
        REAL :: time_uofg = 0.0
        REAL :: time_ush = 0.0
        REAL :: time_ushp = 0.0
        REAL :: time_ushprod = 0.0
        LOGICAL :: record_times = .FALSE.
        LOGICAL :: array_status = .FALSE.
        LOGICAL :: hessian_setup_complete = .FALSE.
        LOGICAL :: jacobian_2d_setup_complete = .FALSE.
        LOGICAL :: hessian_2d_setup_complete = .FALSE.
        LOGICAL :: band_2d_setup_complete = .FALSE.
        LOGICAL :: firstg
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ISWKSP
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ICALCF
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ISTAJC
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: IUSED
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: NZ_components_w
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ROW_start
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: POS_in_H
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: USED
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: FILLED
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: H_row
        INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: H_col
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: FUVALS
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: FT
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: GSCALE_used
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( :  , : ) :: GVALS
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: H_val
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: H_el
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: H_in
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: W_ws
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: W_el
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: W_in
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: G_temp
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : , : ) :: J_2d
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : , : ) :: H_2d
        REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : , : ) :: BAND_2d
        LOGICAL, ALLOCATABLE, DIMENSION( : ) :: LOGIC
      END TYPE CUTEST_work_type

!--------------------------------
!  G l o b a l  v a r i a b l e s
!--------------------------------

      TYPE ( CUTEST_data_type ), SAVE, PUBLIC :: CUTEST_data_global
      TYPE ( CUTEST_work_type ), ALLOCATABLE, DIMENSION( : ), SAVE, PUBLIC ::  &
        CUTEST_work_global

!  module procedures

    CONTAINS

!-  C U T E S T _ i n i t i a l i z e _ w o r k s p a c e  S U B R O U T I N E -

      SUBROUTINE CUTEST_initialize_workspace(                                  &
                    n, ng, nel, ntotel, nvrels, nnza, nvargp,                  &
                    IELING, ISTADG, IELVAR, ISTAEV, INTVAR, ISTADH, ICNA,      &
                    ISTADA, GXEQX, alllin, altriv, lfxi, lgxi, lhxi, lggfx,    &
                    ldx, lgrjac, lnguvl, lnhuvl, ntotin, maxsel, maxsin,       &
                    iprint, out, buffer, l_link_e_u_v, nbprod, FUVALS, lfuval, &
                    LINK_elem_uses_var, ISWKSP, IUSED, ISTAJC, ISTAGV, ISVGRP, &
                    ISLGRP, IGCOLJ, IVALJR, ISYMMH, LIST_elements,             &
                    NZ_components_w, W_ws, W_el, W_in,                         &
                    H_el, H_in, status, alloc_status, bad_alloc, array_status )

!  Compute the starting addresses for the partitions of the workspace array
!  FUVALS. Also fill relevant portions of the workspace arrays

!  History -
!   fortran 77 version originally released in CUTE, 20th June 1990
!   fortran 90 version released pre GALAHAD Version 1.0. February 1st 1995 as
!     INITW_initialize_workspace as part of the GALAHAD module INITW
!   fortran 2003 version released in CUTEst, 5th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, ng, nel, ntotel, nvrels, nnza
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: iprint, out, buffer
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: lfxi, lgxi, lhxi, lggfx, ldx
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: lnguvl, lnhuvl, nvargp, status
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: ntotin, maxsel, maxsin, lgrjac
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: alloc_status
      LOGICAL, INTENT( OUT ) :: altriv, alllin, array_status
      CHARACTER ( LEN = 24 ), INTENT( OUT ) :: bad_alloc
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ntotel  ) :: IELING
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ng  + 1 ) :: ISTADA
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ng  + 1 ) :: ISTADG
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel + 1 ) :: ISTAEV
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nvrels  ) :: IELVAR
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nnza    ) :: ICNA
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( nel + 1 ) :: ISTADH
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( nel + 1 ) :: INTVAR
      LOGICAL, INTENT( IN ), DIMENSION( ng  ) :: GXEQX

!-------------------------------------------------------------
!   D u m m y   A r g u m e n t s  f o r   w o r k s p a c e
!-------------------------------------------------------------

      INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: l_link_e_u_v, lfuval, nbprod

      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: FUVALS

      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ISWKSP
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: IUSED
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ISTAJC
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ISTAGV
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ISVGRP
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ISLGRP
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: IGCOLJ
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: IVALJR
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: LIST_elements
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: LINK_elem_uses_var
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: NZ_components_w
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : , : ) :: ISYMMH

      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: W_ws
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: W_el
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: W_in
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: H_el
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: H_in

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------

      INTEGER ( KIND = ip_ ) :: i, iel, ig, j, k, l, iielts, ientry, nsizeh
      INTEGER ( KIND = ip_ ) :: llink, mlink, nlink, ulink, nel1, lnwksp
      LOGICAL :: reallocate

!     CHARACTER ( LEN = 80 ) :: array

!  set constants

      nel1 = nel + 1
      alllin = nel == 0

!  set up INTVAR, the starting addresses for the element gradients with
!  respect to their internal variables. Also compute maxsin, the maximum
!  number of internal variables in an element

      IF ( .NOT. alllin ) THEN
        k = INTVAR( 1 )
        maxsin = k
        INTVAR( 1 ) = nel1
        DO iel = 2, nel
          l = INTVAR( iel )
          INTVAR( iel ) = INTVAR( iel - 1 ) + k
          k = l
          maxsin = MAX( maxsin, k )
        END DO
        INTVAR( nel1 ) = INTVAR( nel ) + k
      ELSE
        INTVAR( 1 ) = 1
        maxsin = 0
      END IF

!  compute the total number of internal variables

      ntotin = INTVAR( nel1 ) - INTVAR( 1 )

!  calculate the length, iielts, of workspace required to determine which
!  elements use each of the variables. Also find the maximum number of
!  variables in an element, maxsel. This is a dummy run merely to calculate
!  the space required

      llink = n + llink_min
      reallocate = .TRUE.
      IF ( ALLOCATED( LINK_elem_uses_var ) ) THEN
        IF ( SIZE( LINK_elem_uses_var ) < llink ) THEN
          DEALLOCATE( LINK_elem_uses_var )
        ELSE ; llink = SIZE( LINK_elem_uses_var ) ; reallocate = .FALSE.
        END IF
      END IF
      IF ( reallocate ) THEN
        ALLOCATE( LINK_elem_uses_var( llink ), STAT = alloc_status )
        IF ( alloc_status /= 0 ) THEN
          bad_alloc = 'LINK_e' ; GO TO 600 ; END IF
      END IF

!  LINK_elem_uses_var( i ) will be used as a list of links chaining the
!  elements using variable i. If LINK_elem_uses_var( i ) is negative, the
!  list is empty

      LINK_elem_uses_var( : n ) = - 1
      iielts = n ; maxsel = 0
      IF ( .NOT. alllin ) THEN

!  loop over the groups, considering each nonlinear element in turn

        DO iel = 1, nel
          maxsel = MAX( maxsel, ISTAEV( iel + 1 ) - ISTAEV( iel ) )
        END DO
        DO i = 1, ntotel
          iel = IELING( i )

!  loop on the variables from the i-th element

          DO k = ISTAEV( iel ), ISTAEV( iel + 1 ) - 1
            ientry = IELVAR( k )
            IF ( LINK_elem_uses_var( ientry ) >= 0 ) THEN

!  if we have reached the end of the list of the elements using the variable
!  IELVAR( K ), add the iel-th element to it. Otherwise, find the next entry
!  in the list

  30          CONTINUE
              IF ( LINK_elem_uses_var( ientry ) > 0 ) THEN
                ientry = LINK_elem_uses_var( ientry )
                GO TO 30
              ELSE
               IF ( iielts == llink ) THEN
                  nlink = llink
                  ulink = iielts; mlink = iielts + 1
                  CALL CUTEST_extend_array( LINK_elem_uses_var, llink, ulink,  &
                                            nlink, mlink, buffer, status,      &
                                            alloc_status)
                  IF ( status /= 0 ) THEN
                    bad_alloc = 'LINK_elem_uses_var' ; GO TO 610 ; END IF
                  llink = nlink
                END IF
                iielts = iielts + 1
                LINK_elem_uses_var( ientry ) = iielts
                LINK_elem_uses_var( iielts ) = 0
              END IF
            ELSE

!  the list of elements involving the variable IELVAR(k) was
!  previously empty. Indicate that the list has now been started and
!  that its end has been reached

              LINK_elem_uses_var( ientry ) = 0
            END IF
          END DO
        END DO
      END IF

      l_link_e_u_v = iielts

!  -- Calculate the starting addresses for the integer workspace --

!  ISWKSP( j ), j = 1, ..., MAX( ntotel, nel, n ), is used for workspace

      lnwksp = MAX( MAX( ntotel, nel ), n )

      reallocate = .TRUE.
      IF ( ALLOCATED( ISWKSP ) ) THEN
        IF ( SIZE( ISWKSP ) < lnwksp ) THEN ; DEALLOCATE( ISWKSP )
        ELSE ; reallocate = .FALSE. ; END IF
      END IF
      IF ( reallocate ) THEN
        ALLOCATE( ISWKSP( lnwksp ), STAT = alloc_status )
        IF ( alloc_status /= 0 ) THEN
          bad_alloc = 'ISWKSP' ; GO TO 600 ; END IF
      END IF

!  IUSED( j ), j = 1, ..., MAX( n, ng ) Will be used as workspace by
!  the matrix-vector product subroutine hessian_times_sp_vector

      reallocate = .TRUE.
      IF ( ALLOCATED( IUSED ) ) THEN
        IF ( SIZE( IUSED ) < MAX( n, ng ) ) THEN ; DEALLOCATE( IUSED )
        ELSE ; reallocate = .FALSE. ; END IF
      END IF
      IF ( reallocate ) THEN
        ALLOCATE( IUSED( MAX( n, ng ) ), STAT = alloc_status )
        IF ( alloc_status /= 0 ) THEN
          bad_alloc = 'IUSED' ; GO TO 600 ; END IF
      END IF

!  ISLGRP( j ), j = 1, ..., ntotel, will contain the number of the group
!  which uses nonlinear element j

      reallocate = .TRUE.
      IF ( ALLOCATED( ISLGRP ) ) THEN
        IF ( SIZE( ISLGRP ) < ntotel ) THEN ; DEALLOCATE( ISLGRP )
        ELSE ; reallocate = .FALSE. ; END IF
      END IF
      IF ( reallocate ) THEN
        ALLOCATE( ISLGRP( ntotel ), STAT = alloc_status )
        IF ( alloc_status /= 0 ) THEN
          bad_alloc = 'ISLGRP' ; GO TO 600 ; END IF
      END IF

!  ISTAJC( j ), j = 1, ..., n, will contain the starting addresses for
!  the list of nontrivial groups which use the j-th variable.
!  ISTAJC( n + 1 ) will point to the first free location in IGCOLJ after
!  the list of nontrivial groups for the n-th variable

      reallocate = .TRUE.
      IF ( ALLOCATED( ISTAJC ) ) THEN
        IF ( SIZE( ISTAJC ) < n + 1 ) THEN ; DEALLOCATE( ISTAJC )
        ELSE ; reallocate = .FALSE. ; END IF
      END IF
      IF ( reallocate ) THEN
        ALLOCATE( ISTAJC( n + 1 ), STAT = alloc_status )
        IF ( alloc_status /= 0 ) THEN
          bad_alloc = 'ISTAJC' ; GO TO 600 ; END IF
      END IF

!  ISTAGV( j ), j = 1, ..., ng, will contain the starting addresses for
!  the list of variables which occur in the J-th group. ISTAGV( ng + 1 )
!  will point to the first free location in ISVGRP after the list of variables
!  for the NG-th group

      reallocate = .TRUE.
      IF ( ALLOCATED( ISTAGV ) ) THEN
        IF ( SIZE( ISTAGV ) < ng + 1 ) THEN ; DEALLOCATE( ISTAGV )
        ELSE ; reallocate = .FALSE. ; END IF
      END IF
      IF ( reallocate ) THEN
        ALLOCATE( ISTAGV( ng + 1 ), STAT = alloc_status )
        IF ( alloc_status /= 0 ) THEN
          bad_alloc = 'ISTAGV' ; GO TO 600 ; END IF
      END IF

!  Allocate LIST_elements

      reallocate = .TRUE.
      IF ( ALLOCATED( LIST_elements ) ) THEN
        IF ( SIZE( LIST_elements ) < l_link_e_u_v ) THEN
          DEALLOCATE( LIST_elements )
        ELSE ; reallocate = .FALSE. ; END IF
      END IF
      IF ( reallocate ) THEN
        ALLOCATE( LIST_elements( l_link_e_u_v ), STAT = alloc_status )
        IF ( alloc_status /= 0 ) THEN
          bad_alloc = 'LIST_e' ; GO TO 600 ; END IF
      END IF

!  determine which elements use each variable. Initialization

      IF ( .NOT. alllin ) THEN

!  LINK_elem_uses_var( i ) will be used as a list of links chaining the
!  elements using variable i. If LINK_elem_uses_var( i ) is negative, the
!  list is empty

        LINK_elem_uses_var( : n ) = - 1
        LIST_elements( : n ) = - 1   ! needed for epcf90 debugging compiler
        iielts = n

!  loop over the groups, considering each nonlinear element in turn

        DO i = 1, ntotel
          iel = IELING( i )

!  loop on the variables of the I-th element

          DO k = ISTAEV( iel ), ISTAEV( iel + 1 ) - 1
            ientry = IELVAR( k )
            IF ( LINK_elem_uses_var( ientry ) >= 0 ) THEN

!  if we have reached the end of the list of the elements using the variable
!  IELVAR( K ), add the I-th element to it and record that the end of the list
!  has occured. Otherwise, find the next entry in the list

  110         CONTINUE
              IF ( LINK_elem_uses_var( ientry ) > 0 ) THEN
                ientry = LINK_elem_uses_var( ientry )
                GO TO 110
              ELSE
                iielts = iielts + 1
                LINK_elem_uses_var( ientry ) = iielts
                LINK_elem_uses_var( iielts ) = 0
                LIST_elements( iielts ) = i
              END IF
            ELSE

!  the list of elements involving the variable IELVAR( K ) was previously
!  empty. Indicate that the list has now been started, record the element
!  which contains the variable and indicate that the end of the list has
!  been reached

              LINK_elem_uses_var( ientry ) = 0
              LIST_elements( ientry ) = i
            END IF
          END DO
        END DO
      END IF

!  deallocate arrays that have no further use

!      DEALLOCATE( LINK_elem_uses_var, STAT = alloc_status )
!      IF ( alloc_status /= 0 ) THEN
!        bad_alloc = 'LINK_elem_uses_var' ; GO TO 600 ; END IF

!  set up symmetric addresses for the upper triangular storage
!  schemes for the element hessians

      IF ( maxsin > 0 ) THEN
        reallocate = .TRUE.
        IF ( ALLOCATED( ISYMMH ) ) THEN
          IF ( SIZE( ISYMMH, 1 ) /= maxsin .OR. SIZE( ISYMMH, 2 ) /= maxsin ) &
            THEN  ; DEALLOCATE( ISYMMH ) ; ELSE ; reallocate = .FALSE. ; END IF
        END IF
        IF ( reallocate ) THEN
          ALLOCATE( ISYMMH( maxsin, maxsin ), STAT = alloc_status )
          IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'ISYMMH' ; GO TO 600
          END IF
        END IF

        CALL CUTEST_symmh( maxsin, ISYMMH )
      ELSE
        ALLOCATE( ISYMMH( 0, 0 ), STAT = alloc_status )
        IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'ISYMMH' ; GO TO 600
        END IF
      END IF

!  set up the starting addresses for the element Hessians
!  with respect to their internal variables and a pointer beyond
!  the end of the space required for the Hessians

      lggfx = INTVAR( nel1 )
      IF ( .NOT. alllin ) THEN
        DO i = 1, nel
          ISTADH( i ) = lggfx
          nsizeh = INTVAR( i + 1 ) - INTVAR( i )
          lggfx = lggfx + nsizeh * ( nsizeh + 1 ) / 2
        END DO
      END IF
      ISTADH( nel1 ) = lggfx

!  altriv specifies whether all the groups are trivial

      altriv = .TRUE.

!  pass 1: Count the total number of variables in all the groups, nvargp

      nvargp = 0

!  start by initializing the counting array to zero

      ISWKSP( : n ) = 0

!  loop over the groups. See if the ig-th group is trivial

      DO ig = 1, ng

!  check to see if all of the groups are trivial

        IF ( .NOT. GXEQX( ig ) ) altriv = .FALSE.

!  loop over the nonlinear elements from the ig-th group

        DO k = ISTADG( ig ), ISTADG( ig + 1 ) - 1
          iel = IELING( k )

!  run through all the elemental variables changing the I-th entry of
!  ISWKSP from zero to one if variable I appears in an element

          DO j = ISTAEV( iel ), ISTAEV( iel + 1 ) - 1
            i = IELVAR( j )
            IF ( ISWKSP( i ) < ig ) THEN
              ISWKSP( i ) = ig
              nvargp = nvargp + 1
            END IF
          END DO
        END DO

!  consider variables which arise from the linear element

        DO j = ISTADA( ig ), ISTADA( ig + 1 ) - 1
          i = ICNA( j )
          IF ( i <= n ) THEN
            IF ( ISWKSP( i ) < ig ) THEN
               ISWKSP( i ) = ig
               nvargp = nvargp + 1
            END IF
          END IF
        END DO
      END DO

!  ISVGRP( j ), j = 1, ..., nvargp, will contain the indices of the
!  variables which are used by each group in turn. Those for group i occur
!  in locations ISTAGV( i ) to ISTAGV( i + 1 ) - 1

!  allocate the array ISVGRP

      reallocate = .TRUE.
      IF ( ALLOCATED( ISVGRP ) ) THEN
        IF ( SIZE( ISVGRP ) < nvargp ) THEN ; DEALLOCATE( ISVGRP )
        ELSE ; reallocate = .FALSE. ; END IF
      END IF
      IF ( reallocate ) THEN
        ALLOCATE( ISVGRP( nvargp ), STAT = alloc_status )
        IF ( alloc_status /= 0 ) THEN
          bad_alloc = 'ISVGRP' ; GO TO 600 ; END IF
      END IF

!  store the indices of variables which appears in each group and how many
!  groups use each variable. Reinitialize counting arrays to zero

      ISTAJC( 2 : n + 1 ) = 0
      ISWKSP( : n ) = 0

!  pass 2: store the list of variables

      nvargp = 0
      ISTAGV( 1 ) = 1

!  loop over the groups. See if the ig-th group is trivial

      DO ig = 1, ng

!  again, loop over the nonlinear elements from the ig-th group

        DO k = ISTADG( ig ), ISTADG( ig + 1 ) - 1
          iel = IELING( k )

!  run through all the elemental variables changing the i-th entry of
!  ISWKSP from zero to one if variable I appears in an element

          DO j = ISTAEV( iel ), ISTAEV( iel + 1 ) - 1
            i = IELVAR( j )
            IF ( ISWKSP( i ) < ig ) THEN
              ISWKSP( i ) = ig

!  record the nonlinear variables from the ig-th group

              nvargp = nvargp + 1
              ISVGRP( nvargp ) = i
            END IF
          END DO

!  record that nonlinear element K occurs in group IELGRP(iel)

          ISLGRP( k ) = ig
        END DO

!  consider variables which arise from the linear element

        DO j = ISTADA( ig ), ISTADA( ig + 1 ) - 1
          i = ICNA( j )
          IF ( i <= n ) THEN
            IF ( ISWKSP( i ) < ig ) THEN
              ISWKSP( i ) = ig

!  record the linear variables from the ig-th group

              nvargp = nvargp + 1
              ISVGRP( nvargp ) = i
            END IF
          END IF
        END DO

!  record that one further nontrivial group uses variable l-1

        IF ( .NOT. GXEQX( ig ) ) THEN
          DO j = ISTAGV( ig ), nvargp
            l = ISVGRP( j ) + 1
            ISTAJC( l ) = ISTAJC( l ) + 1
          END DO
        END IF

!  record the starting address of the variables in the next group

        ISTAGV( ig + 1 ) = nvargp + 1
      END DO

!  deallocate arrays that have no further use

      nbprod = 0
      ISWKSP( : lnwksp ) = 0

!     DEALLOCATE( ISWKSP, STAT = alloc_status )
!     IF ( alloc_status /= 0 ) THEN
!       bad_alloc = 'ISWKSP' ; GO TO 600 ; END IF

!  IGCOLJ( j ), j = 1, ..., nvargp, will contain the indices of the
!  nontrivial groups which use each variable in turn. Those for variable i
!  occur in locations ISTAJC( i ) to ISTAJC( i + 1 ) - 1

      reallocate = .TRUE.
      IF ( ALLOCATED( IGCOLJ ) ) THEN
        IF ( SIZE( IGCOLJ ) < nvargp ) THEN ; DEALLOCATE( IGCOLJ )
        ELSE ; reallocate = .FALSE. ; END IF
      END IF
      IF ( reallocate ) THEN
        ALLOCATE( IGCOLJ( nvargp ), STAT = alloc_status )
        IF ( alloc_status /= 0 ) THEN
          bad_alloc = 'IGCOLJ' ; GO TO 600 ; END IF
      END IF

!  IVALJR( j ), j = 1, ..., nvargp, will contain the positions in GRJAC of the
!  nonzeros of the Jacobian of the groups corresponding to the variables as
!  ordered in ISVGRP( j )

       reallocate = .TRUE.
       IF ( ALLOCATED( IVALJR ) ) THEN
         IF ( SIZE( IVALJR ) < nvargp ) THEN ; DEALLOCATE( IVALJR )
         ELSE ; reallocate = .FALSE. ; END IF
       END IF
       IF ( reallocate ) THEN
         ALLOCATE( IVALJR( nvargp ), STAT = alloc_status )
         IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'IVALJR' ; GO TO 600
         END IF
       END IF

!  set the starting addresses for the lists of nontrivial groups which use
!  each variable in turn

      k = 1
      ISTAJC( k ) = 1
      DO i = 2, n + 1
        k = k + 1
        ISTAJC( k ) = ISTAJC( k ) + ISTAJC( k - 1 )
      END DO

!  consider the ig-th group in order to associate variables with groups

      DO ig = 1, ng
        IF ( .NOT. GXEQX( ig ) ) THEN
          DO i = ISTAGV( ig ), ISTAGV( ig + 1 ) - 1
            l = ISVGRP( i )

!  record that group ig uses variable ISVGRP( i )

            j = ISTAJC( l )
            IGCOLJ( j ) = ig

!  store the locations in the Jacobian of the groups of the nonzeros
!  corresponding to each variable in the IG-TH group. Increment the starting
!  address for the pointer to the next group using variable ISVGRP( i )

            IVALJR( i ) = j
            ISTAJC( l ) = j + 1
          END DO
        END IF
      END DO

!  reset the starting addresses for the lists of groups using each variable

      DO i = n, 2, - 1
        ISTAJC( i ) = ISTAJC( i - 1 )
      END DO
      ISTAJC( 1 ) = 1

!  Initialize workspace values for subroutine hessian_times_sp_vector

      IUSED( : MAX( n, ng ) ) = 0

!  initialize general workspace arrays

      maxsin = MAX( 1, maxsin )
      maxsel = MAX( 1, maxsel )

      IF ( ALLOCATED( NZ_components_w ) ) DEALLOCATE( NZ_components_w )
      ALLOCATE( NZ_components_w( ng ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'NZ_com' ; GO TO 600
      END IF

      IF ( ALLOCATED( W_ws ) ) DEALLOCATE( W_ws )
      ALLOCATE( W_ws( MAX( n, ng ) ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'W_ws' ; GO TO 600
      END IF

      IF ( ALLOCATED( W_el ) ) DEALLOCATE( W_el )
      ALLOCATE( W_el( maxsel ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'W_el' ; GO TO 600
      END IF

      IF ( ALLOCATED( W_in ) ) DEALLOCATE( W_in )
      ALLOCATE( W_in( maxsin ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'W_in' ; GO TO 600
      END IF

      IF ( ALLOCATED( H_el ) ) DEALLOCATE( H_el )
      ALLOCATE( H_el( maxsel ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'H_el' ; GO TO 600
      END IF

      IF ( ALLOCATED( H_in ) ) DEALLOCATE( H_in )
      ALLOCATE( H_in( maxsin ), STAT = alloc_status )
      IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'H_in' ; GO TO 600
      END IF

!  set the length of the remaining partitions of the workspace for array bound
!  checking in calls to other subprograms

!  -- set the starting addresses for the partitions within FUVALS --

!  a full description of the partitions of FUVALS is given in the introductory
!  comments to the LANCELOT package

      lfxi = 0
      lgxi = lfxi + nel
      lhxi = INTVAR( nel1 ) - 1
      lggfx = lggfx - 1
      ldx = lggfx + n
      lgrjac = ldx + n
      lfuval = lgrjac + nvargp + 1

!  print all of the starting addresses for the workspace array partitions

       IF ( iprint >= 3 ) WRITE( out,                                          &
            "( /,' Starting addresses for the partitions of FUVALS ', /,       &
         &       ' ----------------------------------------------- ', //,      &
         &       '   lfxi   lgxi   lhxi  lggfx    ldx   lgrjac', /, 6I7 )" )   &
           lfxi, lgxi, lhxi, lggfx, ldx, lgrjac

!  set the length of each partition of the real workspace array FUVALS for
!  array bound checking in calls to other subprograms

      lnguvl = MAX( 1, lhxi - lfxi )
      lnhuvl = MAX( 1, lggfx - lfxi )

!  allocate FUVALS

      reallocate = .TRUE.
      IF ( ALLOCATED( FUVALS ) ) THEN
        IF ( SIZE( FUVALS ) < lfuval ) THEN ; DEALLOCATE( FUVALS )
        ELSE ; reallocate = .FALSE. ; END IF
      END IF
      IF ( reallocate ) THEN
        ALLOCATE( FUVALS( lfuval ), STAT = alloc_status )
        IF ( alloc_status /= 0 ) THEN
          bad_alloc = 'FUVALS' ; GO TO 600 ; END IF
      END IF

!  set defalt gradient and Hessian values to zero

      FUVALS( lgxi + 1 : lggfx ) = 0.0_rp_

!  mark remaining arrays as unallocated

      array_status = .FALSE.

      status = 0
      RETURN

!  unsuccessful returns

  600 CONTINUE
      status = 1000 + alloc_status

  610 CONTINUE
      WRITE( out, 2600 ) bad_alloc, alloc_status
      RETURN

!  non-executable statements

 2600 FORMAT( ' ** Message from -CUTEST_initialize_workspace-', /,             &
              ' Allocation error, for ', A, ', status = ', I0 )

!  end of subroutine CUTEST_initialize_workspace

      END SUBROUTINE CUTEST_initialize_workspace

!-*-*-*-  C U T E S T _ f o r m _ g r a d i e n t s  S U B R O U T I N E -*-*-*-

      SUBROUTINE CUTEST_form_gradients(                                        &
                       n, ng, nel, ntotel, nvrels, nnza, nvargp,               &
                       firstg, ICNA, ISTADA, IELING, ISTADG, ISTAEV,           &
                       IELVAR, INTVAR, A, GVALS2, GUVALS, lguval,              &
                       GRAD, GSCALE, ESCALE, GRJAC, GXEQX, INTREP,             &
                       ISVGRP, ISTAGV, ITYPEE, ISTAJC, GRAD_el, W_el,          &
                       RANGE, KNDOFG )

!  Calculate the the gradient, GRAD, of the objective function and the
!  Jacobian matrix of gradients, GRJAC, of each group

!  History -
!   ( based on Conn-Gould-Toint fortran 77 version LANCELOT A, ~1992 )
!   fortran 90 version originally released pre GALAHAD Version 1.0. February
!     7th 1995 as LANCELOT_form_gradients as part of the LANCELOT module
!   update released with GALAHAD Version 2.0. February 16th 2005
!   fortran 2003 version released in CUTEst, 5th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, ng, nel, ntotel, nnza, nvargp
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: nvrels, lguval
      LOGICAL, INTENT( IN ) :: firstg
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ng  + 1 ) :: ISTADA
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ng  + 1 ) :: ISTADG
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel + 1 ) :: ISTAEV
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel + 1 ) :: INTVAR
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nvrels  ) :: IELVAR
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nnza    ) :: ICNA
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ntotel  ) :: IELING
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( nnza ) :: A
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( ng ) :: GVALS2
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( lguval ) :: GUVALS
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( ng ) :: GSCALE
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( ntotel ) :: ESCALE
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: GRAD
      REAL ( KIND = rp_ ), INTENT( INOUT ), DIMENSION( nvargp ) :: GRJAC
      LOGICAL, INTENT( IN ), DIMENSION( ng  ) :: GXEQX
      LOGICAL, INTENT( IN ), DIMENSION( nel ) :: INTREP
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( : ) :: ISVGRP
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( : ) :: ISTAGV
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel ) :: ITYPEE
      INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( : ) :: ISTAJC
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: GRAD_el
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: W_el
      INTEGER ( KIND = ip_ ), INTENT( IN ), OPTIONAL, DIMENSION( ng ) :: KNDOFG

!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------

      INTERFACE
        SUBROUTINE RANGE( ielemn, transp, W1, W2, nelvar, ninvar, ieltyp,      &
                          lw1, lw2 )
        USE CUTEST_KINDS_precision
        INTEGER ( KIND = ip_ ), INTENT( IN ) :: ielemn, nelvar, ninvar
        INTEGER ( KIND = ip_ ), INTENT( IN ) :: ieltyp, lw1, lw2
        LOGICAL, INTENT( IN ) :: transp
        REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION ( lw1 ) :: W1
        REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION ( lw2 ) :: W2
        END SUBROUTINE RANGE
      END INTERFACE

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------

      INTEGER ( KIND = ip_ ) :: i, iel, ig, ii, k, ig1, j, jj, l, ll
      INTEGER ( KIND = ip_ ) :: nin, nvarel, nelow, nelup, istrgv, iendgv
      REAL ( KIND = rp_ ) :: gi, scalee
      LOGICAL :: nontrv

!  Initialize the gradient as zero

      GRAD = 0.0_rp_

!  consider the IG-th group

      DO ig = 1, ng
        IF ( PRESENT( KNDOFG ) ) THEN
          IF ( KNDOFG( ig ) == 0 ) CYCLE ; END IF
        ig1 = ig + 1
        istrgv = ISTAGV( ig ) ; iendgv = ISTAGV( ig1 ) - 1
        nelow = ISTADG( ig ) ; nelup = ISTADG( ig1 ) - 1
        nontrv = .NOT. GXEQX( ig )

!  compute the first derivative of the group

        IF ( nontrv ) THEN
          gi = GSCALE( ig ) * GVALS2( ig )
        ELSE
          gi = GSCALE( ig )
        END IF

!  this is the first gradient evaluation or the group has nonlinear elements

        IF ( firstg .OR. nelow <= nelup ) THEN
          GRAD_el( ISVGRP( istrgv : iendgv ) ) = 0.0_rp_

!  loop over the group's nonlinear elements

          DO ii = nelow, nelup
            iel = IELING( ii )
            k = INTVAR( iel ) ; l = ISTAEV( iel )
            nvarel = ISTAEV( iel + 1 ) - l
            scalee = ESCALE( ii )
            IF ( INTREP( iel ) ) THEN

!  the iel-th element has an internal representation

              nin = INTVAR( iel + 1 ) - k
              CALL RANGE( iel, .TRUE., GUVALS( k : k + nin - 1 ),              &
                          W_el( : nvarel ), nvarel, nin, ITYPEE( iel ),        &
                          nin, nvarel )
!DIR$ IVDEP
              DO i = 1, nvarel
                j = IELVAR( l )
                GRAD_el( j ) = GRAD_el( j ) + scalee * W_el( i )
                l = l + 1
              END DO
            ELSE

!  the iel-th element has no internal representation

!DIR$ IVDEP
              DO i = 1, nvarel
                j = IELVAR( l )
                GRAD_el( j ) = GRAD_el( j ) + scalee * GUVALS( k )
                k = k + 1
                l = l + 1
              END DO
            END IF
          END DO

!  include the contribution from the linear element

!DIR$ IVDEP
          DO k = ISTADA( ig ), ISTADA( ig1 ) - 1
            GRAD_el( ICNA( k ) ) = GRAD_el( ICNA( k ) ) + A( k )
          END DO

!  find the gradient of the group

          IF ( nontrv ) THEN

!  The group is non-trivial

!DIR$ IVDEP
            DO i = istrgv, iendgv
              ll = ISVGRP( i )
              GRAD( ll ) = GRAD( ll ) + gi * GRAD_el( ll )

!  as the group is non-trivial, also store the nonzero entries of the
!  gradient of the function in GRJAC

              jj = ISTAJC( ll )
              GRJAC( jj ) = GRAD_el( ll )

!  increment the address for the next nonzero in the column of the Jacobian
!  for variable ll

              ISTAJC( ll ) = jj + 1
            END DO
          ELSE

!  the group is trivial

!DIR$ IVDEP
            DO i = istrgv, iendgv
              ll = ISVGRP( i )
              GRAD( ll ) = GRAD( ll ) + gi * GRAD_el( ll )
            END DO
          END IF

!  this is not the first gradient evaluation and there is only a linear element

        ELSE

!  add the gradient of the linear element to the overall gradient

!DIR$ IVDEP
          DO k = ISTADA( ig ), ISTADA( ig1 ) - 1
            GRAD( ICNA( k ) ) = GRAD( ICNA( k ) ) + gi * A( k )
          END DO

!  the group is non-trivial; increment the starting addresses for
!  the groups used by each variable in the (unchanged) linear
!  element to avoid resetting the nonzeros in the Jacobian

          IF ( nontrv ) THEN
!DIR$ IVDEP
            DO i = istrgv, iendgv
              ISTAJC( ISVGRP( i ) ) = ISTAJC( ISVGRP( i ) ) + 1
            END DO
          END IF
        END IF
      END DO

!  reset the starting addresses for the lists of groups using each variable to
!  their values on entry

      DO i = n, 2, - 1
        ISTAJC( i ) = ISTAJC( i - 1 )
      END DO
      ISTAJC( 1 ) = 1

      RETURN

!  end of subroutine CUTEST_form_gradients

      END SUBROUTINE CUTEST_form_gradients

!-*-*-  C U T E S T _ a s s e m b l e _ h e s s i a n  S U B R O U T I N E -*-*-

      SUBROUTINE CUTEST_assemble_hessian(                                      &
                      n, ng, nel, ntotel, nvrels, nnza, maxsel, nvargp,        &
                      ISTADH, ICNA, ISTADA, INTVAR, IELVAR, IELING, ISTADG,    &
                      ISTAEV, ISTAGV, ISVGRP, A, GUVALS, lnguvl, HUVALS,       &
                      lnhuvl, GVALS2, GVALS3, GSCALE, ESCALE, GXEQX,           &
                      ITYPEE, INTREP, RANGE, iprint, error, out,               &
                      fixed_structure, use_band, nsemib, status,               &
                      alloc_status, bad_alloc, hessian_setup_complete,         &
                      lh_row, lh_col, lh_val, H_row, H_col, H_val, ROW_start,  &
                      POS_in_H, USED, FILLED, lrowst, lpos, lused, lfilled,    &
                      GRAD_el, W_el, W_in, H_el, H_in,                         &
                      nnzh, maxsbw, DIAG, OFFDIA )

!  Assemble the second derivative matrix of a groups partially separable
!  function in either co-ordinate or band format

!  History -
!   ( based on Conn-Gould-Toint fortran 77 version LANCELOT A, ~1992 )
!   fortran 90 version originally released pre GALAHAD Version 1.0. January
!     25th 1995 as ASMBL_assemble_hessian as part of the ASMBL module
!   update released with GALAHAD Version 2.0. February 16th 2005
!   fortran 2003 version released in CUTEst, 5th November 2012
!   completely revised version 14th June 2013

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, nel, ng, maxsel, nsemib
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: nvrels, ntotel, nvargp, nnza
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: lnguvl, lnhuvl, iprint, error, out
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, alloc_status
      LOGICAL, INTENT( IN ) :: fixed_structure, use_band
      LOGICAL, INTENT( INOUT ) :: hessian_setup_complete
      CHARACTER ( LEN = 24 ) :: bad_alloc
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nnza ) :: ICNA
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ng  + 1 ) :: ISTADA
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ng  + 1 ) :: ISTADG
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ng  + 1 ) :: ISTAGV
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel + 1 ) :: INTVAR
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel + 1 ) :: ISTAEV
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel + 1 ) :: ISTADH
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nvrels ) :: IELVAR
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ntotel ) :: IELING
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nvargp ) :: ISVGRP
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel ) :: ITYPEE
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( nnza ) :: A
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( lnguvl ) :: GUVALS
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( lnhuvl ) :: HUVALS
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( ng ) :: GVALS2
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( ng ) :: GVALS3
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( ng ) :: GSCALE
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( ntotel ) :: ESCALE
      LOGICAL, INTENT( IN ), DIMENSION( ng ) :: GXEQX
      LOGICAL, INTENT( IN ), DIMENSION( nel ) :: INTREP

!---------------------------------------------------------------
!   D u m m y   A r g u m e n t s   f o r   W o r k s p a c e
!--------------------------------------------------------------

      INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: lh_row, lh_col, lh_val
      INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: lrowst, lpos, lused, lfilled
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ROW_start
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: POS_in_H
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: USED
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: FILLED
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: H_row
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: H_col
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: H_val

      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: GRAD_el
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: W_el
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: W_in
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: H_el
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: H_in

!--------------------------------------------------
!   O p t i o n a l   D u m m y   A r g u m e n t s
!--------------------------------------------------

      INTEGER ( KIND = ip_ ), INTENT( OUT ), OPTIONAL :: maxsbw, nnzh
      REAL ( KIND = rp_ ), INTENT( OUT ), OPTIONAL,                            &
                                         DIMENSION( n ) :: DIAG
      REAL ( KIND = rp_ ), INTENT( OUT ), OPTIONAL,                            &
                                         DIMENSION( nsemib, n ) :: OFFDIA

!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------

      INTERFACE
        SUBROUTINE RANGE( ielemn, transp, W1, W2, nelvar, ninvar, ieltyp,      &
                          lw1, lw2 )
        USE CUTEST_KINDS_precision
        INTEGER ( KIND = ip_ ), INTENT( IN ) :: ielemn, nelvar, ninvar
        INTEGER ( KIND = ip_ ), INTENT( IN ) :: ieltyp, lw1, lw2
        LOGICAL, INTENT( IN ) :: transp
        REAL ( KIND = rp_ ), INTENT( IN  ), DIMENSION ( lw1 ) :: W1
        REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION ( lw2 ) :: W2
        END SUBROUTINE RANGE
      END INTERFACE

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------

      INTEGER ( KIND = ip_ ) :: i, ii, j, jj, k, kk, ig, l, ijhess
      INTEGER ( KIND = ip_ ) :: irow, jcol, jcolst, ihnext, n_filled, nin
      INTEGER ( KIND = ip_ ) :: iel, iell, ielh, nvarel, ig1, listvs, listve
      REAL ( KIND = rp_ ) :: wki, hesnew, gdash, g2dash, scalee
      CHARACTER ( LEN = 2 ), DIMENSION( 36, 36 ) :: MATRIX
!     CHARACTER ( LEN = 80 ) :: array

!  if a band storage scheme is to be used, initialize the entries within the
!  band as zero

      IF ( use_band ) THEN
        maxsbw = 0
        DIAG = 0.0_rp_ ; OFFDIA = 0.0_rp_

!  if a co-ordinate scheme is to be used, determine the rows structure of the
!  second derivative matrix of a groups partially separable function with
!  possible repititions if this has not already been done

      ELSE
        IF ( .NOT. hessian_setup_complete ) THEN
          CALL CUTEST_sparse_hessian_by_rows(                                  &
                          n, ng, nel, ntotel, nvrels, nvargp, IELVAR,          &
                          IELING, ISTADG, ISTAEV, ISTAGV, ISVGRP, GXEQX,       &
                          error, status, alloc_status, bad_alloc,              &
                          ROW_start, POS_in_H, lrowst, lpos )
          IF ( status /= 0 ) RETURN

!  allocate workspace if required

          lused = n
          CALL CUTEST_allocate_array( USED, lused, alloc_status )
          IF ( alloc_status /= 0 ) THEN
            bad_alloc = 'USED' ; GO TO 980 ; END IF

          lfilled = n
          CALL CUTEST_allocate_array( FILLED, lfilled, alloc_status )
          IF ( alloc_status /= 0 ) THEN
            bad_alloc = 'FILLED' ; GO TO 980 ; END IF

!  now pass through the nonzeros, setting up the position in the future
!  H_row and H_col arrays of the data gathered from the groups

          USED = 0
          k = 1
          DO i = 1, n
            n_filled = 0
            DO l = ROW_start( i ), ROW_start( i + 1 ) - 1
              j = POS_in_H( l )
              IF ( USED( j ) == 0 ) THEN
                n_filled = n_filled + 1
                FILLED( n_filled ) = j
                USED( j ) = k
                POS_in_H( l ) = k
                k = k + 1
              ELSE
                POS_in_H( l ) = USED( j )
              END IF
            END DO
            USED( FILLED( 1 : n_filled ) ) = 0
          END DO
          nnzh = k - 1

!  allocate space for the row and column indices and values

          lh_row = nnzh
          CALL CUTEST_allocate_array( H_row, lh_row, alloc_status )
          IF ( alloc_status /= 0 ) THEN
            bad_alloc = 'H_row' ; GO TO 980 ; END IF

          lh_col = nnzh
          CALL CUTEST_allocate_array( H_col, lh_col, alloc_status )
          IF ( alloc_status /= 0 ) THEN
            bad_alloc = 'H_col' ; GO TO 980 ; END IF

          lh_val = nnzh
          CALL CUTEST_allocate_array( H_val, lh_val, alloc_status )
          IF ( alloc_status /= 0 ) THEN
            bad_alloc = 'H_val' ; GO TO 980 ; END IF

          hessian_setup_complete = .TRUE.
        ELSE
          nnzh = lh_row
        END IF
        H_val( : nnzh ) = 0.0_rp_
      END IF

!  ---------------------------------------
!  set the row and column lists and values
!  ---------------------------------------

!  consider the rank-one second order term for the i-th group

      DO ig = 1, ng
        IF ( GXEQX( ig ) ) CYCLE
        IF ( .NOT. fixed_structure .AND. GSCALE( ig ) == 0.0_rp_ ) CYCLE
        IF ( iprint >= 100 ) WRITE( out,                                       &
          "( ' Group ', I5, ' rank-one terms ' )" ) ig
        g2dash = GSCALE( ig ) * GVALS3( ig )
        IF ( iprint >= 100 ) WRITE( 6, * ) ' GVALS3( ig ) ', GVALS3( ig )
        ig1 = ig + 1
        listvs = ISTAGV( ig )
        listve = ISTAGV( ig1 ) - 1

!  form the gradient of the ig-th group

        GRAD_el( ISVGRP( listvs : listve ) ) = 0.0_rp_

!  consider any nonlinear elements for the group

        DO iell = ISTADG( ig ), ISTADG( ig1 ) - 1
          iel = IELING( iell )
          k = INTVAR( iel )
          l = ISTAEV( iel )
          nvarel = ISTAEV( iel + 1 ) - l
          scalee = ESCALE( iell )

!  the iel-th element has an internal representation

          IF ( INTREP( iel ) ) THEN
            nin = INTVAR( iel + 1 ) - k
            CALL RANGE( iel, .TRUE., GUVALS( k : k + nin - 1 ),                &
                        H_el, nvarel, nin, ITYPEE( iel ), nin, nvarel )
            DO i = 1, nvarel
              j = IELVAR( l )
              GRAD_el( j ) = GRAD_el( j ) + scalee * H_el( i )
              l = l + 1
            END DO

!  the iel-th element has no internal representation

          ELSE
            DO i = 1, nvarel
              j = IELVAR( l )
              GRAD_el( j ) = GRAD_el( j ) + scalee * GUVALS( k )
              k = k + 1 ; l = l + 1
            END DO
          END IF
        END DO

!  include the contribution from the linear element

        DO k = ISTADA( ig ), ISTADA( ig1 ) - 1
          j = ICNA( k )
          GRAD_el( j ) = GRAD_el( j ) + A( k )
        END DO

!  the gradient is complete. Form the j-th column of the rank-one matrix

        DO l = listvs, listve
          j = ISVGRP( l )
          IF ( j == 0 ) CYCLE

!  find the entry in row i of this column

          DO k = listvs, listve
            i = ISVGRP( k )
            IF ( i == 0 .OR. i > j ) CYCLE

!  Skip all elements which lie outside a band of width nsemib

            IF ( use_band ) maxsbw = MAX( maxsbw, j - i )
            IF ( j - i > nsemib ) CYCLE
            hesnew = GRAD_el( i ) * GRAD_el( j ) * g2dash
            IF ( iprint >= 100 ) WRITE( out,                                   &
              "( ' Row ', I6, ' column ', I6, ' used. Value = ', ES24.16 )" )  &
                i, j, hesnew

!  obtain the appropriate storage location in H for the new entry

!  Case 1: band matrix storage scheme

            IF ( use_band ) THEN

!  the entry belongs on the diagonal

              IF ( i == j ) THEN
                DIAG( i ) = DIAG( i ) + hesnew

!  the entry belongs off the diagonal

              ELSE
                OFFDIA( j - i, i ) = OFFDIA( j - i, i ) + hesnew
              END IF

!  Case 2: co-ordinate storage scheme

            ELSE

!  there is an entry in position (i,j) to be stored in
!  H_row/col(COL(ROW_start(i)))

              kk = POS_in_H( ROW_start( i ) )
              H_row( kk ) = i
              H_col( kk ) = j
              H_val( kk ) = H_val( kk ) + hesnew
              ROW_start( i ) = ROW_start( i ) + 1
            END IF
          END DO
        END DO
      END DO

!  reset the workspace array to zero

      W_el( : maxsel ) = 0.0_rp_

!  now consider the low rank first order terms for the i-th group

      DO ig = 1, ng
        IF ( .NOT. fixed_structure .AND. GSCALE( ig ) == 0.0_rp_ ) CYCLE
        IF ( iprint >= 100 ) WRITE( out,                                       &
          "( ' Group ', I5, ' second-order terms ' )" )  ig
        IF ( GXEQX( ig ) ) THEN
          gdash = GSCALE( ig )
        ELSE
          gdash = GSCALE( ig ) * GVALS2( ig )
          IF ( iprint >= 100 ) WRITE( 6, * ) ' GVALS2( ig )', GVALS2( ig )
        END IF
        ig1 = ig + 1

!  see if the group has any nonlinear elements

        DO iell = ISTADG( ig ), ISTADG( ig + 1 ) - 1
          iel = IELING( iell )
          listvs = ISTAEV( iel )
          listve = ISTAEV( iel + 1 ) - 1
          nvarel = listve - listvs + 1
          ielh = ISTADH( iel )
          ihnext = ielh
          scalee = ESCALE( iell )
          DO l = listvs, listve
            j = IELVAR( l )
            IF ( j /= 0 ) THEN

!  the iel-th element has an internal representation. Compute the j-th column
!  of the element Hessian matrix

              IF ( INTREP( iel ) ) THEN

!  compute the j-th column of the Hessian

                W_el( l - listvs + 1 ) = 1.0_rp_

!  find the internal variables

                nin = INTVAR( iel + 1 ) - INTVAR( iel )
                CALL RANGE( iel, .FALSE., W_el, W_in, nvarel, nin,             &
                            ITYPEE( iel ), nvarel, nin )

!  multiply the internal variables by the element Hessian

                H_in( : nin ) = 0.0_rp_

!  only the upper triangle of the element Hessian is stored

                jcolst = ielh - 1
                DO jcol = 1, nin
                  ijhess = jcolst
                  jcolst = jcolst + jcol
                  wki = W_in( jcol ) * gdash
                  DO irow = 1, nin
                    IF ( irow <= jcol ) THEN
                      ijhess = ijhess + 1
                    ELSE
                      ijhess = ijhess + irow - 1
                    END IF
                    H_in( irow ) = H_in( irow ) + wki * HUVALS( ijhess )
                  END DO
                END DO

!  scatter the product back onto the elemental variables

                CALL RANGE( iel, .TRUE., H_in, H_el, nvarel, nin,              &
                            ITYPEE( iel ), nin, nvarel )
                W_el( l - listvs + 1 ) = 0.0_rp_
              END IF

!  find the entry in row i of this column

              DO k = listvs, l
                i = IELVAR( k )

!  skip all elements which lie outside a band of width nsemib; only the upper
!  triangle of the matrix is stored

                IF ( use_band .AND. i /= 0 ) maxsbw = MAX( maxsbw, ABS( j - i ))
                IF ( ABS( i - j ) <= nsemib .AND. i /= 0 ) THEN
                  IF ( i <= j ) THEN
                    ii = i
                    jj = j
                  ELSE
                    ii = j
                    jj = i
                  END IF

!  obtain the appropriate storage location in H for the new entry

                  IF ( INTREP( iel ) ) THEN
                    hesnew = scalee * H_el( k - listvs + 1 )
                  ELSE
                    hesnew = scalee * HUVALS( ihnext ) * gdash
                  END IF
                  IF ( iprint >= 100 ) WRITE( 6, "( ' Row ', I6, ' Column ',   &
                 &   I6, ' used from element ', I6, ' value = ', ES24.16 )" )  &
                    ii, jj, iel, hesnew

!  Case 1: band matrix storage scheme

                  IF ( use_band ) THEN

!  The entry belongs on the diagonal

                    IF ( ii == jj ) THEN
                      DIAG( ii ) = DIAG( ii ) + hesnew
                      IF ( k /= l ) DIAG( ii ) = DIAG( ii ) + hesnew

!  the entry belongs off the diagonal

                    ELSE
                      OFFDIA( jj - ii, ii ) = OFFDIA( jj - ii, ii ) + hesnew
                    END IF

!  Case 2: co-ordinate storage scheme

                  ELSE

!  there is an entry in position (i,j) to be stored in
!  H_row/col(COL(ROW_start(i)))

                    kk = POS_in_H( ROW_start( ii ) )
                    H_row( kk ) = ii
                    H_col( kk ) = jj
                    H_val( kk ) = H_val( kk ) + hesnew
                    IF ( k /= l .AND. ii == jj )                               &
                      H_val( kk ) = H_val( kk ) + hesnew
                    ROW_start( ii ) = ROW_start( ii ) + 1
                  END IF
                END IF
                ihnext = ihnext + 1
              END DO
            END IF
          END DO
        END DO
      END DO

!  restore the starting addresses

      IF ( .NOT. use_band ) THEN
        DO i = n - 1, 1, - 1
          ROW_start( i + 1 ) = ROW_start( i )
        END DO
        ROW_start( 1 ) = 1
      END IF

!  ---------------------------------------
!  For debugging, print the nonzero values
!  ---------------------------------------

!     IF ( .TRUE. ) THEN
      IF ( iprint >= 10 ) THEN
        IF ( .NOT. use_band )                                                  &
          WRITE( out,                                                          &
           "( '    Row  Column    Value        Row  Column    Value ', /       &
         &    '    ---  ------    -----        ---  ------    ----- ', /       &
         &    ( 2I6, ES24.16, 2I6, ES24.16 ) )" )                              &
            ( H_row( i ), H_col( i ), H_val( i ), i = 1, nnzh )

!  for debugging, print the nonzero pattern of the matrix

        IF ( n <= 36 ) THEN
          MATRIX( : n, : n ) = '  '
          IF ( use_band ) THEN
            DO i = 1, n
              IF ( DIAG( i ) /= 0.0_rp_ ) MATRIX( i, i ) = ' *'
              DO j = 1, MIN( nsemib, n - i )
                IF ( OFFDIA( j, i ) /= 0.0_rp_ ) THEN
                   MATRIX( i + j, i ) = ' *'
                   MATRIX( i, i + j ) = ' *'
                END IF
              END DO
            END DO
          ELSE
            DO i = 1, nnzh
              IF ( H_row( i ) > n ) THEN
                WRITE( out,                                                    &
                  "( ' Entry out of bounds in CUTEST_assemble_hessian',        &
                 &   ' row number = ', I0 )" ) H_row( i )
!               STOP
              END IF
              IF ( H_col( i ) > n ) THEN
                WRITE( out,                                                    &
                  "( ' Entry out of bounds in CUTEST_assemble_hessian',        &
                 &   ' col number = ', I0 )" ) H_col( i )
!               STOP
              END IF
              MATRIX( H_row( i ), H_col( i ) ) = ' *'
              MATRIX( H_col( i ), H_row( i ) ) = ' *'
            END DO
          END IF
          WRITE( out, "( /, 5X, 36I2 )" ) ( i, i = 1, n )
          DO i = 1, n
            WRITE( out, "( I3, 2X, 36A2 )" ) i, ( MATRIX( i, j ), j = 1, n )
          END DO
        END IF
      END IF

!  successful return

      status = 0
      RETURN

!  unsuccessful returns

  980 CONTINUE
      WRITE( error, "( ' ** Message from -CUTEST_assemble_hessian-',           &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  end of subroutine CUTEST_assemble_hessian

     END SUBROUTINE CUTEST_assemble_hessian

!-  C U T E S T _ a s s e m b l e _ h e s s i a n  _ p a t t e r n  SUBROUTINE -

      SUBROUTINE CUTEST_assemble_hessian_pattern(                              &
                      n, ng, nel, ntotel, nvrels, nvargp,                      &
                      IELVAR, IELING, ISTADG, ISTAEV, ISTAGV, ISVGRP, GXEQX,   &
                      iprint, error, out, status, alloc_status, bad_alloc,     &
                      hessian_setup_complete,                                  &
                      lh_row, lh_col, H_row, H_col, ROW_start, POS_in_H,       &
                      USED, FILLED, lrowst, lpos, lused, lfilled, nnzh )

!  Determine the sparisity pattern of the second derivative matrix of a groups
!  partially separable function in co-ordinate format

!  History -
!   fortran 2003 version released in CUTEst, 14th June 2013

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, nel, ng, nvargp
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: nvrels, ntotel
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: iprint, error, out
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, alloc_status, nnzh
      LOGICAL, INTENT( INOUT ) :: hessian_setup_complete
      CHARACTER ( LEN = 24 ) :: bad_alloc
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ng  + 1 ) :: ISTADG
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ng  + 1 ) :: ISTAGV
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel + 1 ) :: ISTAEV
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nvrels ) :: IELVAR
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ntotel ) :: IELING
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nvargp ) :: ISVGRP
      LOGICAL, INTENT( IN ), DIMENSION( ng ) :: GXEQX

!---------------------------------------------------------------
!   D u m m y   A r g u m e n t s   f o r   W o r k s p a c e
!--------------------------------------------------------------

      INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: lh_row, lh_col
      INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: lrowst, lpos, lused, lfilled
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: H_row
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: H_col
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ROW_start
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: POS_in_H
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: USED
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: FILLED

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------

      INTEGER ( KIND = ip_ ) :: i, ii, j, jj, k, kk, ig, l, iel
      INTEGER ( KIND = ip_ ) :: iell, listvs, listve, n_filled
      CHARACTER ( LEN = 2 ), DIMENSION( 36, 36 ) :: MATRIX
!     CHARACTER ( LEN = 80 ) :: array

!  determine the rows structure of the second derivative matrix of a
!  groups partially separable function with possible repititions

      IF ( .NOT. hessian_setup_complete ) THEN
        CALL CUTEST_sparse_hessian_by_rows(                                    &
                        n, ng, nel, ntotel, nvrels, nvargp,                    &
                        IELVAR, IELING, ISTADG, ISTAEV, ISTAGV, ISVGRP, GXEQX, &
                        error, status, alloc_status, bad_alloc,                &
                        ROW_start, POS_in_H, lrowst, lpos )
        IF ( status /= 0 ) RETURN

!  allocate workspace if required

        lused = n
        CALL CUTEST_allocate_array( USED, lused, alloc_status )
        IF ( alloc_status /= 0 ) THEN
          bad_alloc = 'USED' ; GO TO 980 ; END IF

        lfilled = n
        CALL CUTEST_allocate_array( FILLED, lfilled, alloc_status )
        IF ( alloc_status /= 0 ) THEN
          bad_alloc = 'FILLED' ; GO TO 980 ; END IF

!  now pass through the nonzeros, setting up the position in the future
!  H_row and H_col arrays of the data gathered from the groups

        USED = 0
        k = 1
        DO i = 1, n
          n_filled = 0
          DO l = ROW_start( i ), ROW_start( i + 1 ) - 1
            j = POS_in_H( l )
            IF ( USED( j ) == 0 ) THEN
              n_filled = n_filled + 1
              FILLED( n_filled ) = j
              USED( j ) = k
              POS_in_H( l ) = k
              k = k + 1
            ELSE
              POS_in_H( l ) = USED( j )
            END IF
          END DO
          USED( FILLED( 1 : n_filled ) ) = 0
        END DO
        nnzh = k - 1

!  allocate space for the row and column indices

        lh_row = nnzh
        CALL CUTEST_allocate_array( H_row, lh_row, alloc_status )
        IF ( alloc_status /= 0 ) THEN
          bad_alloc = 'H_row' ; GO TO 980 ; END IF

        lh_col = nnzh
        CALL CUTEST_allocate_array( H_col, lh_col, alloc_status )
        IF ( alloc_status /= 0 ) THEN
          bad_alloc = 'H_col' ; GO TO 980 ; END IF
      END IF

!  ----------------------------
!  set the row and column lists
!  ----------------------------

!  consider the rank-one second order term for the i-th group

      DO ig = 1, ng
        IF ( GXEQX( ig ) ) CYCLE
        listvs = ISTAGV( ig )
        listve = ISTAGV( ig + 1 ) - 1

!  Form the j-th column of the rank-one matrix

        DO l = listvs, listve
          j = ISVGRP( l )
          IF ( j == 0 ) CYCLE

!  find the entry in row i of this column

          DO k = listvs, listve
            i = ISVGRP( k )
            IF ( i == 0 .OR. i > j ) CYCLE

!  there is an entry in position (i,j) to be stored in
!  H_row/col(COL(ROW_start(i)))

            kk = POS_in_H( ROW_start( i ) )
            H_row( kk ) = i
            H_col( kk ) = j
            ROW_start( i ) = ROW_start( i ) + 1
          END DO
        END DO
      END DO

!  now consider the low rank first order terms for the i-th group

      DO ig = 1, ng

!  see if the group has any nonlinear elements

        DO iell = ISTADG( ig ), ISTADG( ig + 1 ) - 1
          iel = IELING( iell )
          listvs = ISTAEV( iel )
          listve = ISTAEV( iel + 1 ) - 1
          DO l = listvs, listve
            j = IELVAR( l )

!  find the entry in row i of this column

            IF ( j /= 0 ) THEN
              DO k = listvs, l
                i = IELVAR( k )

!  only the upper triangle of the matrix is stored

                IF ( i /= 0 ) THEN
                  IF ( i <= j ) THEN
                    ii = i
                    jj = j
                  ELSE
                    ii = j
                    jj = i
                  END IF

!  there is an entry in position (i,j) to be stored in
!  H_row/col(COL(ROW_start(i)))

                  kk = POS_in_H( ROW_start( ii ) )
                  H_row( kk ) = ii
                  H_col( kk ) = jj
                  ROW_start( ii ) = ROW_start( ii ) + 1
                END IF
              END DO
            END IF
          END DO
        END DO
      END DO

!  restore the starting addresses

      DO i = n - 1, 1, - 1
        ROW_start( i + 1 ) = ROW_start( i )
      END DO
      ROW_start( 1 ) = 1

!  ---------------------------------------
!  For debugging, print the nonzero values
!  ---------------------------------------

      IF ( iprint >= 10 ) THEN
        WRITE( out,                                                            &
           "( '    Row  Column   Row  Column   Row  Column   Row  Column', /,  &
         &    '    ---  ------   ---  ------   ---  ------   ---  ------', /,  &
         &    ( 2I6, 2I6, 2I6, 2I6 ) )" )                                      &
            ( H_row( i ), H_col( i ), i = 1, nnzh )

!  for debugging, print the nonzero pattern of the matrix

        IF ( n <= 36 ) THEN
          MATRIX( : n, : n ) = '  '
          DO i = 1, nnzh
            IF ( H_row( i ) > n ) THEN
              WRITE( out,                                                      &
                "( ' Entry out of bounds in CUTEST_assemble_hessian',          &
               &   ' row number = ', I0 )" ) H_row( i )
            END IF
            IF ( H_col( i ) > n ) THEN
              WRITE( out,                                                      &
                "( ' Entry out of bounds in CUTEST_assemble_hessian',          &
               &   ' col number = ', I0 )" ) H_col( i )
            END IF
            MATRIX( H_row( i ), H_col( i ) ) = ' *'
            MATRIX( H_col( i ), H_row( i ) ) = ' *'
          END DO
          WRITE( out, "( /, 5X, 36I2 )" ) ( i, i = 1, n )
          DO i = 1, n
            WRITE( out, "( I3, 2X, 36A2 )" ) i, ( MATRIX( i, j ), j = 1, n )
          END DO
        END IF
      END IF

!  successful return

      status = 0
      RETURN

!  unsuccessful returns

  980 CONTINUE
      WRITE( error, "( ' ** Message from -CUTEST_assemble_hessian-',           &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  end of subroutine CUTEST_assemble_hessian_pattern

     END SUBROUTINE CUTEST_assemble_hessian_pattern

!-  C U T E S T _ s i z e _ s p a r s e  _ h e s s i a n  S U B R O U T I N E -

      SUBROUTINE CUTEST_size_sparse_hessian(                                   &
                      n, ng, nel, ntotel, nvrels, nvargp, IELVAR, IELING,      &
                      ISTADG, ISTAEV, ISTAGV, ISVGRP, GXEQX, error, status,    &
                      alloc_status, bad_alloc, hessian_setup_complete,         &
                      ROW_start, POS_in_H, USED, lrowst, lpos, lused, nnzh )

!  Determine the number of nonzeros in the second derivative matrix of a
!  groups partially separable function in co-ordinate format

!  History -
!   Derived from CUTEst_assemble_hessian in this module
!   fortran 2003 version released in CUTEst, 14th June 2013

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, ng, nvargp
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: nvrels, ntotel, nel, error
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, nnzh, alloc_status
      LOGICAL, INTENT( IN ) :: hessian_setup_complete
      CHARACTER ( LEN = 24 ) :: bad_alloc
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ng  + 1 ) :: ISTADG
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ng  + 1 ) :: ISTAGV
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel + 1 ) :: ISTAEV
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nvrels ) :: IELVAR
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ntotel ) :: IELING
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nvargp ) :: ISVGRP
      LOGICAL, INTENT( IN ), DIMENSION( ng  ) :: GXEQX

!---------------------------------------------------------------
!   D u m m y   A r g u m e n t s   f o r   W o r k s p a c e
!--------------------------------------------------------------

      INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: lrowst, lpos, lused
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ROW_start
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: POS_in_H
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: USED

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------

      INTEGER ( KIND = ip_ ) :: i, j, k, l

!  determine the rows structure of the second derivative matrix of a
!  groups partially separable function with possible repititions

      IF ( .NOT. hessian_setup_complete ) THEN
        CALL CUTEST_sparse_hessian_by_rows(                                    &
                        n, ng, nel, ntotel, nvrels, nvargp,                    &
                        IELVAR, IELING, ISTADG, ISTAEV, ISTAGV, ISVGRP, GXEQX, &
                        error, status, alloc_status, bad_alloc,                &
                        ROW_start, POS_in_H, lrowst, lpos )
        IF ( status /= 0 ) RETURN

!  allocate workspace if required

        lused = n
        CALL CUTEST_allocate_array( USED, lused, alloc_status )
        IF ( alloc_status /= 0 ) THEN
          bad_alloc = 'USED' ; GO TO 980 ; END IF
      END IF

!  now pass through the nonzeros, removing duplicates

      USED = 0
      k = 1
      DO i = 1, n
        DO l = ROW_start( i ), ROW_start( i + 1 ) - 1
          j = POS_in_H( l )
          IF ( USED( j ) /= i ) THEN
            USED( j ) = i
            k = k + 1
          END IF
        END DO
      END DO
      nnzh = k - 1

!  deallocate temporary storage

      DEALLOCATE( USED, STAT = alloc_status )

!  successful return

      status = 0
      RETURN

!  unsuccessful returns

  980 CONTINUE
      WRITE( error, "( ' ** Message from -CUTEST_size_sparse_hessian-',        &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  end of subroutine CUTEST_size_sparse_hessian

     END SUBROUTINE CUTEST_size_sparse_hessian

! -*- C U T E S T _ s p a r s e _ h e s s i a n _ b y _ r o w s  SUBROUTINE -*-

      SUBROUTINE CUTEST_sparse_hessian_by_rows(                                &
                      n, ng, nel, ntotel, nvrels, nvargp,                      &
                      IELVAR, IELING, ISTADG, ISTAEV, ISTAGV, ISVGRP, GXEQX,   &
                      error, status, alloc_status, bad_alloc,                  &
                      ROW_start, POS_in_H, lrowst, lpos )

!  Determine the rows structure of the second derivative matrix of a
!  groups partially separable function with possible repititions

!  History -
!   Derived from CUTEst_assemble_hessian in this module
!   fortran 2003 version released in CUTEst, 14th June 2013

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, ng, nvargp
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: nvrels, ntotel, nel, error
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, alloc_status
      CHARACTER ( LEN = 24 ) :: bad_alloc
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ng  + 1 ) :: ISTADG
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ng  + 1 ) :: ISTAGV
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel + 1 ) :: ISTAEV
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nvrels ) :: IELVAR
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ntotel ) :: IELING
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nvargp ) :: ISVGRP
      LOGICAL, INTENT( IN ), DIMENSION( ng  ) :: GXEQX

!---------------------------------------------------------------
!   D u m m y   A r g u m e n t s   f o r   W o r k s p a c e
!--------------------------------------------------------------

      INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: lrowst, lpos
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ROW_start
      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: POS_in_H

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------

      INTEGER ( KIND = ip_ ) :: i, ii, iel, iell, ig, j, jj, k, l
      INTEGER ( KIND = ip_ ) :: listvs, listve

!  allocate workspace

      lrowst = n + 1
      CALL CUTEST_allocate_array( ROW_start, lrowst, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ROW_start' ; GO TO 980 ; END IF

!  ======
!  PASS 1
!  ======

!  ROW_start(i+1) will hold the number of entries (with repeats) in row i

      ROW_start( 2 : n + 1 ) = 0

!  consider the rank-one second order term for the i-th group

      DO ig = 1, ng
        IF ( GXEQX( ig ) ) CYCLE
        listvs = ISTAGV( ig )
        listve = ISTAGV( ig + 1 ) - 1

!  Form the j-th column of the rank-one matrix

        DO l = listvs, listve
          j = ISVGRP( l )
          IF ( j == 0 ) CYCLE

!  find the entry in row i of this column

          DO k = listvs, listve
            i = ISVGRP( k )
            IF ( i == 0 .OR. i > j ) CYCLE
            ROW_start( i + 1 ) = ROW_start( i + 1 ) + 1

!  there is an entry in position (i,j)

          END DO
        END DO
      END DO

!  now consider the low rank first order terms for the i-th group

      DO ig = 1, ng

!  see if the group has any nonlinear elements

        DO iell = ISTADG( ig ), ISTADG( ig + 1 ) - 1
          iel = IELING( iell )
          listvs = ISTAEV( iel )
          listve = ISTAEV( iel + 1 ) - 1
          DO l = listvs, listve
            j = IELVAR( l )

!  find the entry in row i of this column

            IF ( j /= 0 ) THEN
              DO k = listvs, l
                i = IELVAR( k )

!  only the upper triangle of the matrix is stored

                IF ( i /= 0 ) THEN
                  IF ( i <= j ) THEN
                    ii = i
                    jj = j
                  ELSE
                    ii = j
                    jj = i
                  END IF

!  there is an entry in position (ii,jj)

                  ROW_start( ii + 1 ) = ROW_start( ii + 1 ) + 1
                END IF
              END DO
            END IF
          END DO
        END DO
      END DO

!  ROW_start(i) is changed to give the starting address for the list of
!  column entries (with repeats) in row i (and ROW_start(n+1) points one
!  beyond the end)

!  compute starting addesses

      ROW_start( 1 ) = 1
      DO i = 2, n + 1
        ROW_start( i ) = ROW_start( i ) +  ROW_start( i - 1 )
      END DO

!  ======
!  PASS 2
!  ======

!  set the lists of column entries in each row

!  allocate space for column indices

      lpos = ROW_start( n + 1 ) - 1
      CALL CUTEST_allocate_array( POS_in_H, lpos, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'ROW_start' ; GO TO 980 ; END IF

!  consider the rank-one second order term for the i-th group

      DO ig = 1, ng
        IF ( GXEQX( ig ) ) CYCLE
        listvs = ISTAGV( ig )
        listve = ISTAGV( ig + 1 ) - 1

!  Form the j-th column of the rank-one matrix

        DO l = listvs, listve
          j = ISVGRP( l )
          IF ( j == 0 ) CYCLE

!  find the entry in row i of this column

          DO k = listvs, listve
            i = ISVGRP( k )
            IF ( i == 0 .OR. i > j ) CYCLE
            POS_in_H( ROW_start( i ) ) = j
            ROW_start( i ) = ROW_start( i ) + 1

!  there is an entry in position (i,j)

          END DO
        END DO
      END DO

!  now consider the low rank first order terms for the i-th group

      DO ig = 1, ng

!  see if the group has any nonlinear elements

        DO iell = ISTADG( ig ), ISTADG( ig + 1 ) - 1
          iel = IELING( iell )
          listvs = ISTAEV( iel )
          listve = ISTAEV( iel + 1 ) - 1
          DO l = listvs, listve
            j = IELVAR( l )

!  find the entry in row i of this column

            IF ( j /= 0 ) THEN
              DO k = listvs, l
                i = IELVAR( k )

!  only the upper triangle of the matrix is stored

                IF ( i /= 0 ) THEN
                  IF ( i <= j ) THEN
                    ii = i
                    jj = j
                  ELSE
                    ii = j
                    jj = i
                  END IF

!  there is an entry in position (i,j)

                  POS_in_H( ROW_start( ii ) ) = jj
                  ROW_start( ii ) = ROW_start( ii ) + 1
                END IF
              END DO
            END IF
          END DO
        END DO
      END DO

!  restore the starting addresses

      DO i = n - 1, 1, - 1
        ROW_start( i + 1 ) = ROW_start( i )
      END DO
      ROW_start( 1 ) = 1

!  successful return

      status = 0
      RETURN

!  unsuccessful returns

  980 CONTINUE
      WRITE( error, "( ' ** Message from -CUTEST_sparse_hessian_by_rows-',     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  end of subroutine CUTEST_sparse_hessian_by_rows

     END SUBROUTINE CUTEST_sparse_hessian_by_rows

! - C U T E S T _ a s s e m b l e _ e l e m e n t _ h e s s i a n  SUBROUTINE  -

      SUBROUTINE CUTEST_assemble_element_hessian(                              &
                        ng, nel, ntotel, nvrels, nnza, maxsel, nvargp,         &
                        lnguvl, lnhuvl, ISTADH, ICNA, ISTADA, INTVAR, IELVAR,  &
                        IELING, ISTADG, ISTAEV, ISTAGV, ISVGRP, ITYPEE,        &
                        A, GUVALS, HUVALS, GVALS2, GVALS3, GSCALE, ESCALE,     &
                        GXEQX, INTREP, ISWKSP, GRAD_el, W_el, W_in, H_el,      &
                        H_in, RANGE, ne, lhe_ptr, lhe_row, lhe_val,            &
                        HE_row, HE_row_ptr, HE_val, HE_val_ptr, BYROWS,        &
                        iprint, out, error, buffer, alloc_status, bad_alloc,   &
                        status )

!  ------------------------------------------------------------------
!  assemble the second derivative matrix of a groups partially
!  separable function into finite-element format

!           ne
!      H = sum H_e,
!          e=1

!  where each element H_e involves a small subset of the rows of H.
!  H is stored as a list of the row indices involved in each element
!  and the upper triangle of H_e (stored by rows or columns)
!  ------------------------------------------------------------------

!  History -
!   fortran 77 version released in CUTEr as ASMBE, November 25th 1994
!   fortran 2003 version released in CUTEst, 26th November 2012

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: ng, nel, ntotel
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: nvrels, nnza, maxsel, iprint
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: nvargp, lnguvl, lnhuvl, lhe_ptr
      INTEGER ( KIND = ip_ ), INTENT( IN ) :: out, error, buffer
      INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: lhe_row, lhe_val
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: ne, status, alloc_status
      LOGICAL, INTENT( IN ) :: byrows
      CHARACTER ( LEN = 24 ), INTENT( OUT ) :: bad_alloc
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nnza ) :: ICNA
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ng  + 1 ) :: ISTADA
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ng  + 1 ) :: ISTADG
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ng  + 1 ) :: ISTAGV
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel + 1 ) :: INTVAR
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel + 1 ) :: ISTAEV
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel + 1 ) :: ISTADH
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nvrels ) :: IELVAR
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ntotel ) :: IELING
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nvargp ) :: ISVGRP
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel ) :: ITYPEE
      INTEGER ( KIND = ip_ ), DIMENSION( lhe_ptr ) :: HE_row_ptr
      INTEGER ( KIND = ip_ ), DIMENSION( lhe_ptr ) :: HE_val_ptr
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( nnza ) :: A
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( lnguvl ) :: GUVALS
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( lnhuvl ) :: HUVALS
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( ng ) :: GVALS2
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( ng ) :: GVALS3
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( ng ) :: GSCALE
      REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( ntotel ) :: ESCALE
      LOGICAL, INTENT( IN ), DIMENSION( ng  ) :: GXEQX
      LOGICAL, INTENT( IN ), DIMENSION( nel ) :: INTREP
      INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( : ) :: ISWKSP
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: GRAD_el
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: W_el
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: W_in
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: H_el
      REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: H_in
      EXTERNAL :: RANGE

!---------------------------------------------------------------
!   D u m m y   A r g u m e n t s   f o r   W o r k s p a c e
!--------------------------------------------------------------

      INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: HE_row
      REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: HE_val

!  local variables

      INTEGER ( KIND = ip_ ) :: i, ii, ig, iel, iell, ielh, ihi, ig1
      INTEGER ( KIND = ip_ ) :: j, jj, k, l, nin, nvarg, nsizee
      INTEGER ( KIND = ip_ ) :: nvarel, listvs, listve, ihnext
      INTEGER ( KIND = ip_ ) :: ijhess, irow, jcol, jcolst, nlh, ulh, mlh
      REAL ( KIND = rp_ ) :: wki, hesnew, gdash,  g2dash, scalee

!  ensure that there is sufficient space

      CALL CUTEST_allocate_array( HE_row, lhe_row, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'HE_row' ; status = 1 ; GO TO 980 ; END IF

      CALL CUTEST_allocate_array( HE_val, lhe_val, alloc_status )
      IF ( alloc_status /= 0 ) THEN
        bad_alloc = 'HE_val' ; status = 1 ; GO TO 980 ; END IF

! -------------------------------------------------------
!  form the rank-one second order term for the i-th group
! -------------------------------------------------------

      ne = 0
      HE_row_ptr( 1 ) = 1 ; HE_val_ptr( 1 ) = 1
      DO ig = 1, ng
        IF ( iprint >= 100 ) WRITE( out,                                       &
           "( ' Group ', I5, ' rank-one terms ' )" ) ig
        ig1 = ig + 1
        IF ( GXEQX( ig ) ) THEN
          g2dash = 0.0_rp_
        ELSE
          g2dash = GSCALE( ig ) * GVALS3( ig )
          IF ( iprint >= 100 ) WRITE( out, * ) ' GVALS3(ig) ', GVALS3( ig )
        END IF

!  ignore linear groups

        IF ( ISTADG( ig ) >= ISTADG( ig1 ) .AND. GXEQX( ig ) ) CYCLE

!  the group is nonlinear

        ne = ne + 1
        listvs = ISTAGV( ig )
        listve = ISTAGV( ig1 ) - 1
        nvarg = listve - listvs + 1

!  set the starting addresses for the integer and real arrays for group ig + 1

        HE_row_ptr( ne + 1 ) = HE_row_ptr( ne ) + nvarg
        IF ( HE_row_ptr( ne + 1 ) > lhe_row ) THEN
          nlh = 3 * HE_row_ptr( ne + 1 ) / 2 ; ulh = HE_row_ptr( ne ) - 1
          mlh = HE_row_ptr( ne + 1 )
          CALL CUTEST_extend_array( HE_row, lhe_row, ulh, nlh, mlh, buffer,    &
                                    status, alloc_status )
          IF ( status /= 0 ) THEN
             bad_alloc = 'HE_row' ; GO TO 980 ; END IF
          lhe_row = nlh
        END IF
        nsizee = ( nvarg * ( nvarg + 1 ) ) / 2
        HE_val_ptr( ne + 1 ) = HE_val_ptr( ne ) + nsizee
        IF ( HE_val_ptr( ne + 1 ) >= lhe_val ) THEN
          nlh = 3 * HE_val_ptr( ne + 1 ) / 2 ; ulh = HE_val_ptr( ne ) - 1
          mlh = HE_val_ptr( ne + 1 )
          CALL CUTEST_extend_array( HE_val, lhe_val, ulh, nlh, mlh, buffer,    &
                                    status, alloc_status )
          IF ( status /= 0 ) THEN
             bad_alloc = 'HE_val' ; GO TO 980 ; END IF
          lhe_val = nlh
        END IF

!  record the row indices involved in super-element ne

        k = HE_row_ptr( ne )
        DO l = listvs, listve
          HE_row( k ) = ISVGRP( l )
          k = k + 1
        END DO

!  skip if the group contributes nothing to the Hessian

        IF ( GXEQX( ig ) .OR. g2dash == 0.0_rp_ ) THEN
          HE_val( HE_val_ptr( ne ) : HE_val_ptr( ne ) + nsizee - 1 ) = 0.0_rp_
          CYCLE
        END IF

!  form the gradient of the ig-th group

         GRAD_el( ISVGRP( listvs : listve ) ) = 0.0_rp_

!  consider any nonlinear elements for the group

        DO iell = ISTADG( ig ), ISTADG( ig1 ) - 1
          iel = IELING( iell )
          k = INTVAR( iel ) ; l = ISTAEV( iel )
          nvarel = ISTAEV( iel + 1 ) - l
          scalee = ESCALE( iell )
          IF ( INTREP( iel ) ) THEN

!  the iel-th element has an internal representation

            nin = INTVAR( iel + 1 ) - k
            CALL RANGE( iel, .TRUE., GUVALS( k : k + nin - 1 ),                &
                        H_el, nvarel, nin, ITYPEE( iel ), nin, nvarel )
            DO i = 1, nvarel
              j = IELVAR( l )
              GRAD_el( j ) = GRAD_el( j ) + scalee * H_el( i )
              l = l + 1
            END DO
          ELSE

!  the iel-th element has no internal representation

            DO i = 1, nvarel
               j = IELVAR( l )
               GRAD_el( j ) = GRAD_el( j ) + scalee * GUVALS( k )
               k = k + 1 ; l = l + 1
            END DO
          END IF
        END DO

!  include the contribution from the linear element

        DO k = ISTADA( ig ), ISTADA( ig1 ) - 1
          j = ICNA( k )
          GRAD_el( j ) = GRAD_el( j ) + A( k )
        END DO

!  the gradient is complete. Form the j-th column of the rank-one matrix

        DO l = listvs, listve
          jj = ISVGRP( l )
          j = l - listvs + 1

!  find the entry in row i of this column.

          DO k = listvs, l
             ii = ISVGRP( k )
             i = k - listvs + 1
             IF ( byrows ) THEN
               ihi =  HE_val_ptr( ne ) - 1                                     &
                        + nvarg * ( i - 1 ) - ( ( i - 1 ) * i ) / 2 + j
             ELSE
               ihi = HE_val_ptr( ne ) - 1 + i + ( j * ( j - 1 ) ) / 2
             END IF
             HE_val( ihi ) = GRAD_el( ii ) * GRAD_el( jj ) * g2dash
             IF ( iprint >= 100 )  WRITE( out,                                 &
               "( ' Row ', I6, ' column ', I6, ' used. Value = ', ES24.16 )" ) &
                ii, jj, HE_val( ihi )
          END DO
        END DO
      END DO

!  reset the workspace array to zero

      W_el( : maxsel ) = 0.0_rp_

! ---------------------------------------------------------
!  add on the low rank first order terms for the I-th group
! ---------------------------------------------------------

      ne = 0
      DO ig = 1, ng
        ig1 = ig + 1

!  once again, ignore linear groups

        IF ( ISTADG( ig ) >= ISTADG( ig1 ) .AND. GXEQX( ig ) ) CYCLE

!  the group is nonlinear

        ne = ne + 1
        IF ( iprint >= 100 ) WRITE( out,                                       &
           "( ' Group ', I5, ' second-order terms ' )" ) ig
        IF ( GXEQX( ig ) ) THEN
          gdash = GSCALE( ig )
        ELSE
          gdash = GSCALE( ig ) * GVALS2( ig )
          IF ( iprint >= 100 ) WRITE( out, * ) ' GVALS2(ig) ', GVALS2(IG)
        END IF
        IF ( gdash == 0.0_rp_ ) THEN
          CYCLE
        END IF

!  map the problem variables to the elemental variables

        nvarg = HE_row_ptr( ne + 1 ) - HE_row_ptr( ne )
        DO i = HE_row_ptr( ne ), HE_row_ptr( ne + 1 ) - 1
          ISWKSP( HE_row( i ) ) = i + 1 - HE_row_ptr( ne )
        END DO

!  see if the group has any nonlinear elements

        DO iell = ISTADG( ig ), ISTADG( ig1 ) - 1
          iel = IELING( iell )
          listvs = ISTAEV( iel )
          listve = ISTAEV( iel + 1 ) - 1
          nvarel = listve - listvs + 1
          ielh = ISTADH( iel )
          ihnext = ielh
          scalee = ESCALE( iell )
          DO l = listvs, listve
            j = ISWKSP( IELVAR( l ) )

!  the iel-th element has an internal representation. Compute the j-th column
!  of the element Hessian matrix

            IF ( INTREP( iel ) ) THEN

!  compute the j-th column of the Hessian

              W_el( l - listvs + 1 ) = 1.0_rp_

!  find the internal variables

              nin = INTVAR( iel + 1 ) - INTVAR( iel )
              CALL RANGE( iel, .FALSE., W_el, W_in, nvarel, nin,               &
                          ITYPEE( iel ), nvarel, nin )

!  multiply the internal variables by the element Hessian

              H_in( : nin ) = 0.0_rp_

!  only the upper triangle of the element Hessian is stored

              jcolst = ielh - 1
              DO jcol = 1, nin
                ijhess = jcolst
                jcolst = jcolst + jcol
                wki = W_in( jcol ) * gdash
                DO irow = 1, nin
                  IF ( irow <= jcol ) THEN
                    ijhess = ijhess + 1
                  ELSE
                    ijhess = ijhess + irow - 1
                  END IF
                  H_in( irow ) = H_in( irow ) + wki * HUVALS( ijhess )
                END DO
              END DO

!  scatter the product back onto the elemental variables

              CALL RANGE( iel, .TRUE., H_in, H_el, nvarel, nin,                &
                          ITYPEE( iel ), nin, nvarel )
              W_el( l - listvs + 1 ) = 0.0_rp_

!  find the entry in row i of this column

            END IF
            DO k = listvs, l
              i = ISWKSP( IELVAR( k ) )

!  only the upper triangle of the matrix is stored

              IF ( i > j ) THEN
                ii = i ; i = j ; j = ii
              END IF

!  obtain the appropriate storage location in H for the new entry

              IF ( INTREP( iel ) ) THEN
                hesnew = scalee * H_el( k - listvs + 1 )
              ELSE
                hesnew = scalee * HUVALS( ihnext ) * gdash
              END IF
              IF ( iprint >= 100 ) WRITE( out, "( ' Row ', I6, ' Column ',     &
             & I6, ' used from element ', I6, ' value = ', ES24.16 )" )        &
                i, j, iel, hesnew
              IF ( byrows ) THEN
                ihi = HE_val_ptr( ne ) - 1 + nvarg * ( i - 1 ) -               &
                   ( ( i - 1 ) * i ) / 2 + j
              ELSE
                ihi = HE_val_ptr( ne ) - 1 + i + ( j * ( j - 1 ) ) / 2
              END IF
              HE_val( ihi ) = HE_val( ihi ) + hesnew
              IF ( k /= l .AND. i == j )                                       &
                HE_val( ihi ) = HE_val( ihi ) + hesnew
              ihnext = ihnext + 1
            END DO
          END DO
        END DO
      END DO
      ISWKSP = 0

! ----------------------------------------
!  for debugging, print the nonzero values
! ----------------------------------------

      IF ( iprint >= 10 ) THEN
        DO ig = 1, ne
          WRITE( out, "( ' Super-element ', I10 )" ) ig
          WRITE( out, "( ' Super-element variables     ', 8I7, /, ( 11I7 ) )") &
             ( HE_row( i ), i = HE_row_ptr( ig ), HE_row_ptr( ig + 1 ) - 1 )
          WRITE( out, "( ' Nonzeros   ', 6ES12.4, /, ( 7ES12.4 ) )" )          &
              ( HE_val( i ), i = HE_val_ptr( ig ), HE_val_ptr( ig + 1 ) - 1 )
        END DO
      END IF
      status = 0
      RETURN

!  unsuccessful returns

! 610 CONTINUE
!     status = 1
!     RETURN

!  unsuccessful returns

  980 CONTINUE
      WRITE( error, "( ' ** Message from -CUTEST_assemble_element_hessian-',   &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
        alloc_status, bad_alloc
      RETURN

!  end of subroutine CUTEST_assemble_element_hessian

      END SUBROUTINE CUTEST_assemble_element_hessian

! -*-*- C U T E S T _ s i z e _ e l e m e n t _ h e s s i a n  SUBROUTINE  -*-*-

      SUBROUTINE CUTEST_size_element_hessian( ng, ISTADG, ISTAGV, GXEQX, ne,   &
                                              he_val_ne, he_row_ne, status )

!  --------------------------------------------------------------------------
!  compute the number of elements and the space required to store the Hessian
!  matrix of a problem initially written in Standard Input Format (SIF)

!  The matrix is represented in "finite element format", i.e.,

!           ne
!      H = sum H_e,
!          e=1

!  where each element H_i involves a small subset of the rows of H.
!  H is stored as a list of the row indices involved in each element
!  and the upper triangle of H_e (stored by rows or columns).

!  ne (integer) number of elements
!  he_val_ne (integer) number of entries needed to store the real values of H.
!         Specifically, the sum of the number of entries in the upper triangle
!         of each H_e
!  he_row_ne (integer) number of entries needed to store the integer entries of
!         H. Specifically, the sum of the row dimensions of each H_e
!  ---------------------------------------------------------------------------

!  History -
!   fortran 77 version released in CUTEr as U/CDIMSE, November 25th 1994
!   fortran 2003 version released in CUTEst, 26th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

      INTEGER ( KIND = ip_ ), INTENT( IN ) :: ng
      INTEGER ( KIND = ip_ ), INTENT( OUT ) :: ne, he_val_ne, he_row_ne, status
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ng + 1 ) :: ISTADG
      INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ng + 1 ) :: ISTAGV
      LOGICAL, INTENT( IN ), DIMENSION( ng  ) :: GXEQX

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------

!  local variables

      INTEGER ( KIND = ip_ ) :: ig, nvarg, ig1

!  initilaize counts

      ne = 0 ;  he_val_ne = 0 ; he_row_ne = 0

!  loop over the groups

      DO ig = 1, ng
        ig1 = ig + 1

!  only consider nonlinear groups

        IF ( ISTADG( ig ) < ISTADG( ig1 ) .OR. .NOT. GXEQX( ig ) ) THEN
          ne = ne + 1
          nvarg = ISTAGV( ig1 ) - ISTAGV( ig )
          he_row_ne = he_row_ne + nvarg
          he_val_ne = he_val_ne + ( nvarg * ( nvarg + 1 ) ) / 2
        END IF
      END DO
      status = 0

      RETURN

!  end of subroutine CUTEST_size_element_hessian

      END SUBROUTINE CUTEST_size_element_hessian

!-  C U T E S T _ h e s s i a n _ t i m e s _ v e c t o r  S U B R O U T I N E -

     SUBROUTINE CUTEST_hessian_times_vector(                                   &
                      n, ng, nel, ntotel, nvrels, nvargp, alllin,              &
                      ISTAEV, ISTADH, INTVAR, IELING, IELVAR, P, Q, GVALS2,    &
                      GVALS3, GRJAC, GSCALE, ESCALE, HUVALS, lhuval, GXEQX,    &
                      INTREP, IGCOLJ, ISLGRP, ITYPEE, ISYMMH, ISTAJC, AP,      &
                      W_el, W_in, H_in, RANGE )

!  ----------------------------------------------------------------------
!  evaluate Q, the product of the hessian of a groups partially separable
!  function with the vector P
!  ----------------------------------------------------------------------

!  History -
!   fortran 77 version originally released in CUTE, September 23rd, 1991
!   fortran 90 version originally released pre GALAHAD Version 1.0. Febrauary
!     1st 1995 as HSPRD_hessian_times_vector as part of the HSPRD module
!   update released with GALAHAD Version 2.0. February 16th 2005 with
!     sparse products removed
!   fortran 2003 version released in CUTEst, 5th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER ( KIND = ip_ ), INTENT( IN ) :: n, ng, nel, ntotel
     INTEGER ( KIND = ip_ ), INTENT( IN ) :: nvrels, nvargp, lhuval
     LOGICAL, INTENT( IN ) :: alllin
     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel + 1 ) :: ISTAEV
     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel + 1 ) :: ISTADH
     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel + 1 ) :: INTVAR
     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( ntotel ) :: IELING
     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nvrels ) :: IELVAR
     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( nel ) :: ITYPEE
     REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( n ) :: P
     REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( ng ) :: GVALS2
     REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( ng ) :: GVALS3
     REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( ng ) :: GSCALE
     REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( nvargp ) :: GRJAC
     REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( ntotel ) :: ESCALE
     REAL ( KIND = rp_ ), INTENT( IN ), DIMENSION( lhuval ) :: HUVALS
     REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: Q
     LOGICAL, INTENT( IN ), DIMENSION( ng ) :: GXEQX
     LOGICAL, INTENT( IN ), DIMENSION( nel ) :: INTREP
     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( : ) :: IGCOLJ
     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( : ) :: ISLGRP
     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( : ) :: ISTAJC
     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( : , : ) :: ISYMMH
     REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: AP
     REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: W_el
     REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: W_in
     REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: H_in

!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------

     INTERFACE
       SUBROUTINE RANGE( ielemn, transp, W1, W2, nelvar, ninvar, ieltyp,       &
                         lw1, lw2 )
        USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: ielemn, nelvar, ninvar
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: ieltyp, lw1, lw2
       LOGICAL, INTENT( IN ) :: transp
       REAL ( KIND = rp_ ), INTENT( IN  ), DIMENSION ( lw1 ) :: W1
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION ( lw2 ) :: W2
       END SUBROUTINE RANGE
     END INTERFACE

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------

     INTEGER ( KIND = ip_ ) :: i, iel, ig, ii, irow, jcol, ijhess, lthvar
     INTEGER ( KIND = ip_ ) :: iell, nin, k, l, ll, nvarel, ielhst
     REAL ( KIND = rp_ ) :: pi, gi
     LOGICAL :: nullwk

!  =========================== rank-one terms ============================

!  if the ig-th group is non-trivial, form the product of P with the sum of
!  rank-one first order terms, A(trans) * GVALS3 * A. A is stored by both
!  rows and columns

!  initialize AP and Q as zero

     AP( : ng ) = 0.0_rp_ ; Q = 0.0_rp_

!  form the matrix-vector product AP = A * P, using the column-wise
!  storage of A

     DO i = 1, n
       pi = P( i )
!DIR$ IVDEP
       DO k = ISTAJC( i ), ISTAJC( i + 1 ) - 1
         AP( IGCOLJ( k ) ) = AP( IGCOLJ( k ) ) + pi * GRJAC( k )
       END DO
     END DO

!  multiply W by the diagonal matrix GVALS3

     WHERE ( GXEQX( : ng ) )
       AP( : ng ) = AP( : ng ) * GSCALE( : ng )
     ELSEWHERE
       AP( : ng ) = AP( : ng ) * GSCALE( : ng ) * GVALS3( : ng )
     END WHERE

!  form the matrix-vector product Q = A(trans) * W, once again using the
!  column-wise storage of A

     DO i = 1, n
!      Q( i ) =                                                              &
!        DOT_PRODUCT( AP( IGCOLJ( ISTAJC( i ) : ISTAJC( i + 1 ) - 1 ) ),     &
!                     GRJAC ( ISTAJC( i ) : ISTAJC( i + 1 ) - 1 ) )
       pi = 0.0_rp_
       DO ii = ISTAJC( i ), ISTAJC( i + 1 ) - 1
         pi = pi + AP( IGCOLJ( ii ) ) * GRJAC( ii )
       END DO
       Q( i ) = pi
     END DO

     IF ( .NOT. alllin ) THEN

!  ======================= second-order terms =======================

!  now consider the product of P with the second order terms (that is, the
!  2nd derivatives of the elements)

       DO iell = 1, ntotel
         ig = ISLGRP( iell )
         iel = IELING( iell )
         nvarel = ISTAEV( iel + 1 ) - ISTAEV( iel )
         IF ( GXEQX( ig ) ) THEN
           gi = GSCALE( ig ) * ESCALE( iell )
         ELSE
           gi = GSCALE( ig ) * ESCALE( iell ) * GVALS2( ig )
         END IF
         IF ( INTREP( iel ) ) THEN

!  the iel-th element Hessian has an internal representation. Copy the
!  elemental variables into W

           nullwk = .TRUE.
           ll = ISTAEV( iel )
!DIR$ IVDEP
           DO ii = 1, nvarel
             pi = P( IELVAR( ll ) )
             W_el( ii ) = pi
             IF ( pi /= 0.0_rp_ ) nullwk = .FALSE.
             ll = ll + 1
           END DO
           IF ( nullwk ) CYCLE

!  find the internal variables, W_in

           nin = INTVAR( iel + 1 ) - INTVAR( iel )
           CALL RANGE( iel, .FALSE., W_el, W_in, nvarel, nin,                  &
                       ITYPEE( iel ), nvarel, nin )

!  multiply the internal variables by the element Hessian and put the
!  product in H_in. Consider the first column of the element Hessian

           ielhst = ISTADH( iel )
           pi = gi * W_in( 1 )
           H_in( : nin ) = pi * HUVALS( ISYMMH( 1, : nin ) + ielhst )

!  now consider the remaining columns of the element Hessian

           DO jcol = 2, nin
             pi = gi * W_in( jcol )
             IF ( pi /= 0.0_rp_ ) THEN
               H_in( : nin ) = H_in( : nin ) +                                 &
                 pi * HUVALS( ISYMMH( jcol, : nin ) + ielhst )
             END IF
           END DO

!  scatter the product back onto the elemental variables, W

           CALL RANGE( iel, .TRUE., H_in, W_el, nvarel, nin,                   &
                       ITYPEE( iel ), nin, nvarel )

!  add the scattered product to Q

           ll = ISTAEV( iel )
!DIR$ IVDEP
           DO ii = 1, nvarel
              l = IELVAR( ll )
              Q( l ) = Q( l ) + W_el( ii )
              ll = ll + 1
           END DO
         ELSE

!  the iel-th element Hessian has no internal representation

           lthvar = ISTAEV( iel ) - 1
           ielhst = ISTADH( iel )
           DO jcol = 1, nvarel
             pi = gi * P( IELVAR( lthvar + jcol ) )
             IF ( pi /= 0.0_rp_ ) THEN
!DIR$ IVDEP
               DO irow = 1, nvarel
                 ijhess = ISYMMH( jcol, irow ) + ielhst
                 l = IELVAR( lthvar + irow )
                 Q( l ) = Q( l ) + pi * HUVALS( ijhess )
               END DO
             END IF
           END DO
         END IF
       END DO
     END IF

!  ==================== the product is complete =======================

     RETURN

!  end of subroutine CUTEST_hessian_times_vector

     END SUBROUTINE CUTEST_hessian_times_vector

!-  C U T E S T _ h e s s i a n _ t i m e s _ s p _ v e c t o r  SUBROUTINE -

     SUBROUTINE CUTEST_hessian_times_sp_vector(                                &
                      n, ng, nel, ntotel, nvrels, nvargp, nvar1, nvar2,        &
                      nnonnz, nbprod, alllin, IVAR, ISTAEV, ISTADH,            &
                      INTVAR, IELING, IELVAR, ISWKSP, INONNZ, P, Q, GVALS2,    &
                      GVALS3, GRJAC, GSCALE, ESCALE, HUVALS, lhuval, GXEQX,    &
                      INTREP, IGCOLJ, ISLGRP, ISVGRP, ISTAGV, IVALJR,          &
                      ITYPEE, ISYMMH, ISTAJC, IUSED, LIST_elements,            &
                      LINK_elem_uses_var, NZ_components_w,                     &
                      AP, W_el, W_in, H_in, RANGE )

!  ----------------------------------------------------------------------
!  evaluate Q, the product of the hessian of a groups partially separable
!  function with the sparse vector P. The nonzero components of P have
!  indices IVAR( i ), i = nvar1, ..., nvar2. The nonzero components of
!  the product Q have indices INNONZ( i ), i = 1, ..., nnonnz. The
!  components of ISWKSP must be less than nbprod on entry; on exit they
!  will be no larger than nbprod
!  ----------------------------------------------------------------------

!  History -
!   fortran 77 version originally released in CUTE, September 23rd, 1991
!   fortran 90 version originally released pre GALAHAD Version 1.0. Febrauary
!     1st 1995 as HSPRD_hessian_times_vector as part of the HSPRD module
!   update released with GALAHAD Version 2.0. February 16th 2005 with
!     dense products removed
!   fortran 2003 version released in CUTEst, 295th August 2014

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER ( KIND = ip_ ), INTENT( IN    ) :: n, ng, nel, ntotel
     INTEGER ( KIND = ip_ ), INTENT( IN    ) :: nvrels, nvar1, nvar2
     INTEGER ( KIND = ip_ ), INTENT( IN    ) :: nbprod, nvargp, lhuval
     INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: nnonnz
     LOGICAL, INTENT( IN    ) :: alllin
     INTEGER ( KIND = ip_ ), INTENT( IN    ), DIMENSION( * ) :: IVAR
     INTEGER ( KIND = ip_ ), INTENT( IN    ), DIMENSION( nel + 1 ) :: ISTAEV
     INTEGER ( KIND = ip_ ), INTENT( IN    ), DIMENSION( nel + 1 ) :: ISTADH
     INTEGER ( KIND = ip_ ), INTENT( IN    ), DIMENSION( nel + 1 ) :: INTVAR
     INTEGER ( KIND = ip_ ), INTENT( IN    ), DIMENSION( ntotel  ) :: IELING
     INTEGER ( KIND = ip_ ), INTENT( IN    ), DIMENSION( nvrels  ) :: IELVAR
     INTEGER ( KIND = ip_ ), INTENT( IN    ), DIMENSION( nel     ) :: ITYPEE
     INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( ntotel ) :: ISWKSP
     INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( n ) :: INONNZ
     REAL ( KIND = rp_ ), INTENT( IN  ), DIMENSION( n ) :: P
     REAL ( KIND = rp_ ), INTENT( IN  ), DIMENSION( ng ) :: GVALS2
     REAL ( KIND = rp_ ), INTENT( IN  ), DIMENSION( ng ) :: GVALS3
     REAL ( KIND = rp_ ), INTENT( IN  ), DIMENSION( ng ) :: GSCALE
     REAL ( KIND = rp_ ), INTENT( IN  ), DIMENSION( nvargp ) :: GRJAC
     REAL ( KIND = rp_ ), INTENT( IN  ), DIMENSION( ntotel ) :: ESCALE
     REAL ( KIND = rp_ ), INTENT( IN  ), DIMENSION( lhuval ) :: HUVALS
     REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( n ) :: Q
     LOGICAL, INTENT( IN ), DIMENSION( ng ) :: GXEQX
     LOGICAL, INTENT( IN ), DIMENSION( nel ) :: INTREP
     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( : ) :: IGCOLJ
     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( : ) :: ISLGRP
     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( : ) :: ISVGRP
     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( : ) :: ISTAGV
     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( : ) :: IVALJR
     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( : ) :: ISTAJC
     INTEGER ( KIND = ip_ ), INTENT( INOUT ), DIMENSION( : ) :: IUSED
     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( : ) :: LIST_elements
     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( : , : ) :: ISYMMH

     INTEGER ( KIND = ip_ ), INTENT( IN ), DIMENSION( : ) :: LINK_elem_uses_var
     INTEGER ( KIND = ip_ ), INTENT( OUT ), DIMENSION( : ) :: NZ_components_w
     REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: AP
     REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: W_el
     REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: W_in
     REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION( : ) :: H_in

!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------

     INTERFACE
       SUBROUTINE RANGE( ielemn, transp, W1, W2, nelvar, ninvar, ieltyp,       &
                         lw1, lw2 )
       USE CUTEST_KINDS_precision
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: ielemn, nelvar, ninvar
       INTEGER ( KIND = ip_ ), INTENT( IN ) :: ieltyp, lw1, lw2
       LOGICAL, INTENT( IN ) :: transp
       REAL ( KIND = rp_ ), INTENT( IN  ), DIMENSION ( lw1 ) :: W1
       REAL ( KIND = rp_ ), INTENT( OUT ), DIMENSION ( lw2 ) :: W2
       END SUBROUTINE RANGE
     END INTERFACE

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------

     INTEGER ( KIND = ip_ ) :: i, iel, ig, ii, ipt, j, nnz_components_w
     INTEGER ( KIND = ip_ ) :: irow, jcol, ijhess, lthvar
     INTEGER ( KIND = ip_ ) :: iell  , nin, k, l , ll, nvarel, ielhst
     REAL ( KIND = rp_ ) :: pi, gi, smallest

     smallest = TINY( 1.0_rp_ )

!  ======================= rank-one terms ==========================

!  If the IG-th group is non-trivial, form the product of P with the
!  sum of rank-one first order terms, A(trans) * GVALS3 * A. A is
!  stored by both rows and columns.

     nnz_components_w = 0

!  Form the matrix-vector product W = A * P, using the column-wise
!  storage of A. Keep track of the nonzero components of W in NZ_components_w.
!  Only store components corresponding to non trivial groups

     DO j = nvar1, nvar2
       i = IVAR( j )
       pi = P( i )
!DIR$ IVDEP
       DO k = ISTAJC( i ), ISTAJC( i + 1 ) - 1
         ig = IGCOLJ( k )
         IF ( IUSED( ig ) == 0 ) THEN
           AP( ig ) = pi * GRJAC( k )
           IUSED( ig ) = 1
           nnz_components_w = nnz_components_w + 1
           NZ_components_w( nnz_components_w ) = ig
         ELSE
           AP( ig ) = AP( ig ) + pi * GRJAC( k )
         END IF
       END DO
     END DO

!  Reset IUSED to zero

     IUSED( NZ_components_w( : nnz_components_w ) ) = 0

!  Form the matrix-vector product Q = A( TRANS ) * W, using the row-wise
!  storage of A

     nnonnz = 0

     DO j = 1, nnz_components_w
       ig = NZ_components_w( j )
       IF ( .NOT. GXEQX( ig ) ) THEN

!  If group ig is non trivial, there are contributions from its rank-one term

         pi = GSCALE( ig ) * GVALS3( ig ) * AP( ig )
!DIR$ IVDEP
         DO k = ISTAGV( ig ), ISTAGV( ig + 1 ) - 1
           l = ISVGRP( k )

!  If Q has a nonzero in position L, store its index in INONNZ

           IF ( IUSED( l ) == 0 ) THEN
             Q( l ) = pi * GRJAC( IVALJR( k ) )
             IUSED( l ) = 1
             nnonnz = nnonnz + 1
             INONNZ( nnonnz ) = l
           ELSE
             Q( l ) = Q( l ) + pi * GRJAC( IVALJR( k ) )
           END IF
         END DO
       END IF
     END DO

     IF ( .NOT. alllin ) THEN

!  ======================= second-order terms =======================

!  Now consider the product of P with the second order terms (that is, the
!  2nd derivatives of the elements).

       DO j = nvar1, nvar2

!  Consider each nonzero component of P separately

         i = IVAR( j )
         ipt = LINK_elem_uses_var( i )
         IF ( ipt >= 0 ) THEN

!  The index of the I-th component lies in the IEL-th nonlinear element

           iell = LIST_elements( i )
  310      CONTINUE

!  Check to ensure that the IEL-th element has not already been used

           IF ( ISWKSP( iell ) < nbprod ) THEN
             ISWKSP( iell ) = nbprod
             iel = IELING( iell )
             nvarel = ISTAEV( iel + 1 ) - ISTAEV( iel )
             ig = ISLGRP( iell )
             IF ( GXEQX( ig ) ) THEN
               gi = GSCALE( ig ) * ESCALE( iell )
             ELSE
               gi = GSCALE( ig ) * ESCALE( iell ) * GVALS2( ig )
             END IF
             IF ( INTREP( iel ) ) THEN

!  The IEL-th element Hessian has an internal representation. Copy the
!  elemental variables into W

               ll = ISTAEV( iel )
               W_el( : nvarel ) = P( IELVAR( ll : ll + nvarel - 1 ) )

!  Find the internal variables

               nin = INTVAR( iel + 1 ) - INTVAR( iel )
               CALL RANGE ( iel, .FALSE., W_el, W_in, nvarel, nin,             &
                            ITYPEE( iel ), nvarel, nin )

!  Multiply the internal variables by the element Hessian and put the
!  product in W_in. Consider the first column of the element Hessian

               ielhst = ISTADH( iel )
               pi = gi * W_in( 1 )
               H_in( : nin ) = pi * HUVALS( ISYMMH( 1, : nin ) + ielhst )

!  Now consider the remaining columns of the element Hessian

               DO jcol = 2, nin
                 pi = gi * W_in( jcol )
                 IF ( pi /= 0.0_rp_ ) THEN
                   H_in( : nin ) = H_in( : nin ) + pi *                        &
                     HUVALS( ISYMMH( jcol, : nin ) + ielhst )
                 END IF
               END DO

!  Scatter the product back onto the elemental variables, W

               CALL RANGE ( iel, .TRUE., H_in, W_el, nvarel, nin,              &
                            ITYPEE( iel ), nin, nvarel )

!  Add the scattered product to Q

               ll = ISTAEV( iel )
!DIR$ IVDEP
               DO ii = 1, nvarel
                 l = IELVAR( ll )

!  If Q has a nonzero in position L, store its index in INONNZ

                 IF ( ABS( W_el( ii ) ) > smallest ) THEN
                   IF ( IUSED( l ) == 0 ) THEN
                     Q( l ) = W_el( ii )
                     IUSED( l ) = 1
                     nnonnz = nnonnz + 1
                     INONNZ( nnonnz ) = l
                   ELSE
                     Q( l ) = Q( l ) + W_el( ii )
                   END IF
                 END IF
                 ll = ll + 1
               END DO

!  The IEL-th element Hessian has no internal representation

             ELSE
               lthvar = ISTAEV( iel ) - 1
               ielhst = ISTADH( iel )
               DO jcol = 1, nvarel
                 pi = gi * P( IELVAR( lthvar + jcol ) )
                 IF ( pi /= 0.0_rp_ ) THEN
!DIR$ IVDEP
                   DO irow = 1, nvarel
                     ijhess = ISYMMH( jcol, irow ) + ielhst

!  If Q has a nonzero in position L, store its index in INONNZ

                     IF ( ABS( HUVALS( ijhess ) ) > smallest ) THEN
                       l = IELVAR( lthvar + irow )
                       IF ( IUSED( l ) == 0 ) THEN
                         Q( l ) = pi * HUVALS( ijhess )
                         IUSED( l ) = 1
                         nnonnz = nnonnz + 1
                         INONNZ( nnonnz ) = l
                       ELSE
                          Q( l ) = Q( l ) + pi * HUVALS( ijhess )
                       END IF
                     END IF
                   END DO
                 END IF
               END DO
             END IF
           END IF

!  Check to see if there are any further elements whose variables
!  include the I-th variable

           IF ( ipt > 0 ) THEN
             iell = LIST_elements( ipt )
             ipt = LINK_elem_uses_var( ipt )
             GO TO 310
           END IF
         END IF
       END DO
     END IF

!  ==================== the product is complete =======================

!  Reset IUSED to zero

     IUSED( INONNZ( : nnonnz ) ) = 0
     RETURN

!  end of subroutine CUTEST_hessian_times_sp_vector

     END SUBROUTINE CUTEST_hessian_times_sp_vector

! C U T E S T _ a l l o c a t e _ a r r a y _ i n t e g e r  S U B R O U T I N E

     SUBROUTINE CUTEST_allocate_array_integer( ARRAY, new_length, alloc_status )

!  -----------------------------------------------------------------------
!  reallocate an integer array so that its length is at least new_length.
!  If the array is lready allocated and of length at least new_length, the
!  allocation will be skipped and new_length replaced by SIZE(ARRAY)
!  -----------------------------------------------------------------------

!  History -
!   fortran 2003 version first released in SIFDECODE/CUTEst, 26th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER ( KIND = ip_ ), INTENT( OUT ) :: alloc_status
     INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: new_length
     INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ARRAY

     IF ( ALLOCATED( ARRAY ) ) THEN
       IF ( SIZE( ARRAY ) < new_length ) THEN
         DEALLOCATE( ARRAY, STAT = alloc_status )
         IF ( alloc_status /= 0 ) RETURN
       ELSE
         new_length = SIZE( ARRAY )
         alloc_status = 0
         RETURN
       END IF
     END IF
     ALLOCATE( ARRAY( new_length ), STAT = alloc_status )

     RETURN

!  end of subroutine CUTEST_allocate_array_integer

     END SUBROUTINE CUTEST_allocate_array_integer

! -  C U T E S T _ a l l o c a t e _ a r r a y _ r e a l  S U B R O U T I N E  -

     SUBROUTINE CUTEST_allocate_array_real( ARRAY, new_length, alloc_status )

!  -----------------------------------------------------------------------
!  reallocate a real array so that its length is at least new_length.
!  If the array is lready allocated and of length at least new_length, the
!  allocation will be skipped and new_length replaced by SIZE(ARRAY)
!  -----------------------------------------------------------------------

!  History -
!   fortran 2003 version first released in SIFDECODE/CUTEst, 26th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER ( KIND = ip_ ), INTENT( OUT ) :: alloc_status
     INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: new_length
     REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: ARRAY

     IF ( ALLOCATED( ARRAY ) ) THEN
       IF ( SIZE( ARRAY ) < new_length ) THEN
         DEALLOCATE( ARRAY, STAT = alloc_status )
         IF ( alloc_status /= 0 ) RETURN
       ELSE
         new_length = SIZE( ARRAY )
         alloc_status = 0
         RETURN
       END IF
     END IF
     ALLOCATE( ARRAY( new_length ), STAT = alloc_status )

     RETURN

!  end of subroutine CUTEST_allocate_array_real

     END SUBROUTINE CUTEST_allocate_array_real

!-  C U T E S T _ e x t e n d _ a r r a y _ i n t e g e r  S U B R O U T I N E -

     SUBROUTINE CUTEST_extend_array_integer( ARRAY, old_length, used_length,   &
                                             new_length, min_length, buffer,   &
                                             status, alloc_status )

!  -------------------------------------------------------------------------
!  extend an integer array so that its length is increaed from old_length to
!  as close to new_length as possible while keeping existing data intact
!  -------------------------------------------------------------------------

!  History -
!   fortran 90 version released pre GALAHAD Version 1.0. February 7th 1995 as
!     EXTEND_array_integer as part of the GALAHAD module EXTEND
!   fortran 2003 version released in SIFDECODE/CUTEst, 5th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER ( KIND = ip_ ), INTENT( IN ) :: old_length, buffer
     INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, alloc_status
     INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: used_length, min_length
     INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: new_length
     INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: ARRAY

     INTEGER ( KIND = ip_ ) :: length
     LOGICAL :: file_open
     INTEGER ( KIND = ip_ ), ALLOCATABLE, DIMENSION( : ) :: DUMMY

!  make sure that the new length is larger than the old

     IF ( new_length <= old_length ) new_length = 2 * old_length

!  ensure that the input data is consistent

     used_length = MIN( used_length, old_length )
     min_length = MAX( old_length + 1, MIN( min_length, new_length ) )

!  if possible, allocate DUMMY to hold the old values of ARRAY

     ALLOCATE( DUMMY( used_length ), STAT = alloc_status )

!  if the allocation failed, resort to using an external unit

     IF ( alloc_status /= 0 ) GO TO 100

     DUMMY( : used_length ) = ARRAY( : used_length )

!  extend the length of ARRAY

     DEALLOCATE( ARRAY )
     length = new_length

  10 CONTINUE
     ALLOCATE( ARRAY( length ), STAT = alloc_status )

!  if the allocation failed, reduce the new length and retry

     IF ( alloc_status /= 0 ) THEN
       length = length + ( length - min_length ) / 2

!  if there is insufficient room for both ARRAY and DUMMY, use an external unit

       IF ( length < min_length ) THEN

!  rewind the buffer i/o unit

         INQUIRE( UNIT = buffer, OPENED = file_open )
         IF ( file_open ) THEN
           REWIND( UNIT = buffer )
         ELSE
           OPEN( UNIT = buffer )
         END IF

!  copy the contents of ARRAY into the buffer i/o area

         WRITE( UNIT = buffer, FMT = * ) DUMMY( : used_length )

!  extend the length of ARRAY

         DEALLOCATE( DUMMY )
         GO TO 110
       END IF
       GO TO 10
     END IF

!  copy the contents of ARRAY back from the buffer i/o area

     ARRAY( : used_length ) = DUMMY( : used_length )
     DEALLOCATE( DUMMY )
     new_length = length
     GO TO 200

!  use an external unit for writing

 100 CONTINUE

!  rewind the buffer i/o unit

     INQUIRE( UNIT = buffer, OPENED = file_open )
     IF ( file_open ) THEN
       REWIND( UNIT = buffer )
     ELSE
       OPEN( UNIT = buffer )
     END IF

!  copy the contents of ARRAY into the buffer i/o area

     WRITE( UNIT = buffer, FMT = * ) ARRAY( : used_length )

!  extend the length of ARRAY

     DEALLOCATE( ARRAY )

 110 CONTINUE
     ALLOCATE( ARRAY( new_length ), STAT = alloc_status )

!  if the allocation failed, reduce the new length and retry

     IF ( alloc_status /= 0 ) THEN
       new_length = min_length + ( new_length - min_length ) / 2
       IF ( new_length < min_length ) THEN
         status = 12
         RETURN
       END IF
       GO TO 110
     END IF

!  copy the contents of ARRAY back from the buffer i/o area

     REWIND( UNIT = buffer )
     READ( UNIT = buffer, FMT = * ) ARRAY( : used_length )

!  successful exit

 200 CONTINUE
     status = 0
     RETURN

!  end of subroutine CUTEST_extend_array_integer

     END SUBROUTINE CUTEST_extend_array_integer

!-*-  C U T E S T _ e x t e n d _ a r r a y _ r e a l  S U B R O U T I N E -*-

     SUBROUTINE CUTEST_extend_array_real( ARRAY, old_length, used_length,      &
                                          new_length, min_length, buffer,      &
                                          status, alloc_status )

!  ---------------------------------------------------------------------
!  extend a real array so that its length is increaed from old_length to
!  as close to new_length as possible while keeping existing data intact
!  ---------------------------------------------------------------------

!  History -
!   fortran 90 version released pre GALAHAD Version 1.0. February 7th 1995 as
!     EXTEND_array_real as part of the GALAHAD module EXTEND
!   fortran 2003 version released in SIFDECODE/CUTEst, 5th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER ( KIND = ip_ ), INTENT( IN ) :: old_length, buffer
     INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, alloc_status
     INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: used_length, min_length
     INTEGER ( KIND = ip_ ), INTENT( INOUT ) :: new_length
     REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: ARRAY

     INTEGER ( KIND = ip_ ) :: length
     LOGICAL :: file_open
     REAL ( KIND = rp_ ), ALLOCATABLE, DIMENSION( : ) :: DUMMY

!  make sure that the new length is larger than the old

     IF ( new_length <= old_length ) new_length = 2 * old_length

!  ensure that the input data is consistent

     used_length = MIN( used_length, old_length )
     min_length = MAX( old_length + 1, MIN( min_length, new_length ) )

!  if possible, allocate DUMMY to hold the old values of ARRAY

     ALLOCATE( DUMMY( used_length ), STAT = alloc_status )

!  if the allocation failed, resort to using an external unit

     IF ( alloc_status /= 0 ) GO TO 100

     DUMMY( : used_length ) = ARRAY( : used_length )

!  extend the length of ARRAY

     DEALLOCATE( ARRAY )
     length = new_length

  10 CONTINUE
     ALLOCATE( ARRAY( length ), STAT = alloc_status )

!  if the allocation failed, reduce the new length and retry

     IF ( alloc_status /= 0 ) THEN
       length = length + ( length - min_length ) / 2

!  if there is insufficient room for both ARRAY and DUMMY, use an external unit

       IF ( length < min_length ) THEN

!  rewind the buffer i/o unit

         INQUIRE( UNIT = buffer, OPENED = file_open )
         IF ( file_open ) THEN
           REWIND( UNIT = buffer )
         ELSE
           OPEN( UNIT = buffer )
         END IF

!  copy the contents of ARRAY into the buffer i/o area

         WRITE( UNIT = buffer, FMT = * ) DUMMY( : used_length )

!  extend the length of ARRAY

         DEALLOCATE( DUMMY )
         GO TO 110
       END IF
       GO TO 10
     END IF

!  copy the contents of ARRAY back from the buffer i/o area

       ARRAY( : used_length ) = DUMMY( : used_length )
       DEALLOCATE( DUMMY )
       new_length = length
       GO TO 200

!  use an external unit for writing

 100   CONTINUE

!  rewind the buffer i/o unit

     INQUIRE( UNIT = buffer, OPENED = file_open )
     IF ( file_open ) THEN
       REWIND( UNIT = buffer )
     ELSE
       OPEN( UNIT = buffer )
     END IF

!  copy the contents of ARRAY into the buffer i/o area

     WRITE( UNIT = buffer, FMT = * ) ARRAY( : used_length )

!  extend the length of ARRAY

     DEALLOCATE( ARRAY )

 110 CONTINUE
     ALLOCATE( ARRAY( new_length ), STAT = alloc_status )

!  if the allocation failed, reduce the new length and retry

     IF ( alloc_status /= 0 ) THEN
       new_length = min_length + ( new_length - min_length ) / 2
       IF ( new_length < min_length ) THEN
          status = 12
          RETURN
       END IF
       GO TO 110
     END IF

!  copy the contents of ARRAY back from the buffer i/o area

     REWIND( UNIT = buffer )
     READ( UNIT = buffer, FMT = * ) ARRAY( : used_length )

!  successful exit

 200 CONTINUE
     status = 0
     RETURN

!  end of subroutine CUTEST_extend_array_real

     END SUBROUTINE CUTEST_extend_array_real

!-*-*-*-*-*-*-*-  C U T E S T _ s y m m h  S U B R O U T I N E -*-*-*-*-*-*-*-*-

     SUBROUTINE CUTEST_symmh( maxszh, ISYMMH )

!  -------------------------------------------------------------
!  Given a columnwise storage scheme of the upper triangle of a
!  symmetric matrix of order MAXSZH, compute the position of the
!  i,j-th entry of the symmetric matrix in this scheme

!  The value ISYMMH( i, j ) + 1 gives the position of the i,j-th
!  entry of the matrix in the upper triangular scheme
!  -------------------------------------------------------------

!  History -
!   fortran 77 version originally released in CUTE, September 23rd, 1991
!   fortran 90 version released pre GALAHAD Version 1.0. January 26th 1995 as
!     OTHERS_symmh as part of the GALAHAD module OTHERS
!   fortran 2003 version released in CUTEst, 5th November 2012

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

     INTEGER ( KIND = ip_ ), INTENT( IN ) :: maxszh
     INTEGER ( KIND = ip_ ), INTENT( OUT ),                                    &
                             DIMENSION( maxszh, maxszh ) :: ISYMMH

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------

     INTEGER ( KIND = ip_ ) :: i, j, k

     k = 0
     DO j = 1, maxszh
       DO i = 1, j - 1
         ISYMMH( i, j ) = k ; ISYMMH( j, i ) = k ; k = k + 1
       END DO
       ISYMMH( j, j ) = k ; k = k + 1
     END DO
     RETURN

!  end of subroutine CUTEST_symmh

     END SUBROUTINE CUTEST_symmh

!-*-   C U T E S T _ i n i t i a l i z e _ t h r e a d  S U B R O U T I N E  -*-

     SUBROUTINE CUTEST_initialize_thread( data, work, constrained, status,     &
                                          alloc_status, bad_alloc )

!  dummy arguments

     TYPE ( CUTEST_data_type ), INTENT( IN ) :: data
     TYPE ( CUTEST_work_type ), INTENT( OUT ) :: work
     INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, alloc_status
     LOGICAL, INTENT( IN ) :: constrained
     CHARACTER ( LEN = 24 ), INTENT( OUT ) :: bad_alloc

!  set default output values

     status = 0 ; alloc_status = 0 ; bad_alloc = REPEAT( ' ', 24 )

!  set scalar values

     work%lh_row = lmin ; work%lh_col = lmin ; work%lh_val = lmin
     work%nc2of = 0 ; work%nc2og = 0 ; work%nc2oh = 0
     work%nc2cf = 0 ; work%nc2cg = 0 ; work%nc2ch = 0 ; work%nhvpr = 0
     work%njvpr = 0 ; work%pnc = data%numcon
     work%firstg = .TRUE.

!  allocate arrays

     ALLOCATE( work%ISWKSP( MAX( data%ntotel, data%nel, data%n ) ),            &
               STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'work%ISWKSP' ; GO TO 910
     END IF

     ALLOCATE( work%ICALCF( MAX( data%nel, data%ng ) ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'work%ICALCF' ; GO TO 910
     END IF

     ALLOCATE( work%ISTAJC( data%n + 1 ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'work%ISTAJC' ; GO TO 910
     END IF

     ALLOCATE( work%IUSED(  MAX( data%n, data%ng )  ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'work%IUSED' ; GO TO 910
     END IF

     ALLOCATE( work%NZ_components_w( data%ng ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'work%NZ_components_w' ; GO TO 910
     END IF

     ALLOCATE( work%FUVALS( data%lfuval ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'work%FUVALS' ; GO TO 910
     END IF

     ALLOCATE( work%FT( data%ng ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'work%FT' ; GO TO 910
     END IF

     ALLOCATE( work%GSCALE_used( data%ng ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'work%GSCALE_used' ; GO TO 910
     END IF

     ALLOCATE( work%GVALS( data%ng, 3 ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'work%GVALS' ; GO TO 910
     END IF

     ALLOCATE( work%H_el( data%maxsel ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'work%H_el' ; GO TO 910
     END IF

     ALLOCATE( work%H_in( data%maxsin ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'work%H_in' ; GO TO 910
     END IF

     ALLOCATE( work%W_ws( MAX( data%n, data%ng ) ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'work%W_ws' ; GO TO 910
     END IF

     ALLOCATE( work%W_el( data%maxsel ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'work%W_el' ; GO TO 910
     END IF

     ALLOCATE( work%W_in( data%maxsin ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'work%W_in' ; GO TO 910
     END IF

     IF ( constrained ) THEN
       ALLOCATE( work%G_temp( data%n ), STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%G_temp' ; GO TO 910
       END IF

       ALLOCATE( work%LOGIC( data%nel ), STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%LOGIC' ; GO TO 910
       END IF
     END IF

     RETURN

!  unsuccessful returns

  910 CONTINUE
      IF ( data%out > 0 )                                                      &
        WRITE( data%out, "( ' ** Message from -CUTEST_initialize_thread-',     &
     &    /, ' Allocation error (status = ', I0, ') for ', A )" )              &
         alloc_status, bad_alloc
      RETURN

!  end of subroutine CUTEST_initialize_thread

      END SUBROUTINE CUTEST_initialize_thread

!-*-*-  C U T E S T _ t e r m i n a t e _ d a t a   S U B R O U T I N E  -*-*-

     SUBROUTINE CUTEST_terminate_data( data, status, alloc_status, bad_alloc )

!  dummy arguments

     TYPE ( CUTEST_data_type ), INTENT( INOUT ) :: data
     INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, alloc_status
     CHARACTER ( LEN = 24 ), INTENT( OUT ) :: bad_alloc

!  set default output values

     status = 0 ; alloc_status = 0 ; bad_alloc = REPEAT( ' ', 24 )

!  delallocate any array in data that has been allocated

     IF ( ALLOCATED( data%ISTADG ) ) THEN
       DEALLOCATE( data%ISTADG, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%ISTADG' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%ISTGP ) ) THEN
       DEALLOCATE( data%ISTGP, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%ISTGP' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%ISTADA ) ) THEN
       DEALLOCATE( data%ISTADA, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%ISTADA' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%ISTAEV ) ) THEN
       DEALLOCATE( data%ISTAEV, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%ISTAEV' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%ISTEP ) ) THEN
       DEALLOCATE( data%ISTEP, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%ISTEP' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%ITYPEG ) ) THEN
       DEALLOCATE( data%ITYPEG, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%ITYPEG' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%KNDOFC ) ) THEN
       DEALLOCATE( data%KNDOFC, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%KNDOFC' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%ITYPEE ) ) THEN
       DEALLOCATE( data%ITYPEE, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%ITYPEE' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%IELING ) ) THEN
       DEALLOCATE( data%IELING, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%IELING' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%IELVAR ) ) THEN
       DEALLOCATE( data%IELVAR, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%IELVAR' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%ICNA ) ) THEN
       DEALLOCATE( data%ICNA, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%ICNA' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%ISTADH ) ) THEN
       DEALLOCATE( data%ISTADH, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%ISTADH' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%INTVAR ) ) THEN
       DEALLOCATE( data%INTVAR, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%INTVAR' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%IVAR ) ) THEN
       DEALLOCATE( data%IVAR, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%IVAR' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%ITYPEV ) ) THEN
       DEALLOCATE( data%ITYPEV, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%ITYPEV' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%CGROUP ) ) THEN
       DEALLOCATE( data%CGROUP, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%CGROUP' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%ISTAGV ) ) THEN
       DEALLOCATE( data%ISTAGV, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%ISTAGV' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%ISVGRP ) ) THEN
       DEALLOCATE( data%ISVGRP, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%ISVGRP' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%ISLGRP ) ) THEN
       DEALLOCATE( data%ISLGRP, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%ISLGRP' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%IGCOLJ ) ) THEN
       DEALLOCATE( data%IGCOLJ, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%IGCOLJ' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%IVALJR ) ) THEN
       DEALLOCATE( data%IVALJR, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%IVALJR' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%LINK_elem_uses_var ) ) THEN
       DEALLOCATE( data%LINK_elem_uses_var, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%LINK_elem_uses_var' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%ISYMMH ) ) THEN
       DEALLOCATE( data%ISYMMH, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%ISYMMH' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%LIST_elements ) ) THEN
       DEALLOCATE( data%LIST_elements, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%ISYMMH' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%A ) ) THEN
       DEALLOCATE( data%A, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%A' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%B ) ) THEN
       DEALLOCATE( data%B, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%B' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%U ) ) THEN
       DEALLOCATE( data%U, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%U' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%GPVALU ) ) THEN
       DEALLOCATE( data%GPVALU, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%GPVALU' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%EPVALU ) ) THEN
       DEALLOCATE( data%EPVALU, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%EPVALU' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%ESCALE ) ) THEN
       DEALLOCATE( data%ESCALE, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%ESCALE' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%GSCALE ) ) THEN
       DEALLOCATE( data%GSCALE, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%GSCALE' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%VSCALE ) ) THEN
       DEALLOCATE( data%VSCALE, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%VSCALE' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%INTREP ) ) THEN
       DEALLOCATE( data%INTREP, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%INTREP' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%GXEQX ) ) THEN
       DEALLOCATE( data%GXEQX, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%GXEQX' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%GNAMES ) ) THEN
       DEALLOCATE( data%GNAMES, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%GNAMES' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( data%VNAMES ) ) THEN
       DEALLOCATE( data%VNAMES, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'data%VNAMES' ; GO TO 600 ; END IF
     END IF
     RETURN

!  unsuccessful returns

 600 CONTINUE
     status = 1000 + alloc_status
     IF ( data%out > 0 ) WRITE( data%out,                                      &
       "( ' ** Message from -CUTEST_terminate_data-', /, ' Deallocation ',     &
    &  'error for ', A, ', status = ', I0 )" ) bad_alloc, alloc_status
     RETURN

!  end of subroutine CUTEST_terminate_data

     END SUBROUTINE CUTEST_terminate_data

!-*-*-   C U T E S T _ t e r m i n a t e _ w o r k  S U B R O U T I N E   -*-*-

     SUBROUTINE CUTEST_terminate_work( data, work, status,                     &
                                       alloc_status, bad_alloc )

!  dummy arguments

     TYPE ( CUTEST_data_type ), INTENT( INOUT ) :: data
     TYPE ( CUTEST_work_type ), INTENT( INOUT ) :: work
     INTEGER ( KIND = ip_ ), INTENT( OUT ) :: status, alloc_status
     CHARACTER ( LEN = 24 ), INTENT( OUT ) :: bad_alloc

!  set default output values

     status = 0 ; alloc_status = 0 ; bad_alloc = REPEAT( ' ', 24 )

!  delallocate any array in work that has been allocated

     IF ( ALLOCATED( work%ICALCF ) ) THEN
       DEALLOCATE( work%ICALCF, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%ICALCF' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%ROW_start ) ) THEN
       DEALLOCATE( work%ROW_start, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%ROW_start' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%POS_in_H ) ) THEN
       DEALLOCATE( work%POS_in_H, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%POS_in_H' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%USED ) ) THEN
       DEALLOCATE( work%USED, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%USED' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%FILLED ) ) THEN
       DEALLOCATE( work%FILLED, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%FILLED' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%H_row ) ) THEN
       DEALLOCATE( work%H_row, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%H_row' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%H_col ) ) THEN
       DEALLOCATE( work%H_col, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%H_col' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%ISTAJC ) ) THEN
       DEALLOCATE( work%ISTAJC, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%ISTAJC' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%ISWKSP ) ) THEN
       DEALLOCATE( work%ISWKSP, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%ISWKSP' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%IUSED ) ) THEN
       DEALLOCATE( work%IUSED, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%IUSED' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%NZ_components_w ) ) THEN
       DEALLOCATE( work%NZ_components_w, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%NZ_components_w' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%GSCALE_used ) ) THEN
       DEALLOCATE( work%GSCALE_used, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%GSCALE_used' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%G_temp ) ) THEN
       DEALLOCATE( work%G_temp, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%G_temp' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%FT ) ) THEN
       DEALLOCATE( work%FT, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%FT' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%H_val ) ) THEN
       DEALLOCATE( work%H_val, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%H_val' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%J_2d ) ) THEN
       DEALLOCATE( work%J_2d, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%H_val' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%H_2d ) ) THEN
       DEALLOCATE( work%H_2d, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%H_val' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%BAND_2d ) ) THEN
       DEALLOCATE( work%BAND_2d, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%H_val' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%FUVALS ) ) THEN
       DEALLOCATE( work%FUVALS, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%FUVALS' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%W_ws ) ) THEN
       DEALLOCATE( work%W_ws, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%W_ws' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%W_el ) ) THEN
       DEALLOCATE( work%W_el, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%W_el' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%W_in ) ) THEN
       DEALLOCATE( work%W_in, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%W_in' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%H_el ) ) THEN
       DEALLOCATE( work%H_el, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%H_el' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%H_in ) ) THEN
       DEALLOCATE( work%H_in, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%H_in' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%GVALS ) ) THEN
       DEALLOCATE( work%GVALS, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%GVALS' ; GO TO 600 ; END IF
     END IF

     IF ( ALLOCATED( work%LOGIC ) ) THEN
       DEALLOCATE( work%LOGIC, STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN
         bad_alloc = 'work%LOGIC' ; GO TO 600 ; END IF
     END IF
     work%array_status = .FALSE.
     work%hessian_setup_complete = .FALSE.
     work%jacobian_2d_setup_complete = .FALSE.
     work%hessian_2d_setup_complete = .FALSE.
     work%band_2d_setup_complete = .FALSE.

     RETURN

!  unsuccessful returns

 600 CONTINUE
     status = 1000 + alloc_status
     IF ( data%out > 0 ) WRITE( data%out,                                      &
       "( ' ** Message from -CUTEST_terminate_work-', /, ' Deallocation ',     &
    &  'error for ', A, ', status = ', I0 )" ) bad_alloc, alloc_status
     RETURN

!  end of subroutine CUTEST_terminate_work

     END SUBROUTINE CUTEST_terminate_work

!  end of module CUTEST

   END MODULE CUTEST_precision
