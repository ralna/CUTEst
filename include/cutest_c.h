/* THIS VERSION: CUTEST 2.4 - 2024-10-30 AT 10:00 GMT */

/*
 * ======================================================================
 *
 * cutest_c.h
 * Data type definitions, constants definitions and function prototypes
 * to interface the CUTEst testing environment Fortran library with C
 *
 * Proper C 0-based indices, and row-major 2D arrays variant of cutest.h
 *
 * This header file is built from different sources and different authors
 * have contributed to it. In any case, many thanks to Andreas Waechter.
 *
 * Nick Gould and Dominique Orban, adapted from cutest.c with
 *    swap from 1- to 0-based indices, and column- to row-major 
 *    2D arrays, October 15 2024
 *
 * ======================================================================
 */

#include <stdlib.h>
#include <stdbool.h>

#ifndef CUTEST_DOT_H_INCLUDED
#define CUTEST_DOT_H_INCLUDED
#endif

/*
 * give a version number
 */

#define CUTEST_VERSION 2.4.0

/*
 * Define name of main() function on a
 * compiler by compiler basis.
 */
#ifdef Isg95
#define MAINENTRY MAIN_
#endif
#ifdef Ispgf
#define MAINENTRY MAIN_
#endif
#ifdef Isifr
#define MAINENTRY MAIN__
#endif

#ifndef MAINENTRY
#define MAINENTRY main
#endif

/*
 * Define Fortran types for integer and double precision
 * The following choices are from f2c.h
 */
/* typedef long int integer; */
typedef int      integer;
typedef float    real;
typedef double   doublereal;
#ifdef REAL_128
typedef __float128   quadreal;
#endif
/* typedef _Bool    logical; */
typedef bool    logical;

#ifdef REAL_32
typedef float    rp_;
typedef float    rpc_;
#elif REAL_128
typedef __float128    rp_;
typedef __float128    rpc_;
#else
typedef double   rp_;
typedef double   rpc_;
#endif

#ifdef INTEGER_64
typedef long long  ip_;
typedef long long  ipc_;
#else
typedef int        ip_;
typedef int        ipc_;
#endif


#define FALSE_ (0)     /* Fortran FALSE */
#define TRUE_  (1)     /* Fortran  TRUE */
/* #define max(a,b) ((a)>(b)?(a):(b)) */

#define ZERO     0e0
#define ONE      1e0
#define CUTE_INF 1e20    /* 'infinity' in CUTEst interface */
#define FSTRING_LEN 10   /* Length of Fortran strings     */
#define FCSTRING_LEN 30  /* Length of Fortran classification string */

/* AIX does not append underscore to Fortran subroutine names */
#ifdef _AIX
#define FUNDERSCORE(a)   a
#else
#define FUNDERSCORE(a)   a##_
#endif

typedef struct VarTypes {
    int nbnds, neq, nlin, nrange, nlower, nupper,
        nineq, nineq_lin, nineq_nlin, neq_lin,
        neq_nlin;
} VarTypes;

/*
 * Define shortcuts for the CUTEst library functions,
 * and try to avoid the trailing underscore.
 * double-precision (double) procedures
 */

#define CUTEST_usetup_c         FUNDERSCORE(cutest_usetup)
#define CUTEST_csetup_c         FUNDERSCORE(cutest_cint_csetup)

#define CUTEST_udimen_c         FUNDERSCORE(cutest_udimen)
#define CUTEST_udimsh_c         FUNDERSCORE(cutest_udimsh)
#define CUTEST_udimse_c         FUNDERSCORE(cutest_udimse)
#define CUTEST_uvartype_c       FUNDERSCORE(cutest_uvartype)
#define CUTEST_unames_c         FUNDERSCORE(cutest_cint_unames)
#define CUTEST_ureport_c        FUNDERSCORE(cutest_ureport)

#define CUTEST_cdimen_c         FUNDERSCORE(cutest_cdimen)
#define CUTEST_cnoobj_c         FUNDERSCORE(cutest_cint_cnoobj)
#define CUTEST_cdimsg_c         FUNDERSCORE(cutest_cdimsg)
#define CUTEST_cdimsj_c         FUNDERSCORE(cutest_cdimsj)
#define CUTEST_cdimsh_c         FUNDERSCORE(cutest_cdimsh)
#define CUTEST_cdimohp_c        FUNDERSCORE(cutest_cdimohp)
#define CUTEST_cdimchp_c        FUNDERSCORE(cutest_cdimchp)
#define CUTEST_cdimse_c         FUNDERSCORE(cutest_cdimse)
#define CUTEST_cstats_c         FUNDERSCORE(cutest_cstats)
#define CUTEST_cvartype_c       FUNDERSCORE(cutest_cvartype)
#define CUTEST_cnames_c         FUNDERSCORE(cutest_cint_cnames)
#define CUTEST_creport_c        FUNDERSCORE(cutest_creport)
#define CUTEST_classification_c FUNDERSCORE(cutest_cint_classification)
#define CUTEST_connames_c       FUNDERSCORE(cutest_cint_connames)
#define CUTEST_pname_c          FUNDERSCORE(cutest_cint_pname)
#define CUTEST_probname_c       FUNDERSCORE(cutest_cint_probname)
#define CUTEST_varnames_c       FUNDERSCORE(cutest_cint_varnames)
#define CUTEST_ufn_c            FUNDERSCORE(cutest_ufn)
#define CUTEST_ugr_c            FUNDERSCORE(cutest_ugr)
#define CUTEST_uofg_c           FUNDERSCORE(cutest_cint_uofg)
#define CUTEST_ubandh_c         FUNDERSCORE(cutest_ubandh_c)
#define CUTEST_udh_c            FUNDERSCORE(cutest_udh_c)
#define CUTEST_ushp_c           FUNDERSCORE(cutest_ushp_c)
#define CUTEST_ush_c            FUNDERSCORE(cutest_ush_c)
#define CUTEST_ueh_c            FUNDERSCORE(cutest_ueh_c)
#define CUTEST_ugrdh_c          FUNDERSCORE(cutest_ugrdh_c)
#define CUTEST_ugrsh_c          FUNDERSCORE(cutest_ugrsh_c)
#define CUTEST_ugreh_c          FUNDERSCORE(cutest_ugreh_c)
#define CUTEST_uhprod_c         FUNDERSCORE(cutest_cint_uhprod)
#define CUTEST_ushprod_c        FUNDERSCORE(cutest_ushprod_c)

#define CUTEST_cfn_c            FUNDERSCORE(cutest_cfn)
#define CUTEST_cconst_c         FUNDERSCORE(cutest_cconst)
#define CUTEST_cofg_c           FUNDERSCORE(cutest_cint_cofg)
#define CUTEST_cofsg_c          FUNDERSCORE(cutest_cofsg_c)
#define CUTEST_ccfg_c           FUNDERSCORE(cutest_ccfg_c)
#define CUTEST_ccf_c            FUNDERSCORE(cutest_ccf)
#define CUTEST_clfg_c           FUNDERSCORE(cutest_cint_clfg)
#define CUTEST_cgr_c            FUNDERSCORE(cutest_cgr_c)
#define CUTEST_csgr_c           FUNDERSCORE(cutest_csgr_c)
#define CUTEST_csgrp_c          FUNDERSCORE(cutest_csgrp_c)
#define CUTEST_csjp_c           FUNDERSCORE(cutest_csjp_c)
#define CUTEST_ccfsg_c          FUNDERSCORE(cutest_ccfsg_c)
#define CUTEST_ccifg_c          FUNDERSCORE(cutest_ccifg_c)
#define CUTEST_ccifsg_c         FUNDERSCORE(cutest_ccifsg_c)
#define CUTEST_cgrdh_c          FUNDERSCORE(cutest_cgrdh_c)
#define CUTEST_cdh_c            FUNDERSCORE(cutest_cdh_c)
#define CUTEST_cdhc_c           FUNDERSCORE(cutest_cdhc_c)
#define CUTEST_cdhj_c           FUNDERSCORE(cutest_cdhj_c)
#define CUTEST_cshp_c           FUNDERSCORE(cutest_cshp_c)
#define CUTEST_csh_c            FUNDERSCORE(cutest_csh_c)
#define CUTEST_cshc_c           FUNDERSCORE(cutest_cshc_c)
#define CUTEST_cshj_c           FUNDERSCORE(cutest_cshj_c)
#define CUTEST_ceh_c            FUNDERSCORE(cutest_ceh_c)
#define CUTEST_cifn_c           FUNDERSCORE(cutest_cifn_c)
#define CUTEST_cigr_c           FUNDERSCORE(cutest_cigr_c)
#define CUTEST_cisgr_c          FUNDERSCORE(cutest_cisgr_c)
#define CUTEST_cisgrp_c         FUNDERSCORE(cutest_cisgrp_c)
#define CUTEST_cidh_c           FUNDERSCORE(cutest_cidh_c)
#define CUTEST_cish_c           FUNDERSCORE(cutest_cish_c)
#define CUTEST_csgrsh_c         FUNDERSCORE(cutest_csgrsh_c)
#define CUTEST_csgrshp_c        FUNDERSCORE(cutest_csgrshp_c)
#define CUTEST_csgreh_c         FUNDERSCORE(cutest_csgreh_c)
#define CUTEST_chprod_c         FUNDERSCORE(cutest_cint_chprod)
#define CUTEST_cshprod_c        FUNDERSCORE(cutest_cshprod_c)
#define CUTEST_cshcprod_c       FUNDERSCORE(cutest_cshcprod_c)
#define CUTEST_chcprod_c        FUNDERSCORE(cutest_cint_chcprod)
#define CUTEST_chjprod_c        FUNDERSCORE(cutest_cint_chjprod)
#define CUTEST_cjprod_c         FUNDERSCORE(cutest_cint_cjprod)
#define CUTEST_csjprod_c        FUNDERSCORE(cutest_csjprod_c)
#define CUTEST_cchprods_c       FUNDERSCORE(cutest_cchprods_c)
#define CUTEST_cchprodsp_c      FUNDERSCORE(cutest_cchprodsp_c)
#define CUTEST_cohprods_c       FUNDERSCORE(cutest_cohprods_c)
#define CUTEST_cohprodsp_c      FUNDERSCORE(cutest_cohprodsp_c)

#define CUTEST_uterminate_c     FUNDERSCORE(cutest_uterminate)
#define CUTEST_cterminate_c     FUNDERSCORE(cutest_cterminate)

#define FORTRAN_open_c          FUNDERSCORE(fortran_open)
#define FORTRAN_close_c         FUNDERSCORE(fortran_close)

/*
 * Prototypes for CUTEst FORTRAN routines found in libcutest.a/libcutest.so
 * See https://github.com/ralna/CUTEst
 */

/*_c Setup routines */
void CUTEST_usetup_c( integer *status, const integer *funit,
                      const integer *iout, const integer *io_buffer,
                      integer *n, doublereal *x, doublereal *bl,
                      doublereal *bu );
void CUTEST_csetup_c( integer *status, const integer *funit,
                      const integer *iout,
                      const integer *io_buffer, integer *n, integer *m,
                      doublereal *x, doublereal *bl, doublereal *bu,
                      doublereal *v, doublereal *cl, doublereal *cu,
                      logical *equatn, logical *linear, 
                      const integer *e_order, const integer *l_order, 
                      const integer *v_order );

/* Unconstrained dimensioning and report routines */
void CUTEST_udimen_c( integer *status, const integer *funit, integer *n );
void CUTEST_udimsh_c( integer *status, integer *nnzh );
void CUTEST_udimse_c( integer *status, integer *ne, integer *nzh,
                      integer *nzirnh );
void CUTEST_uvartype_c(integer *status, const integer *n, integer *ivarty );
void CUTEST_unames_c( integer *status, const integer *n, char *pname,
                      char *vnames );
void CUTEST_ureport_c ( integer *status, doublereal *calls, doublereal *time );

/* Constrained dimensioning and report routines */
void CUTEST_cdimen_c( integer *status, const integer *funit, integer *n,
                      integer *m );
void CUTEST_cnoobj_c( integer *status, const integer *funit, logical *noobj );
void CUTEST_cdimsg_c( integer *status, integer *nnzg );
void CUTEST_cdimsj_c( integer *status, integer *nnzj );
void CUTEST_cdimsh_c( integer *status, integer *nnzh );
void CUTEST_cdimohp_c( integer *status, integer *nnzohp );
void CUTEST_cdimchp_c( integer *status, integer *nnzchp );
void CUTEST_cdimse_c( integer *status, integer *ne, integer *nzh,
                      integer *nzirnh );
void CUTEST_cstats_c( integer *status, integer *nonlinear_variables_objective,
                      integer *nonlinear_variables_constraints,
                      integer *equality_constraints,
                      integer *linear_constraints );
void CUTEST_cvartype_c( integer *status, const integer *n, integer *ivarty );
void CUTEST_cnames_c( integer *status, const integer *n, const integer *m,
                      char *pname, char *vnames, char *gnames );
void CUTEST_creport_c( integer *status, doublereal *calls, doublereal *time );

void CUTEST_connames_c( integer *status, const integer *m, char *gname );
void CUTEST_pname_c   ( integer *status, const integer *funit, char *pname );
void CUTEST_classification_c( integer *status, const integer *funit, 
                              char *classification );
void CUTEST_probname_c( integer *status, char *pname );
void CUTEST_varnames_c( integer *status, const integer *n, char *vname );

/* Unconstrained optimization routines */
void CUTEST_ufn_c( integer *status, const integer *n, const doublereal *x,
                   doublereal *f );
void CUTEST_ugr_c( integer *status, const integer *n, const doublereal *x,
                   doublereal *g );
void CUTEST_uofg_c( integer *status, const integer *n, const doublereal *x,
                    doublereal *f, doublereal *g, const logical *grad );
void CUTEST_udh_c( integer *status, const integer *n, const doublereal *x,
                   const integer *lh1, doublereal *h );
void CUTEST_ushp_c( integer *status, const integer *n, integer *nnzh,
                    const integer *lh, integer *irnh, integer *icnh );
void CUTEST_ush_c( integer *status, const integer *n, const doublereal *x,
                   integer *nnzh, const integer *lh, doublereal *h,
                   integer *irnh, integer *icnh );
void CUTEST_ueh_c( integer *status, const integer *n, const doublereal *x,
                   integer *ne, const integer *le, integer *iprnhi,
                   integer *iprhi, const integer *lirnhi, integer *irnhi,
                   const integer *lhi, doublereal *hi,
                   const logical *byrows );
void CUTEST_ugrdh_c( integer *status, const integer *n, const doublereal *x,
                     doublereal *g, const integer *lh1, doublereal *h);
void CUTEST_ugrsh_c( integer *status, const integer *n, const doublereal *x,
                     doublereal *g, integer *nnzh, integer *lh, 
                     doublereal *h, integer *irnh, integer *icnh );
void CUTEST_ugreh_c( integer *status, const integer *n, const doublereal *x,
                     doublereal *g, integer *ne, const integer *le,
                     integer *iprnhi, integer *iprhi, const integer *lirnhi,
                     integer *irnhi, const integer *lhi, doublereal *hi,
                     const logical *byrows );
void CUTEST_uhprod_c( integer *status, const integer *n, const logical *goth,
                      const doublereal *x, const doublereal *p, 
                      doublereal *r );
void CUTEST_ushprod_c( integer *status, const integer *n, const logical *goth,
                       const doublereal *x, const integer *nnzp,
                       const integer *indp, const doublereal *p,
                       integer *nnzr, integer *indr, doublereal *r );
void CUTEST_ubandh_c( integer *status, const integer *n, const doublereal *x,
                      const integer *nsemib, doublereal *bandh,
                      const integer *lbandh, integer *maxsbw );

/* Constrained optimization routines */
void CUTEST_cfn_c( integer *status,  const integer *n, const integer *m,
                   const doublereal *x, doublereal *f, doublereal *c );
void CUTEST_cconst_c( integer *status,  const integer *m, doublereal *c );
void CUTEST_cofg_c( integer *status, const integer *n, const doublereal *x,
                    doublereal *f, doublereal *g, const logical *grad );
void CUTEST_cofsg_c( integer *status, const integer *n, const doublereal *x,
                     doublereal *f, integer *nnzg, const integer *lg,
                     doublereal *sg, integer *ivsg, const logical *grad );
void CUTEST_ccf_c( integer *status, const integer *n, const integer *m,
                   const doublereal *x, doublereal *c );
void CUTEST_ccfg_c( integer *status, const integer *n, const integer *m,
                    const doublereal *x, doublereal *c, 
                    const logical *jtrans, const integer *lcjac1, 
                    const integer *lcjac2, doublereal *cjac, 
                    const logical *grad );
void CUTEST_clfg_c( integer *status, const integer *n, const integer *m,
                    const doublereal *x, const doublereal *y, doublereal *f,
                    doublereal *g, const logical *grad );
void CUTEST_cgr_c( integer *status,  const integer *n, const integer *m,
                   const doublereal *x, const doublereal *y,
                   const logical *grlagf, doublereal *g,
                   const logical *jtrans, const integer *lcjac1,
                   const integer *lcjac2, doublereal *cjac );
void CUTEST_csgr_c( integer *status, const integer *n, const integer *m,
                    const doublereal *x, const doublereal *y,
                    const logical *grlagf, integer *nnzj,
                    const integer *lcjac, doublereal *cjac,
                    integer *indvar, integer *indfun );
void CUTEST_csgrp_c( integer *status, const integer *n, integer *nnzj,
                     const integer *lj, integer *jvar, integer *jcon );
void CUTEST_csjp_c( integer *status, integer *nnzj, const integer *lj,
                    integer *jvar, integer *jcon );
void CUTEST_ccfsg_c( integer *status,  const integer *n, const integer *m,
                     const doublereal *x, doublereal *c, integer *nnzj,
                     const integer *lcjac, doublereal *cjac, integer *indvar,
                     integer *indfun, const logical *grad );
void CUTEST_ccifg_c( integer *status,  const integer *n, const integer *icon,
                     const doublereal *x, doublereal *ci, doublereal *gci,
                     const logical *grad );
void CUTEST_ccifsg_c( integer *status, const integer *n, const integer *con,
                      const doublereal *x, doublereal *ci, integer *nnzsgc,
                      const integer *lsgci, doublereal *sgci, integer *ivsgci,
                      const logical *grad );
void CUTEST_cgrdh_c( integer *status, const integer *n, const integer *m,
                     const doublereal *x, const doublereal *y,
                     const logical *grlagf, doublereal *g,
                     const logical *jtrans, const integer *lcjac1,
                     const integer *lcjac2, doublereal *cjac,
                     const integer *lh1, doublereal *h );
void CUTEST_cdh_c( integer *status, const integer *n, const integer *m,
                    const doublereal *x, const doublereal *y,
                    const integer *lh1, doublereal *h );
void CUTEST_cdhc_c( integer *status, const integer *n, const integer *m,
                    const doublereal *x, const doublereal *y,
                    const integer *lh1, doublereal *h );
void CUTEST_cdhj_c( integer *status, const integer *n, const integer *m,
                    const doublereal *x, const doublereal *y0,
                    const doublereal *y, const integer *lh1,
                    doublereal *h );
void CUTEST_cshp_c( integer *status, const integer *n, integer *nnzh,
                    const integer *lh, integer *irnh, integer *icnh );
void CUTEST_csh_c( integer *status, const integer *n, const integer *m,
                   const doublereal *x, const doublereal *y, integer *nnzh,
                   const integer *lh, doublereal *h, integer *irnh,
                   integer *icnh );
void CUTEST_cshc_c( integer *status, const integer *n, const integer *m,
                    const doublereal *x, const doublereal *y, integer *nnzh,
                    const integer *lh, doublereal *h,
                    integer *irnh, integer *icnh );
void CUTEST_cshj_c( integer *status, const integer *n, const integer *m,
                    const doublereal *x, const doublereal *y0,
                    const doublereal *y, integer *nnzh,
                    const integer *lh, doublereal *h,
                        integer *irnh, integer *icnh );
void CUTEST_ceh_c( integer *status, const integer *n, const integer *m,
                   const doublereal *x, const doublereal *y,
                   integer *ne, const integer *le, integer *iprnhi,
                   integer *iprhi, const integer *lirnhi, integer *irnhi,
                   const integer *lhi, doublereal *hi,
                   const logical *byrows );
void CUTEST_cifn_c( integer *status, const integer *n, const integer *iprob,
                    const doublereal *x, doublereal *f );
void CUTEST_cigr_c( integer *status, const integer *n, const integer *iprob,
                    const doublereal *x, doublereal *g );
void CUTEST_cisgr_c( integer *status, const integer *n, const integer *iprob,
                     const doublereal *x, integer *nnzg, const integer *lg,
                     doublereal *sg, integer *ivsg );
void CUTEST_cisgrp_c( integer *status, const integer *n, const integer *iprob,
                      integer *nnzg, const integer *lg, integer *ivsg );
void CUTEST_cidh_c( integer *status, const integer *n, const doublereal *x,
                    const integer *iprob, const integer *lh1, 
                    doublereal *h );
void CUTEST_cish_c( integer *status, const integer *n, const doublereal *x,
                    const integer *iprob, integer *nnzh, const integer *lh,
                    doublereal *h, integer *irnh, integer *icnh );
void CUTEST_csgrsh_c( integer *status, const integer *n, const integer *m,
                      const doublereal *x, const doublereal *y,
                      const logical *grlagf, integer *nnzj,
                      const integer *lcjac, doublereal *cjac, integer *indvar,
                      integer *indfun, integer *nnzh, const integer *lh,
                      doublereal *h, integer *irnh, integer *icnh );
void CUTEST_csgrshp_c( integer *status, const integer *n, integer *nnzj,
                       const integer *lcjac, integer *indvar,
                       integer *indfun, integer *nnzh, const integer *lh,
                       integer *irnh, integer *icnh );
void CUTEST_csgreh_c( integer *status, const integer *n, const integer *m,
                      const doublereal *x, const doublereal *y,
                      const logical *grlagf, integer *nnzj,
                      const integer *lcjac, doublereal *cjac,
                      integer *indvar, integer *indfun,
                      integer *ne, const integer *le, integer *iprnhi,
                      integer *iprhi, const integer *lirnhi,
                      integer *irnhi, const integer *lhi, doublereal *hi,
                      const logical *byrows );
void CUTEST_chprod_c( integer *status, const integer *n, const integer *m,
                      const logical *goth, const doublereal *x,
                      const doublereal *y, doublereal *p, doublereal *q );
void CUTEST_cshprod_c( integer *status, const integer *n, const integer *m,
                       const logical *goth, const doublereal *x,
                       const doublereal *y, const integer *nnzp,
                       const integer *indp, const doublereal *p,
                       integer *nnzr, integer *indr, doublereal *r );
void CUTEST_chcprod_c( integer *status, const integer *n, const integer *m,
                       const logical *goth, const doublereal *x,
                       const doublereal *y, doublereal *p, doublereal *q );
void CUTEST_cshcprod_c( integer *status, const integer *n, const integer *m,
                        const logical *goth, const doublereal *x,
                        const doublereal *y, integer *nnzp, integer *indp,
                        doublereal *p, integer *nnzr, integer *indr,
                        doublereal *r );
void CUTEST_chjprod_c( integer *status, const integer *n, const integer *m,
                       const logical *goth, const doublereal *x,
                       const doublereal *y0,
                       const doublereal *y, doublereal *p, doublereal *q );
void CUTEST_cjprod_c( integer *status, const integer *n, const integer *m,
                      const logical *gotj, const logical *jtrans,
                      const doublereal *x, const doublereal *p,
                      const integer *lp, doublereal *r, const integer *lr );
void CUTEST_csjprod_c( integer *status, const integer *n, const integer *m,
                       const logical *gotj, const logical *jtrans,
                       const doublereal *x, const integer *nnzp,
                       const integer *indp, const doublereal *p,
                       const integer *lp, integer *nnzr,
                       integer *indr, doublereal *r, const integer *lr );
void CUTEST_cchprods_c( integer *status, const integer *n, const integer *m,
                        const logical *goth, const doublereal *x,
                        const doublereal *p, const integer *lchp,
                        doublereal *chpval, integer *chpind, integer *chpptr );
void CUTEST_cchprodsp_c( integer *status, const integer *m,
                         const integer *lchp, integer *chpind, 
                         integer *chpptr );
void CUTEST_cohprods_c( integer *status, const integer *n,
                        const logical *goth, const doublereal *x,
                        const doublereal *p, integer *nnzohp, 
                        const integer *lohp, doublereal *ohpval, 
                        integer *ohpind );
void CUTEST_cohprodsp_c( integer *status, integer *nnzohp,
                         const integer *lohp, integer *chpind );

/* Termination routines */
void CUTEST_uterminate_c ( integer *status );
void CUTEST_cterminate_c ( integer *status );

/* FORTRAN auxiliary subroutines to retrieve stream unit numbers */
void FORTRAN_open_c(  const integer *funit, const char *fname, integer *ierr );
void FORTRAN_close_c( const integer *funit, integer *ierr );

/*
 * Define shortcuts for the CUTEst library functions,
 * and try to avoid the trailing underscore.
 * single-precision (float) procedures
 */

#define CUTEST_usetup_c_s         FUNDERSCORE(cutest_usetup_s)
#define CUTEST_csetup_c_s         FUNDERSCORE(cutest_cint_csetup_s)

#define CUTEST_udimen_c_s         FUNDERSCORE(cutest_udimen_s)
#define CUTEST_udimsh_c_s         FUNDERSCORE(cutest_udimsh_s)
#define CUTEST_udimse_c_s         FUNDERSCORE(cutest_udimse_s)
#define CUTEST_uvartype_c_s       FUNDERSCORE(cutest_uvartype_s)
#define CUTEST_unames_c_s         FUNDERSCORE(cutest_cint_unames_s)
#define CUTEST_ureport_c_s        FUNDERSCORE(cutest_ureport_s)

#define CUTEST_cdimen_c_s         FUNDERSCORE(cutest_cdimen_s)
#define CUTEST_cnoobj_c_s         FUNDERSCORE(cutest_cint_cnoobj_s)
#define CUTEST_cdimsg_c_s         FUNDERSCORE(cutest_cdimsg_s)
#define CUTEST_cdimsj_c_s         FUNDERSCORE(cutest_cdimsj_s)
#define CUTEST_cdimsh_c_s         FUNDERSCORE(cutest_cdimsh_s)
#define CUTEST_cdimohp_c_s        FUNDERSCORE(cutest_cdimohp_s)
#define CUTEST_cdimchp_c_s        FUNDERSCORE(cutest_cdimchp_s)
#define CUTEST_cdimse_c_s         FUNDERSCORE(cutest_cdimse_s)
#define CUTEST_cstats_c_s         FUNDERSCORE(cutest_cstats_s)
#define CUTEST_cvartype_c_s       FUNDERSCORE(cutest_cvartype_s)
#define CUTEST_cnames_c_s         FUNDERSCORE(cutest_cint_cnames_s)
#define CUTEST_creport_c_s        FUNDERSCORE(cutest_creport_s)

#define CUTEST_classification_c_s FUNDERSCORE(cutest_cint_classification_s)
#define CUTEST_connames_c_s       FUNDERSCORE(cutest_cint_connames_s)
#define CUTEST_pname_c_s          FUNDERSCORE(cutest_cint_pname_s)
#define CUTEST_probname_c_s       FUNDERSCORE(cutest_cint_probname_s)
#define CUTEST_varnames_c_s       FUNDERSCORE(cutest_cint_varnames_s)

#define CUTEST_ufn_c_s            FUNDERSCORE(cutest_ufn_s)
#define CUTEST_ugr_c_s            FUNDERSCORE(cutest_ugr_s)
#define CUTEST_uofg_c_s           FUNDERSCORE(cutest_cint_uofg_s)
#define CUTEST_ubandh_c_s         FUNDERSCORE(cutest_ubandh_c_s)
#define CUTEST_udh_c_s            FUNDERSCORE(cutest_udh_c_s)
#define CUTEST_ushp_c_s           FUNDERSCORE(cutest_ushp_c_s)
#define CUTEST_ush_c_s            FUNDERSCORE(cutest_ush_c_s)
#define CUTEST_ueh_c_s            FUNDERSCORE(cutest_ueh_c_s)
#define CUTEST_ugrdh_c_s          FUNDERSCORE(cutest_ugrdh_c_s)
#define CUTEST_ugrsh_c_s          FUNDERSCORE(cutest_ugrsh_c_s)
#define CUTEST_ugreh_c_s          FUNDERSCORE(cutest_ugreh_c_s)
#define CUTEST_uhprod_c_s         FUNDERSCORE(cutest_cint_uhprod_s)
#define CUTEST_ushprod_c_s        FUNDERSCORE(cutest_ushprod_c_s)

#define CUTEST_cfn_c_s            FUNDERSCORE(cutest_cfn_s)
#define CUTEST_cconst_c_s         FUNDERSCORE(cutest_cconst_s)
#define CUTEST_cofg_c_s           FUNDERSCORE(cutest_cint_cofg_s)
#define CUTEST_cofsg_c_s          FUNDERSCORE(cutest_cofsg_c_s)
#define CUTEST_ccfg_c_s           FUNDERSCORE(cutest_ccfg_c_s)
#define CUTEST_ccf_c_s            FUNDERSCORE(cutest_ccf_s)
#define CUTEST_clfg_c_s           FUNDERSCORE(cutest_cint_clfg_s)
#define CUTEST_cgr_c_s            FUNDERSCORE(cutest_cgr_c_s)
#define CUTEST_csgr_c_s           FUNDERSCORE(cutest_csgr_c_s)
#define CUTEST_csgrp_c_s          FUNDERSCORE(cutest_csgrp_c_s)
#define CUTEST_csjp_c_s           FUNDERSCORE(cutest_csjp_c_s)
#define CUTEST_ccfsg_c_s          FUNDERSCORE(cutest_ccfsg_c_s)
#define CUTEST_ccifg_c_s          FUNDERSCORE(cutest_ccifg_c_s)
#define CUTEST_ccifsg_c_s         FUNDERSCORE(cutest_ccifsg_c_s)
#define CUTEST_cgrdh_c_s          FUNDERSCORE(cutest_cgrdh_c_s)
#define CUTEST_cdh_c_s            FUNDERSCORE(cutest_cdh_c_s)
#define CUTEST_cdhc_c_s           FUNDERSCORE(cutest_cdhc_c_s)
#define CUTEST_cdhj_c_s           FUNDERSCORE(cutest_cdhj_c_s)
#define CUTEST_cshp_c_s           FUNDERSCORE(cutest_cshp_c_s)
#define CUTEST_csh_c_s            FUNDERSCORE(cutest_csh_c_s)
#define CUTEST_cshc_c_s           FUNDERSCORE(cutest_cshc_c_s)
#define CUTEST_cshj_c_s           FUNDERSCORE(cutest_cshj_c_s)
#define CUTEST_ceh_c_s            FUNDERSCORE(cutest_ceh_c_s)
#define CUTEST_cifn_c_s           FUNDERSCORE(cutest_cifn_c_s)
#define CUTEST_cigr_c_s           FUNDERSCORE(cutest_cigr_c_s)
#define CUTEST_cisgr_c_s          FUNDERSCORE(cutest_cisgr_c_s)
#define CUTEST_cisgrp_c_s         FUNDERSCORE(cutest_cisgrp_c_s)
#define CUTEST_cidh_c_s           FUNDERSCORE(cutest_cidh_c_s)
#define CUTEST_cish_c_s           FUNDERSCORE(cutest_cish_c_s)
#define CUTEST_csgrsh_c_s         FUNDERSCORE(cutest_csgrsh_c_s)
#define CUTEST_csgrshp_c_s        FUNDERSCORE(cutest_csgrshp_c_s)
#define CUTEST_csgreh_c_s         FUNDERSCORE(cutest_csgreh_c_s)
#define CUTEST_chprod_c_s         FUNDERSCORE(cutest_cint_chprod_s)
#define CUTEST_cshprod_c_s        FUNDERSCORE(cutest_cshprod_c_s)
#define CUTEST_cshcprod_c_s       FUNDERSCORE(cutest_cshcprod_c_s)
#define CUTEST_chcprod_c_s        FUNDERSCORE(cutest_cint_chcprod_s)
#define CUTEST_chjprod_c_s        FUNDERSCORE(cutest_cint_chjprod_s)
#define CUTEST_cjprod_c_s         FUNDERSCORE(cutest_cint_cjprod_s)
#define CUTEST_csjprod_c_s        FUNDERSCORE(cutest_csjprod_c_s)
#define CUTEST_cchprods_c_s       FUNDERSCORE(cutest_cchprods_c_s)
#define CUTEST_cchprodsp_c_s      FUNDERSCORE(cutest_cchprodsp_c_s)
#define CUTEST_cohprods_c_s       FUNDERSCORE(cutest_cohprods_c_s)
#define CUTEST_cohprodsp_c_s      FUNDERSCORE(cutest_cohprodsp_c_s)

#define CUTEST_uterminate_c_s     FUNDERSCORE(cutest_uterminate_s)
#define CUTEST_cterminate_c_s     FUNDERSCORE(cutest_cterminate_s)

#define FORTRAN_open_c_s          FUNDERSCORE(fortran_open_s)
#define FORTRAN_close_c_s         FUNDERSCORE(fortran_close_s)

/*
 * Prototypes for CUTEst FORTRAN routines found in libcutest.a/libcutest.so
 * See https://github.com/ralna/CUTEst
 */

/* Setup routines */
void CUTEST_usetup_c_s  ( integer *status, const integer *funit,
                        const integer *iout, const integer *io_buffer,
                        integer *n, real *x, real *bl,
                        real *bu );
void CUTEST_csetup_c_s  ( integer *status, const integer *funit,
                        const integer *iout,
                        const integer *io_buffer, integer *n, integer *m,
                        real *x, real *bl, real *bu,
                        real *v, real *cl, real *cu,
                        logical *equatn, logical *linear, 
                        const integer *e_order, const integer *l_order, 
                        const integer *v_order );

/* Unconstrained dimensioning and report routines */
void CUTEST_udimen_c_s  ( integer *status, const integer *funit, integer *n );
void CUTEST_udimsh_c_s  ( integer *status, integer *nnzh );
void CUTEST_udimse_c_s  ( integer *status, integer *ne, integer *nzh,
                          integer *nzirnh );
void CUTEST_uvartype_c_s( integer *status, const integer *n, integer *ivarty );
void CUTEST_unames_c_s  ( integer *status, const integer *n, char *pname,
                          char *vnames );
void CUTEST_ureport_c_s ( integer *status, real *calls, real *time );

/* Constrained dimensioning and report routines */
void CUTEST_cdimen_c_s  ( integer *status, const integer *funit, integer *n,
                          integer *m );
void CUTEST_cnoobj_c_s  ( integer *status, const integer *funit, 
                          logical *noobj );
void CUTEST_cdimsg_c_s  ( integer *status, integer *nnzg );
void CUTEST_cdimsj_c_s  ( integer *status, integer *nnzj );
void CUTEST_cdimsh_c_s  ( integer *status, integer *nnzh );
void CUTEST_cdimohp_c_s ( integer *status, integer *nnzohp );
void CUTEST_cdimchp_c_s ( integer *status, integer *nnzchp );
void CUTEST_cdimse_c_s  ( integer *status, integer *ne, integer *nzh,
                          integer *nzirnh );
void CUTEST_cstats_c_s  ( integer *status, 
                          integer *nonlinear_variables_objective,
                          integer *nonlinear_variables_constraints,
                          integer *equality_constraints,
                          integer *linear_constraints );
void CUTEST_cvartype_c_s( integer *status, const integer *n, integer *ivarty );
void CUTEST_cnames_c_s  ( integer *status, const integer *n, const integer *m,
                          char *pname, char *vnames, char *gnames );
void CUTEST_creport_c_s ( integer *status, real *calls, real *time );

void CUTEST_connames_c_s( integer *status, const integer *m, char *gname );
void CUTEST_pname_c_s   ( integer *status, const integer *funit, char *pname );
void CUTEST_classification_c_s( integer *status, const integer *funit, 
                                char *classification );
void CUTEST_probname_c_s( integer *status, char *pname );
void CUTEST_varnames_c_s( integer *status, const integer *n, char *vname );

/* Unconstrained optimization routines */
void CUTEST_ufn_c_s     ( integer *status, const integer *n, const real *x,
                          real *f );
void CUTEST_ugr_c_s     ( integer *status, const integer *n, const real *x,
                          real *g );
void CUTEST_uofg_c_s    ( integer *status, const integer *n, const real *x,
                          real *f, real *g, const logical *grad );
void CUTEST_udh_c_s     ( integer *status, const integer *n, const real *x,
                          const integer *lh1, real *h );
void CUTEST_ushp_c_s    ( integer *status, const integer *n, integer *nnzh,
                          const integer *lh, integer *irnh, integer *icnh );
void CUTEST_ush_c_s     ( integer *status, const integer *n, const real *x,
                          integer *nnzh, const integer *lh, real *h,
                          integer *irnh, integer *icnh );
void CUTEST_ueh_c_s     ( integer *status, const integer *n, const real *x,
                          integer *ne, const integer *le, integer *iprnhi,
                          integer *iprhi, const integer *lirnhi, integer *irnhi,
                          const integer *lhi, real *hi,
                          const logical *byrows );
void CUTEST_ugrdh_c_s   ( integer *status, const integer *n, const real *x,
                          real *g, const integer *lh1, real *h);
void CUTEST_ugrsh_c_s   ( integer *status, const integer *n, const real *x,
                          real *g, integer *nnzh, integer *lh, real *h,
                          integer *irnh, integer *icnh );
void CUTEST_ugreh_c_s   ( integer *status, const integer *n, const real *x,
                          real *g, integer *ne, const integer *le,
                          integer *iprnhi, integer *iprhi, 
                          const integer *lirnhi, integer *irnhi, 
                          const integer *lhi, real *hi, 
                           const logical *byrows );
void CUTEST_uhprod_c_s  ( integer *status, const integer *n, 
                          const logical *goth, const real *x, const real *p, 
                          real *r );
void CUTEST_ushprod_c_s ( integer *status, const integer *n, 
                          const logical *goth,
                          const real *x, const integer *nnzp,
                          const integer *indp, const real *p,
                          integer *nnzr, integer *indr, real *r );
void CUTEST_ubandh_c_s  ( integer *status, const integer *n, const real *x,
                          const integer *nsemib, real *bandh,
                          const integer *lbandh, integer *maxsbw );

/* Constrained optimization routines */
void CUTEST_cfn_c_s     ( integer *status,  const integer *n, const integer *m,
                          const real *x, real *f, real *c );
void CUTEST_cconst_c_s  ( integer *status,  const integer *m, real *c );
void CUTEST_cofg_c_s    ( integer *status, const integer *n, const real *x,
                          real *f, real *g, const logical *grad );
void CUTEST_cofsg_c_s   ( integer *status, const integer *n, const real *x,
                          real *f, integer *nnzg, const integer *lg,
                          real *sg, integer *ivsg, const logical *grad );
void CUTEST_ccf_c_s     ( integer *status, const integer *n, const integer *m,
                          const real *x, real *c );
void CUTEST_ccfg_c_s    ( integer *status, const integer *n, const integer *m,
                          const real *x, real *c, const logical *jtrans,
                          const integer *lcjac1, const integer *lcjac2,
                          real *cjac, const logical *grad );
void CUTEST_clfg_c_s    ( integer *status, const integer *n, const integer *m,
                          const real *x, const real *y, real *f,
                          real *g, const logical *grad );
void CUTEST_cgr_c_s     ( integer *status,  const integer *n, const integer *m,
                          const real *x, const real *y,
                          const logical *grlagf, real *g,
                          const logical *jtrans, const integer *lcjac1,
                          const integer *lcjac2, real *cjac );
void CUTEST_csgr_c_s    ( integer *status, const integer *n, const integer *m,
                          const real *x, const real *y,
                          const logical *grlagf, integer *nnzj,
                          const integer *lcjac, real *cjac,
                          integer *indvar, integer *indfun );
void CUTEST_csgrp_c_s   ( integer *status, const integer *n, integer *nnzj,
                          const integer *lj, integer *jvar, integer *jcon );
void CUTEST_csjp_c_s    ( integer *status, integer *nnzj, const integer *lj,
                          integer *jvar, integer *jcon );
void CUTEST_ccfsg_c_s   ( integer *status,  const integer *n, const integer *m,
                          const real *x, real *c, integer *nnzj,
                          const integer *lcjac, real *cjac, integer *indvar,
                          integer *indfun, const logical *grad );
void CUTEST_ccifg_c_s   ( integer *status,  const integer *n, 
                          const integer *icon, const real *x, real *ci, 
                          real *gci, const logical *grad );
void CUTEST_ccifsg_c_s  ( integer *status, const integer *n, const integer *con,
                          const real *x, real *ci, integer *nnzsgc,
                          const integer *lsgci, real *sgci, integer *ivsgci,
                          const logical *grad );
void CUTEST_cgrdh_c_s   ( integer *status, const integer *n, const integer *m,
                          const real *x, const real *y,
                          const logical *grlagf, real *g,
                          const logical *jtrans, const integer *lcjac1,
                          const integer *lcjac2, real *cjac,
                          const integer *lh1, real *h );
void CUTEST_cdh_c_s     ( integer *status, const integer *n, const integer *m,
                          const real *x, const real *y,
                          const integer *lh1, real *h );
void CUTEST_cdhc_c_s    ( integer *status, const integer *n, const integer *m,
                          const real *x, const real *y,
                          const integer *lh1, real *h );
void CUTEST_cdhj_c_s    ( integer *status, const integer *n, const integer *m,
                          const real *x, const real *y0, const real *y,
                          const integer *lh1, real *h );
void CUTEST_cshp_c_s    ( integer *status, const integer *n, integer *nnzh,
                          const integer *lh, integer *irnh, integer *icnh );
void CUTEST_csh_c_s     ( integer *status, const integer *n, const integer *m,
                          const real *x, const real *y, integer *nnzh,
                          const integer *lh, real *h, integer *irnh,
                          integer *icnh );
void CUTEST_cshc_c_s    ( integer *status, const integer *n, const integer *m,
                          const real *x, const real *y, integer *nnzh,
                          const integer *lh, real *h,
                          integer *irnh, integer *icnh );
void CUTEST_cshj_c_s    ( integer *status, const integer *n, const integer *m,
                          const real *x, const real *y0,
                          const real *y, integer *nnzh,
                          const integer *lh, real *h,
                          integer *irnh, integer *icnh );
void CUTEST_ceh_c_s     ( integer *status, const integer *n, const integer *m,
                          const real *x, const real *y,
                          integer *ne, const integer *le, integer *iprnhi,
                          integer *iprhi, const integer *lirnhi, integer *irnhi,
                          const integer *lhi, real *hi,
                          const logical *byrows );
void CUTEST_cifn_c_s    ( integer *status, const integer *n, 
                          const integer *iprob, const real *x, real *f );
void CUTEST_cigr_c_s    ( integer *status, const integer *n, 
                          const integer *iprob, const real *x, real *g );
void CUTEST_cisgr_c_s   ( integer *status, const integer *n, 
                          const integer *iprob, const real *x, 
                          integer *nnzg, const integer *lg,
                          real *sg, integer *ivsg );
void CUTEST_cisgrp_c_s  ( integer *status, const integer *n, 
                          const integer *iprob, integer *nnzg, 
                          const integer *lg, integer *ivsg );
void CUTEST_cidh_c_s    ( integer *status, const integer *n, const real *x,
                          const integer *iprob, const integer *lh1, real *h );
void CUTEST_cish_c_s    ( integer *status, const integer *n, const real *x,
                          const integer *iprob, integer *nnzh, 
                          const integer *lh, real *h, integer *irnh, 
                          integer *icnh );
void CUTEST_csgrsh_c_s  ( integer *status, const integer *n, const integer *m,
                          const real *x, const real *y,
                          const logical *grlagf, integer *nnzj,
                          const integer *lcjac, real *cjac, integer *indvar,
                          integer *indfun, integer *nnzh, const integer *lh,
                          real *h, integer *irnh, integer *icnh );
void CUTEST_csgrshp_c_s ( integer *status, const integer *n, integer *nnzj,
                          const integer *lcjac, integer *indvar,
                          integer *indfun, integer *nnzh, const integer *lh,
                          integer *irnh, integer *icnh );
void CUTEST_csgreh_c_s  ( integer *status, const integer *n, const integer *m,
                          const real *x, const real *y,
                          const logical *grlagf, integer *nnzj,
                          const integer *lcjac, real *cjac,
                          integer *indvar, integer *indfun,
                          integer *ne, const integer *le, integer *iprnhi,
                          integer *iprhi, const integer *lirnhi,
                          integer *irnhi, const integer *lhi, real *hi,
                          const logical *byrows );
void CUTEST_chprod_c_s  ( integer *status, const integer *n, const integer *m,
                          const logical *goth, const real *x,
                          const real *y, real *p, real *q );
void CUTEST_cshprod_c_s ( integer *status, const integer *n, const integer *m,
                          const logical *goth, const real *x,
                          const real *y, const integer *nnzp,
                          const integer *indp, const real *p,
                          integer *nnzr, integer *indr, real *r );
void CUTEST_chcprod_c_s ( integer *status, const integer *n, const integer *m,
                          const logical *goth, const real *x,
                          const real *y, real *p, real *q );
void CUTEST_cshcprod_c_s( integer *status, const integer *n, const integer *m,
                          const logical *goth, const real *x,
                          const real *y, integer *nnzp, integer *indp,
                          real *p, integer *nnzr, integer *indr,
                          real *r );
void CUTEST_chjprod_c_s ( integer *status, const integer *n, const integer *m,
                          const logical *goth, const real *x,
                          const real *y0,
                          const real *y, real *p, real *q );
void CUTEST_cjprod_c_s  ( integer *status, const integer *n, const integer *m,
                          const logical *gotj, const logical *jtrans,
                          const real *x, const real *p,
                          const integer *lp, real *r, const integer *lr );
void CUTEST_csjprod_c_s ( integer *status, const integer *n, const integer *m,
                          const logical *gotj, const logical *jtrans,
                          const real *x, const integer *nnzp,
                          const integer *indp, const real *p,
                          const integer *lp, integer *nnzr,
                          integer *indr, real *r, const integer *lr );
void CUTEST_cchprods_c_s( integer *status, const integer *n, const integer *m,
                          const logical *goth, const real *x,
                          const real *p, const integer *lchp,
                          real *chpval, integer *chpind, integer *chpptr );
void CUTEST_cchprodsp_c_s( integer *status, const integer *m,
                           const integer *lchp, integer *chpind, 
                           integer *chpptr );
void CUTEST_cohprods_c_s( integer *status, const integer *n,
                          const logical *goth, const real *x,
                          const real *p, integer *nnzohp, const integer *lohp,
                          real *ohpval, integer *ohpind );
void CUTEST_cohprodsp_c_s( integer *status, integer *nnzohp,
                           const integer *lohp, integer *chpind );

/* Termination routines */
void CUTEST_uterminate_c_s( integer *status );
void CUTEST_cterminate_c_s( integer *status );

/* FORTRAN auxiliary subroutines to retrieve stream unit numbers */
void FORTRAN_open_c_s( const integer *funit, const char *fname, 
                       integer *ierr );
void FORTRAN_close_c_s( const integer *funit, integer *ierr );

/*
 * Define shortcuts for the CUTEst library functions,
 * and try to avoid the trailing underscore.
 * quadruple-precision (quad) procedures, if available
 */

#ifdef REAL_128

#define CUTEST_usetup_c_q         FUNDERSCORE(cutest_usetup_q)
#define CUTEST_csetup_c_q         FUNDERSCORE(cutest_cint_csetup_q)

#define CUTEST_udimen_c_q         FUNDERSCORE(cutest_udimen_q)
#define CUTEST_udimsh_c_q         FUNDERSCORE(cutest_udimsh_q)
#define CUTEST_udimse_c_q         FUNDERSCORE(cutest_udimse_q)
#define CUTEST_uvartype_c_q       FUNDERSCORE(cutest_uvartype_q)
#define CUTEST_unames_c_q         FUNDERSCORE(cutest_cint_unames_q)
#define CUTEST_ureport_c_q        FUNDERSCORE(cutest_ureport_q)

#define CUTEST_cdimen_c_q         FUNDERSCORE(cutest_cdimen_q)
#define CUTEST_cnoobj_c_q         FUNDERSCORE(cutest_cint_cnoobj_q)
#define CUTEST_cdimsg_c_q         FUNDERSCORE(cutest_cdimsg_q)
#define CUTEST_cdimsj_c_q         FUNDERSCORE(cutest_cdimsj_q)
#define CUTEST_cdimsh_c_q         FUNDERSCORE(cutest_cdimsh_q)
#define CUTEST_cdimohp_c_q        FUNDERSCORE(cutest_cdimohp_q)
#define CUTEST_cdimchp_c_q        FUNDERSCORE(cutest_cdimchp_q)
#define CUTEST_cdimse_c_q         FUNDERSCORE(cutest_cdimse_q)
#define CUTEST_cstats_c_q         FUNDERSCORE(cutest_cstats_q)
#define CUTEST_cvartype_c_q       FUNDERSCORE(cutest_cvartype_q)
#define CUTEST_cnames_c_q         FUNDERSCORE(cutest_cint_cnames_q)
#define CUTEST_creport_c_q        FUNDERSCORE(cutest_creport_q)

#define CUTEST_classification_c_q FUNDERSCORE(cutest_cint_classification_q)
#define CUTEST_connames_c_q       FUNDERSCORE(cutest_cint_connames_q)
#define CUTEST_pname_c_q          FUNDERSCORE(cutest_cint_pname_q)
#define CUTEST_probname_c_q       FUNDERSCORE(cutest_cint_probname_q)
#define CUTEST_varnames_c_q       FUNDERSCORE(cutest_cint_varnames_q)

#define CUTEST_ufn_c_q            FUNDERSCORE(cutest_ufn_q)
#define CUTEST_ugr_c_q            FUNDERSCORE(cutest_ugr_q)
#define CUTEST_uofg_c_q           FUNDERSCORE(cutest_cint_uofg_q)
#define CUTEST_ubandh_c_q         FUNDERSCORE(cutest_ubandh_c_q)
#define CUTEST_udh_c_q            FUNDERSCORE(cutest_udh_c_q)
#define CUTEST_ushp_c_q           FUNDERSCORE(cutest_ushp_c_q)
#define CUTEST_ush_c_q            FUNDERSCORE(cutest_ush_c_q)
#define CUTEST_ueh_c_q            FUNDERSCORE(cutest_ueh_c_q)
#define CUTEST_ugrdh_c_q          FUNDERSCORE(cutest_ugrdh_c_q)
#define CUTEST_ugrsh_c_q          FUNDERSCORE(cutest_ugrsh_c_q)
#define CUTEST_ugreh_c_q          FUNDERSCORE(cutest_ugreh_c_q)
#define CUTEST_uhprod_c_q         FUNDERSCORE(cutest_cint_uhprod_q)
#define CUTEST_ushprod_c_q        FUNDERSCORE(cutest_ushprod_c_q)

#define CUTEST_cfn_c_q            FUNDERSCORE(cutest_cfn_q)
#define CUTEST_cconst_c_q         FUNDERSCORE(cutest_cconst_q)
#define CUTEST_cofg_c_q           FUNDERSCORE(cutest_cint_cofg_q)
#define CUTEST_cofsg_c_q          FUNDERSCORE(cutest_cofsg_c_q)
#define CUTEST_ccfg_c_q           FUNDERSCORE(cutest_ccfg_c_q)
#define CUTEST_ccf_c_q            FUNDERSCORE(cutest_ccf_q)
#define CUTEST_clfg_c_q           FUNDERSCORE(cutest_cint_clfg_q)
#define CUTEST_cgr_c_q            FUNDERSCORE(cutest_cgr_c_q)
#define CUTEST_csgr_c_q           FUNDERSCORE(cutest_csgr_c_q)
#define CUTEST_csgrp_c_q          FUNDERSCORE(cutest_csgrp_c_q)
#define CUTEST_csjp_c_q           FUNDERSCORE(cutest_csjp_c_q)
#define CUTEST_ccfsg_c_q          FUNDERSCORE(cutest_ccfsg_c_q)
#define CUTEST_ccifg_c_q          FUNDERSCORE(cutest_ccifg_c_q)
#define CUTEST_ccifsg_c_q         FUNDERSCORE(cutest_ccifsg_c_q)
#define CUTEST_cgrdh_c_q          FUNDERSCORE(cutest_cgrdh_c_q)
#define CUTEST_cdh_c_q            FUNDERSCORE(cutest_cdh_c_q)
#define CUTEST_cdhc_c_q           FUNDERSCORE(cutest_cdhc_c_q)
#define CUTEST_cdhj_c_q           FUNDERSCORE(cutest_cdhj_c_q)
#define CUTEST_cshp_c_q           FUNDERSCORE(cutest_cshp_c_q)
#define CUTEST_csh_c_q            FUNDERSCORE(cutest_csh_c_q)
#define CUTEST_cshc_c_q           FUNDERSCORE(cutest_cshc_c_q)
#define CUTEST_cshj_c_q           FUNDERSCORE(cutest_cshj_c_q)
#define CUTEST_ceh_c_q            FUNDERSCORE(cutest_ceh_c_q)
#define CUTEST_cifn_c_q           FUNDERSCORE(cutest_cifn_c_q)
#define CUTEST_cigr_c_q           FUNDERSCORE(cutest_cigr_c_q)
#define CUTEST_cisgr_c_q          FUNDERSCORE(cutest_cisgr_c_q)
#define CUTEST_cisgrp_c_q         FUNDERSCORE(cutest_cisgrp_c_q)
#define CUTEST_cidh_c_q           FUNDERSCORE(cutest_cidh_c_q)
#define CUTEST_cish_c_q           FUNDERSCORE(cutest_cish_c_q)
#define CUTEST_csgrsh_c_q         FUNDERSCORE(cutest_csgrsh_c_q)
#define CUTEST_csgrshp_c_q        FUNDERSCORE(cutest_csgrshp_c_q)
#define CUTEST_csgreh_c_q         FUNDERSCORE(cutest_csgreh_c_q)
#define CUTEST_chprod_c_q         FUNDERSCORE(cutest_cint_chprod_q)
#define CUTEST_cshprod_c_q        FUNDERSCORE(cutest_cshprod_c_q)
#define CUTEST_cshcprod_c_q       FUNDERSCORE(cutest_cshcprod_c_q)
#define CUTEST_chcprod_c_q        FUNDERSCORE(cutest_cint_chcprod_q)
#define CUTEST_chjprod_c_q        FUNDERSCORE(cutest_cint_chjprod_q)
#define CUTEST_cjprod_c_q         FUNDERSCORE(cutest_cint_cjprod_q)
#define CUTEST_csjprod_c_q        FUNDERSCORE(cutest_csjprod_c_q)
#define CUTEST_cchprods_c_q       FUNDERSCORE(cutest_cchprods_c_q)
#define CUTEST_cchprodsp_c_q      FUNDERSCORE(cutest_cchprodsp_c_q)
#define CUTEST_cohprods_c_q       FUNDERSCORE(cutest_cohprods_c_q)
#define CUTEST_cohprodsp_c_q      FUNDERSCORE(cutest_cohprodsp_c_q)

#define CUTEST_uterminate_c_q     FUNDERSCORE(cutest_uterminate_q)
#define CUTEST_cterminate_c_q     FUNDERSCORE(cutest_cterminate_q)

#define FORTRAN_open_c_q          FUNDERSCORE(fortran_open_q)
#define FORTRAN_close_c_q         FUNDERSCORE(fortran_close_q)

/*
 * Prototypes for CUTEst FORTRAN routines found in libcutest.a/libcutest.so
 * See https://github.com/ralna/CUTEst
 */

/* Setup routines */
void CUTEST_usetup_c_q  ( integer *status, const integer *funit,
                          const integer *iout, const integer *io_buffer,
                          integer *n, quadreal *x, quadreal *bl,
                          quadreal *bu );
void CUTEST_csetup_c_q  ( integer *status, const integer *funit,
                          const integer *iout,
                          const integer *io_buffer, integer *n, integer *m,
                          quadreal *x, quadreal *bl, quadreal *bu,
                          quadreal *v, quadreal *cl, quadreal *cu,
                          logical *equatn, logical *linear, 
                          const integer *e_order, const integer *l_order, 
                          const integer *v_order );

/* Unconstrained dimensioning and report routines */
void CUTEST_udimen_c_q  ( integer *status, const integer *funit, integer *n );
void CUTEST_udimsh_c_q  ( integer *status, integer *nnzh );
void CUTEST_udimse_c_q  ( integer *status, integer *ne, integer *nzh,
                          integer *nzirnh );
void CUTEST_uvartype_c_q( integer *status, const integer *n, integer *ivarty );
void CUTEST_unames_c_q  ( integer *status, const integer *n, char *pname,
                          char *vnames );
void CUTEST_ureport_c_q ( integer *status, quadreal *calls, quadreal *time );

/* Constrained dimensioning and report routines */
void CUTEST_cdimen_c_q  ( integer *status, const integer *funit, integer *n,
                          integer *m );
void CUTEST_cnoobj_c_q  ( integer *status, const integer *funit, 
                          logical *noobj );
void CUTEST_cdimsg_c_q  ( integer *status, integer *nnzg );
void CUTEST_cdimsj_c_q  ( integer *status, integer *nnzj );
void CUTEST_cdimsh_c_q  ( integer *status, integer *nnzh );
void CUTEST_cdimohp_c_q ( integer *status, integer *nnzohp );
void CUTEST_cdimchp_c_q ( integer *status, integer *nnzchp );
void CUTEST_cdimse_c_q  ( integer *status, integer *ne, integer *nzh,
                          integer *nzirnh );
void CUTEST_cstats_c_q  ( integer *status, 
                          integer *nonlinear_variables_objective,
                          integer *nonlinear_variables_constraints,
                          integer *equality_constraints,
                          integer *linear_constraints );
void CUTEST_cvartype_c_q( integer *status, const integer *n, integer *ivarty );
void CUTEST_cnames_c_q  ( integer *status, const integer *n, const integer *m,
                          char *pname, char *vnames, char *gnames );
void CUTEST_creport_c_q ( integer *status, quadreal *calls, quadreal *time );

void CUTEST_connames_c_q( integer *status, const integer *m, char *gname );
void CUTEST_pname_c_q   ( integer *status, const integer *funit, char *pname );
void CUTEST_classification_c_q( integer *status, const integer *funit, 
                                char *classification );
void CUTEST_probname_c_q( integer *status, char *pname );
void CUTEST_varnames_c_q( integer *status, const integer *n, char *vname );

/* Unconstrained optimization routines */
void CUTEST_ufn_c_q     ( integer *status, const integer *n, const quadreal *x,
                          quadreal *f );
void CUTEST_ugr_c_q     ( integer *status, const integer *n, const quadreal *x,
                          quadreal *g );
void CUTEST_uofg_c_q    ( integer *status, const integer *n, const quadreal *x,
                          quadreal *f, quadreal *g, const logical *grad );
void CUTEST_udh_c_q     ( integer *status, const integer *n, const quadreal *x,
                          const integer *lh1, quadreal *h );
void CUTEST_ushp_c_q    ( integer *status, const integer *n, integer *nnzh,
                          const integer *lh, integer *irnh, integer *icnh );
void CUTEST_ush_c_q     ( integer *status, const integer *n, const quadreal *x,
                          integer *nnzh, const integer *lh, quadreal *h,
                          integer *irnh, integer *icnh );
void CUTEST_ueh_c_q     ( integer *status, const integer *n, const quadreal *x,
                          integer *ne, const integer *le, integer *iprnhi,
                          integer *iprhi, const integer *lirnhi, integer *irnhi,
                          const integer *lhi, quadreal *hi,
                          const logical *byrows );
void CUTEST_ugrdh_c_q   ( integer *status, const integer *n, const quadreal *x,
                          quadreal *g, const integer *lh1, quadreal *h);
void CUTEST_ugrsh_c_q   ( integer *status, const integer *n, const quadreal *x,
                          quadreal *g, integer *nnzh, integer *lh, quadreal *h,
                          integer *irnh, integer *icnh );
void CUTEST_ugreh_c_q   ( integer *status, const integer *n, const quadreal *x,
                          quadreal *g, integer *ne, const integer *le,
                          integer *iprnhi, integer *iprhi, 
                          const integer *lirnhi, integer *irnhi, 
                          const integer *lhi, quadreal *hi,
                          const logical *byrows );
void CUTEST_uhprod_c_q  ( integer *status, const integer *n, 
                          const logical *goth, const quadreal *x, 
                          const quadreal *p, quadreal *r );
void CUTEST_ushprod_c_q ( integer *status, const integer *n, 
                          const logical *goth,
                          const quadreal *x, const integer *nnzp,
                          const integer *indp, const quadreal *p,
                          integer *nnzr, integer *indr, quadreal *r );
void CUTEST_ubandh_c_q  ( integer *status, const integer *n, const quadreal *x,
                          const integer *nsemib, quadreal *bandh,
                          const integer *lbandh, integer *maxsbw );

/* Constrained optimization routines */
void CUTEST_cfn_c_q     ( integer *status,  const integer *n, const integer *m,
                          const quadreal *x, quadreal *f, quadreal *c );
void CUTEST_cconst_c_q  ( integer *status,  const integer *m, quadreal *c );
void CUTEST_cofg_c_q    ( integer *status, const integer *n, const quadreal *x,
                          quadreal *f, quadreal *g, const logical *grad );
void CUTEST_cofsg_c_q   ( integer *status, const integer *n, const quadreal *x,
                          quadreal *f, integer *nnzg, const integer *lg,
                          quadreal *sg, integer *ivsg, const logical *grad );
void CUTEST_ccf_c_q     ( integer *status, const integer *n, const integer *m,
                          const quadreal *x, quadreal *c );
void CUTEST_ccfg_c_q    ( integer *status, const integer *n, const integer *m,
                          const quadreal *x, quadreal *c, const logical *jtrans,
                          const integer *lcjac1, const integer *lcjac2,
                          quadreal *cjac, const logical *grad );
void CUTEST_clfg_c_q    ( integer *status, const integer *n, const integer *m,
                          const quadreal *x, const quadreal *y, quadreal *f,
                          quadreal *g, const logical *grad );
void CUTEST_cgr_c_q     ( integer *status,  const integer *n, const integer *m,
                          const quadreal *x, const quadreal *y,
                          const logical *grlagf, quadreal *g,
                          const logical *jtrans, const integer *lcjac1,
                          const integer *lcjac2, quadreal *cjac );
void CUTEST_csgr_c_q    ( integer *status, const integer *n, const integer *m,
                          const quadreal *x, const quadreal *y,
                          const logical *grlagf, integer *nnzj,
                          const integer *lcjac, quadreal *cjac,
                          integer *indvar, integer *indfun );
void CUTEST_csgrp_c_q   ( integer *status, const integer *n, integer *nnzj,
                          const integer *lj, integer *jvar, integer *jcon );
void CUTEST_csjp_c_q    ( integer *status, integer *nnzj, const integer *lj,
                          integer *jvar, integer *jcon );
void CUTEST_ccfsg_c_q   ( integer *status,  const integer *n, const integer *m,
                          const quadreal *x, quadreal *c, integer *nnzj,
                          const integer *lcjac, quadreal *cjac, integer *indvar,
                          integer *indfun, const logical *grad );
void CUTEST_ccifg_c_q   ( integer *status,  const integer *n, 
                          const integer *icon, const quadreal *x, 
                          quadreal *ci, quadreal *gci, const logical *grad );
void CUTEST_ccifsg_c_q  ( integer *status, const integer *n, const integer *con,
                          const quadreal *x, quadreal *ci, integer *nnzsgc,
                          const integer *lsgci, quadreal *sgci, integer *ivsgci,
                          const logical *grad );
void CUTEST_cgrdh_c_q   ( integer *status, const integer *n, const integer *m,
                          const quadreal *x, const quadreal *y,
                          const logical *grlagf, quadreal *g,
                          const logical *jtrans, const integer *lcjac1,
                          const integer *lcjac2, quadreal *cjac,
                          const integer *lh1, quadreal *h );
void CUTEST_cdh_c_q     ( integer *status, const integer *n, const integer *m,
                          const quadreal *x, const quadreal *y,
                          const integer *lh1, quadreal *h );
void CUTEST_cdhc_c_q    ( integer *status, const integer *n, const integer *m,
                          const quadreal *x, const quadreal *y,
                          const integer *lh1, quadreal *h );
void CUTEST_cdhj_c_q   ( integer *status, const integer *n, const integer *m,
                          const quadreal *x, const quadreal *y0, 
                          const quadreal *y, const integer *lh1, quadreal *h );
void CUTEST_cshp_c_q    ( integer *status, const integer *n, integer *nnzh,
                          const integer *lh, integer *irnh, integer *icnh );
void CUTEST_csh_c_q     ( integer *status, const integer *n, const integer *m,
                          const quadreal *x, const quadreal *y, integer *nnzh,
                          const integer *lh, quadreal *h, integer *irnh,
                          integer *icnh );
void CUTEST_cshc_c_q    ( integer *status, const integer *n, const integer *m,
                      const quadreal *x, const quadreal *y, integer *nnzh,
                      const integer *lh, quadreal *h,
                      integer *irnh, integer *icnh );
void CUTEST_cshj_c_q    ( integer *status, const integer *n, const integer *m,
                          const quadreal *x, const quadreal *y0,
                          const quadreal *y, integer *nnzh,
                          const integer *lh, quadreal *h,
                          integer *irnh, integer *icnh );
void CUTEST_ceh_c_q     ( integer *status, const integer *n, const integer *m,
                          const quadreal *x, const quadreal *y,
                          integer *ne, const integer *le, integer *iprnhi,
                          integer *iprhi, const integer *lirnhi, integer *irnhi,
                          const integer *lhi, quadreal *hi,
                          const logical *byrows );
void CUTEST_cifn_c_q    ( integer *status, const integer *n, 
                          const integer *iprob,
                          const quadreal *x, quadreal *f );
void CUTEST_cigr_c_q    ( integer *status, const integer *n, 
                          const integer *iprob, const quadreal *x, 
                          quadreal *g );
void CUTEST_cisgr_c_q   ( integer *status, const integer *n, 
                          const integer *iprob, const quadreal *x, 
                          integer *nnzg, const integer *lg,
                          quadreal *sg, integer *ivsg );
void CUTEST_cisgrp_c_q  ( integer *status, const integer *n, 
                          const integer *iprob, integer *nnzg, 
                          const integer *lg, integer *ivsg );
void CUTEST_cidh_c_q    ( integer *status, const integer *n, const quadreal *x,
                          const integer *iprob, const integer *lh1, 
                          quadreal *h );
void CUTEST_cish_c_q    ( integer *status, const integer *n, const quadreal *x,
                          const integer *iprob, integer *nnzh, 
                          const integer *lh, quadreal *h, 
                          integer *irnh, integer *icnh );
void CUTEST_csgrsh_c_q  ( integer *status, const integer *n, const integer *m,
                          const quadreal *x, const quadreal *y,
                          const logical *grlagf, integer *nnzj,
                          const integer *lcjac, quadreal *cjac, integer *indvar,
                          integer *indfun, integer *nnzh, const integer *lh,
                          quadreal *h, integer *irnh, integer *icnh );
void CUTEST_csgrshp_c_q ( integer *status, const integer *n, integer *nnzj,
                          const integer *lcjac, integer *indvar,
                          integer *indfun, integer *nnzh, const integer *lh,
                          integer *irnh, integer *icnh );
void CUTEST_csgreh_c_q  ( integer *status, const integer *n, const integer *m,
                          const quadreal *x, const quadreal *y,
                          const logical *grlagf, integer *nnzj,
                          const integer *lcjac, quadreal *cjac,
                          integer *indvar, integer *indfun,
                          integer *ne, const integer *le, integer *iprnhi,
                          integer *iprhi, const integer *lirnhi,
                          integer *irnhi, const integer *lhi, quadreal *hi,
                          const logical *byrows );
void CUTEST_chprod_c_q  ( integer *status, const integer *n, const integer *m,
                          const logical *goth, const quadreal *x,
                          const quadreal *y, quadreal *p, quadreal *q );
void CUTEST_cshprod_c_q ( integer *status, const integer *n, const integer *m,
                          const logical *goth, const quadreal *x,
                          const quadreal *y, const integer *nnzp,
                          const integer *indp, const quadreal *p,
                          integer *nnzr, integer *indr, quadreal *r );
void CUTEST_chcprod_c_q ( integer *status, const integer *n, const integer *m,
                          const logical *goth, const quadreal *x,
                          const quadreal *y, quadreal *p, quadreal *q );
void CUTEST_cshcprod_c_q( integer *status, const integer *n, const integer *m,
                          const logical *goth, const quadreal *x,
                          const quadreal *y, integer *nnzp, integer *indp,
                          quadreal *p, integer *nnzr, integer *indr,
                          quadreal *r );
void CUTEST_chjprod_c_q ( integer *status, const integer *n, const integer *m,
                          const logical *goth, const quadreal *x,
                          const quadreal *y0,
                          const quadreal *y, quadreal *p, quadreal *q );
void CUTEST_cjprod_c_q  ( integer *status, const integer *n, const integer *m,
                          const logical *gotj, const logical *jtrans,
                          const quadreal *x, const quadreal *p,
                          const integer *lp, quadreal *r, const integer *lr );
void CUTEST_csjprod_c_q ( integer *status, const integer *n, const integer *m,
                          const logical *gotj, const logical *jtrans,
                          const quadreal *x, const integer *nnzp,
                          const integer *indp, const quadreal *p,
                          const integer *lp, integer *nnzr,
                          integer *indr, quadreal *r, const integer *lr );
void CUTEST_cchprods_c_q( integer *status, const integer *n, const integer *m,
                          const logical *goth, const quadreal *x,
                          const quadreal *p, const integer *lchp,
                          quadreal *chpval, integer *chpind, integer *chpptr );
void CUTEST_cchprodsp_c_q( integer *status, const integer *m,
                           const integer *lchp, integer *chpind, 
                           integer *chpptr );
void CUTEST_cohprods_c_q( integer *status, const integer *n,
                          const logical *goth, const quadreal *x,
                          const quadreal *p, integer *nnzohp, 
                          const integer *lohp, quadreal *ohpval, 
                          integer *ohpind );
void CUTEST_cohprodsp_c_q( integer *status, integer *nnzohp,
                           const integer *lohp, integer *chpind );

/* Termination routines */
void CUTEST_uterminate_c_q( integer *status );
void CUTEST_cterminate_c_q( integer *status );

/* FORTRAN auxiliary subroutines to retrieve stream unit numbers */
void FORTRAN_open_c_q(  const integer *funit, const char *fname, 
                        integer *ierr );
void FORTRAN_close_c_q( const integer *funit, integer *ierr );

#endif

/*
 * Memory allocation shortcuts
 */

void *CUTEst_malloc( void *object, int length, size_t s );
void *CUTEst_calloc( void *object, int length, size_t s );
void *CUTEst_realloc( void *object, int length, size_t s );
void  CUTEst_free( void **object );

#ifndef MALLOC
#define MALLOC(object,length,type)  object = (type *)CUTEst_malloc(object,length,sizeof(type))
#endif
#ifndef CALLOC
#define CALLOC(object,length,type)  object = (type *)CUTEst_calloc(object,length,sizeof(type))
#endif
#ifndef REALLOC
#define REALLOC(object,length,type) object = (type *)CUTEst_realloc(object,length,sizeof(type))
#endif
#ifndef FREE
#define FREE(object) CUTEst_free((void **)(&(object)))
#endif
