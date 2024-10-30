/* THIS VERSION: CUTEST 2.4 - 2024-10-30 AT 09:00 GMT */

/*
 * ======================================================================
 *
 * cutest.h
 * Data type definitions, constants definitions and function prototypes
 * to interface the CUTEst testing environment Fortran library with C
 *
 * Fortran 1-based indices for interactions with Matlab an Julia
 *
 * This header file is built from different sources and different authors
 * have contributed to it. In any case, many thanks to Andreas Waechter.
 *
 * D. Orban. CUTEr version, July 10 2002.
 * Nick Gould, CUTEst evolution, January 4 2013.
 *             Boolean logicals provided, August 21 2013
 *             fortran intent(in) variables defined as const, Dec 2 2015
 *             optional quadruple precision support added, January 2024
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

#define CUTEST_usetup         FUNDERSCORE(cutest_usetup)
#define CUTEST_csetup         FUNDERSCORE(cutest_cint_csetup)

#define CUTEST_udimen         FUNDERSCORE(cutest_udimen)
#define CUTEST_udimsh         FUNDERSCORE(cutest_udimsh)
#define CUTEST_udimse         FUNDERSCORE(cutest_udimse)
#define CUTEST_uvartype       FUNDERSCORE(cutest_uvartype)
#define CUTEST_unames         FUNDERSCORE(cutest_cint_unames)
#define CUTEST_ureport        FUNDERSCORE(cutest_ureport)

#define CUTEST_cdimen         FUNDERSCORE(cutest_cdimen)
#define CUTEST_cnoobj         FUNDERSCORE(cutest_cint_cnoobj)
#define CUTEST_cdimsg         FUNDERSCORE(cutest_cdimsg)
#define CUTEST_cdimsj         FUNDERSCORE(cutest_cdimsj)
#define CUTEST_cdimsh         FUNDERSCORE(cutest_cdimsh)
#define CUTEST_cdimohp        FUNDERSCORE(cutest_cdimohp)
#define CUTEST_cdimchp        FUNDERSCORE(cutest_cdimchp)
#define CUTEST_cdimse         FUNDERSCORE(cutest_cdimse)
#define CUTEST_cstats         FUNDERSCORE(cutest_cstats)
#define CUTEST_cvartype       FUNDERSCORE(cutest_cvartype)
#define CUTEST_cnames         FUNDERSCORE(cutest_cint_cnames)
#define CUTEST_creport        FUNDERSCORE(cutest_creport)
#define CUTEST_classification FUNDERSCORE(cutest_cint_classification)
#define CUTEST_connames       FUNDERSCORE(cutest_cint_connames)
#define CUTEST_pname          FUNDERSCORE(cutest_cint_pname)
#define CUTEST_probname       FUNDERSCORE(cutest_cint_probname)
#define CUTEST_varnames       FUNDERSCORE(cutest_cint_varnames)

#define CUTEST_ufn            FUNDERSCORE(cutest_ufn)
#define CUTEST_ugr            FUNDERSCORE(cutest_ugr)
#define CUTEST_uofg           FUNDERSCORE(cutest_cint_uofg)
#define CUTEST_ubandh         FUNDERSCORE(cutest_ubandh)
#define CUTEST_udh            FUNDERSCORE(cutest_udh)
#define CUTEST_ushp           FUNDERSCORE(cutest_ushp)
#define CUTEST_ush            FUNDERSCORE(cutest_ush)
#define CUTEST_ueh            FUNDERSCORE(cutest_cint_ueh)
#define CUTEST_ugrdh          FUNDERSCORE(cutest_ugrdh)
#define CUTEST_ugrsh          FUNDERSCORE(cutest_ugrsh)
#define CUTEST_ugreh          FUNDERSCORE(cutest_cint_ugreh)
#define CUTEST_uhprod         FUNDERSCORE(cutest_cint_uhprod)
#define CUTEST_ushprod        FUNDERSCORE(cutest_cint_ushprod)

#define CUTEST_cfn            FUNDERSCORE(cutest_cfn)
#define CUTEST_cconst         FUNDERSCORE(cutest_cconst)
#define CUTEST_cofg           FUNDERSCORE(cutest_cint_cofg)
#define CUTEST_cofsg          FUNDERSCORE(cutest_cint_cofsg)
#define CUTEST_ccf            FUNDERSCORE(cutest_ccf)
#define CUTEST_ccfg           FUNDERSCORE(cutest_cint_ccfg)
#define CUTEST_clfg           FUNDERSCORE(cutest_cint_clfg)
#define CUTEST_cgr            FUNDERSCORE(cutest_cint_cgr)
#define CUTEST_csgr           FUNDERSCORE(cutest_cint_csgr)
#define CUTEST_csgrp          FUNDERSCORE(cutest_csgrp)
#define CUTEST_csjp           FUNDERSCORE(cutest_csjp)
#define CUTEST_ccfsg          FUNDERSCORE(cutest_cint_ccfsg)
#define CUTEST_ccifg          FUNDERSCORE(cutest_cint_ccifg)
#define CUTEST_ccifsg         FUNDERSCORE(cutest_cint_ccifsg)
#define CUTEST_cgrdh          FUNDERSCORE(cutest_cint_cgrdh)
#define CUTEST_cdh            FUNDERSCORE(cutest_cdh)
#define CUTEST_cdhc           FUNDERSCORE(cutest_cdhc)
#define CUTEST_cdhj           FUNDERSCORE(cutest_cdhj)
#define CUTEST_cshp           FUNDERSCORE(cutest_cshp)
#define CUTEST_csh            FUNDERSCORE(cutest_csh)
#define CUTEST_cshc           FUNDERSCORE(cutest_cshc)
#define CUTEST_cshj           FUNDERSCORE(cutest_cshj)
#define CUTEST_ceh            FUNDERSCORE(cutest_cint_ceh)
#define CUTEST_cifn           FUNDERSCORE(cutest_cifn)
#define CUTEST_cigr           FUNDERSCORE(cutest_cigr)
#define CUTEST_cisgr          FUNDERSCORE(cutest_cisgr)
#define CUTEST_cisgrp         FUNDERSCORE(cutest_cisgrp)
#define CUTEST_cidh           FUNDERSCORE(cutest_cidh)
#define CUTEST_cish           FUNDERSCORE(cutest_cish)
#define CUTEST_csgrsh         FUNDERSCORE(cutest_cint_csgrsh)
#define CUTEST_csgrshp        FUNDERSCORE(cutest_csgrshp)
#define CUTEST_csgreh         FUNDERSCORE(cutest_cint_csgreh)
#define CUTEST_chprod         FUNDERSCORE(cutest_cint_chprod)
#define CUTEST_cshprod        FUNDERSCORE(cutest_cint_cshprod)
#define CUTEST_chcprod        FUNDERSCORE(cutest_cint_chcprod)
#define CUTEST_cshcprod       FUNDERSCORE(cutest_cint_cshcprod)
#define CUTEST_chjprod        FUNDERSCORE(cutest_cint_chjprod)
#define CUTEST_cjprod         FUNDERSCORE(cutest_cint_cjprod)
#define CUTEST_csjprod        FUNDERSCORE(cutest_cint_csjprod)
#define CUTEST_cchprods       FUNDERSCORE(cutest_cint_cchprods)
#define CUTEST_cchprodsp      FUNDERSCORE(cutest_cchprodsp)
#define CUTEST_cohprods       FUNDERSCORE(cutest_cint_cohprods)
#define CUTEST_cohprodsp      FUNDERSCORE(cutest_cohprodsp)

#define CUTEST_uterminate     FUNDERSCORE(cutest_uterminate)
#define CUTEST_cterminate     FUNDERSCORE(cutest_cterminate)

#define FORTRAN_open          FUNDERSCORE(fortran_open)
#define FORTRAN_close         FUNDERSCORE(fortran_close)

/*
 * Prototypes for CUTEst FORTRAN routines found in libcutest.a/libcutest.so
 * See https://github.com/ralna/CUTEst
 */

/*_c Setup routines */
void CUTEST_usetup( integer *status, const integer *funit,
                    const integer *iout, const integer *io_buffer,
                    integer *n, doublereal *x, doublereal *bl,
                    doublereal *bu );
void CUTEST_csetup( integer *status, const integer *funit,
                    const integer *iout,
                    const integer *io_buffer, integer *n, integer *m,
                    doublereal *x, doublereal *bl, doublereal *bu,
                    doublereal *v, doublereal *cl, doublereal *cu,
                    logical *equatn, logical *linear, 
                    const integer *e_order, const integer *l_order, 
                    const integer *v_order );

/* Unconstrained dimensioning and report routines */
void CUTEST_udimen( integer *status, const integer *funit, integer *n );
void CUTEST_udimsh( integer *status, integer *nnzh );
void CUTEST_udimse( integer *status, integer *ne, integer *nzh,
                    integer *nzirnh );
void CUTEST_uvartype( integer *status, const integer *n, integer *ivarty );
void CUTEST_unames( integer *status, const integer *n, char *pname,
                    char *vnames );
void CUTEST_ureport( integer *status, doublereal *calls, doublereal *time );

/* Constrained dimensioning and report routines */
void CUTEST_cdimen( integer *status, const integer *funit, integer *n,
                    integer *m );
void CUTEST_cnoobj( integer *status, const integer *funit, logical *noobj );
void CUTEST_cdimsg( integer *status, integer *nnzg );
void CUTEST_cdimsj( integer *status, integer *nnzj );
void CUTEST_cdimsh( integer *status, integer *nnzh );
void CUTEST_cdimohp( integer *status, integer *nnzohp );
void CUTEST_cdimchp( integer *status, integer *nnzchp );
void CUTEST_cdimse( integer *status, integer *ne, integer *nzh,
                    integer *nzirnh );
void CUTEST_cstats( integer *status, integer *nonlinear_variables_objective,
                    integer *nonlinear_variables_constraints,
                    integer *equality_constraints,
                    integer *linear_constraints );
void CUTEST_cvartype( integer *status, const integer *n, integer *ivarty );
void CUTEST_cnames( integer *status, const integer *n, const integer *m,
                    char *pname, char *vnames, char *gnames );
void CUTEST_creport( integer *status, doublereal *calls, doublereal *time );

void CUTEST_connames( integer *status, const integer *m, char *gname );
void CUTEST_pname( integer *status, const integer *funit, char *pname );
void CUTEST_classification( integer *status, const integer *funit, 
                              char *classification );
void CUTEST_probname( integer *status, char *pname );
void CUTEST_varnames( integer *status, const integer *n, char *vname );

/* Unconstrained optimization routines */
void CUTEST_ufn( integer *status, const integer *n, const doublereal *x,
                 doublereal *f );
void CUTEST_ugr( integer *status, const integer *n, const doublereal *x,
                 doublereal *g );
void CUTEST_uofg( integer *status, const integer *n, const doublereal *x,
                 doublereal *f, doublereal *g, const logical *grad );
void CUTEST_udh( integer *status, const integer *n, const doublereal *x,
                 const integer *lh1, doublereal *h );
void CUTEST_ushp( integer *status, const integer *n, integer *nnzh,
                  const integer *lh, integer *irnh, integer *icnh );
void CUTEST_ush( integer *status, const integer *n, const doublereal *x,
                 integer *nnzh, const integer *lh, doublereal *h,
                 integer *irnh, integer *icnh );
void CUTEST_ueh( integer *status, const integer *n, const doublereal *x,
                 integer *ne, const integer *le, integer *iprnhi,
                 integer *iprhi, const integer *lirnhi, integer *irnhi,
                 const integer *lhi, doublereal *hi,
                 const logical *byrows );
void CUTEST_ugrdh( integer *status, const integer *n, const doublereal *x,
                   doublereal *g, const integer *lh1, doublereal *h);
void CUTEST_ugrsh( integer *status, const integer *n, const doublereal *x,
                   doublereal *g, integer *nnzh, integer *lh, 
                   doublereal *h, integer *irnh, integer *icnh );
void CUTEST_ugreh( integer *status, const integer *n, const doublereal *x,
                   doublereal *g, integer *ne, const integer *le,
                   integer *iprnhi, integer *iprhi, const integer *lirnhi,
                   integer *irnhi, const integer *lhi, doublereal *hi,
                   const logical *byrows );
void CUTEST_uhprod( integer *status, const integer *n, const logical *goth,
                    const doublereal *x, const doublereal *p, 
                    doublereal *r );
void CUTEST_ushprod( integer *status, const integer *n, const logical *goth,
                     const doublereal *x, const integer *nnzp,
                     const integer *indp, const doublereal *p,
                     integer *nnzr, integer *indr, doublereal *r );
void CUTEST_ubandh( integer *status, const integer *n, const doublereal *x,
                    const integer *nsemib, doublereal *bandh,
                    const integer *lbandh, integer *maxsbw );

/* Constrained optimization routines */
void CUTEST_cfn( integer *status,  const integer *n, const integer *m,
                 const doublereal *x, doublereal *f, doublereal *c );
void CUTEST_cconst( integer *status,  const integer *m, doublereal *c );
void CUTEST_cofg( integer *status, const integer *n, const doublereal *x,
                  doublereal *f, doublereal *g, const logical *grad );
void CUTEST_cofsg( integer *status, const integer *n, const doublereal *x,
                   doublereal *f, integer *nnzg, const integer *lg,
                   doublereal *sg, integer *ivsg, const logical *grad );
void CUTEST_ccf( integer *status, const integer *n, const integer *m,
                 const doublereal *x, doublereal *c );
void CUTEST_ccfg( integer *status, const integer *n, const integer *m,
                  const doublereal *x, doublereal *c, 
                  const logical *jtrans, const integer *lcjac1, 
                  const integer *lcjac2, doublereal *cjac, 
                  const logical *grad );
void CUTEST_clfg( integer *status, const integer *n, const integer *m,
                  const doublereal *x, const doublereal *y, doublereal *f,
                  doublereal *g, const logical *grad );
void CUTEST_cgr( integer *status,  const integer *n, const integer *m,
                 const doublereal *x, const doublereal *y,
                 const logical *grlagf, doublereal *g,
                 const logical *jtrans, const integer *lcjac1,
                 const integer *lcjac2, doublereal *cjac );
void CUTEST_csgr( integer *status, const integer *n, const integer *m,
                  const doublereal *x, const doublereal *y,
                  const logical *grlagf, integer *nnzj,
                  const integer *lcjac, doublereal *cjac,
                  integer *indvar, integer *indfun );
void CUTEST_csgrp( integer *status, const integer *n, integer *nnzj,
                   const integer *lj, integer *jvar, integer *jcon );
void CUTEST_csjp( integer *status, integer *nnzj, const integer *lj,
                  integer *jvar, integer *jcon );
void CUTEST_ccfsg( integer *status,  const integer *n, const integer *m,
                   const doublereal *x, doublereal *c, integer *nnzj,
                   const integer *lcjac, doublereal *cjac, integer *indvar,
                   integer *indfun, const logical *grad );
void CUTEST_ccifg( integer *status,  const integer *n, const integer *icon,
                   const doublereal *x, doublereal *ci, doublereal *gci,
                   const logical *grad );
void CUTEST_ccifsg( integer *status, const integer *n, const integer *con,
                    const doublereal *x, doublereal *ci, integer *nnzsgc,
                    const integer *lsgci, doublereal *sgci, integer *ivsgci,
                    const logical *grad );
void CUTEST_cgrdh( integer *status, const integer *n, const integer *m,
                   const doublereal *x, const doublereal *y,
                   const logical *grlagf, doublereal *g,
                   const logical *jtrans, const integer *lcjac1,
                   const integer *lcjac2, doublereal *cjac,
                   const integer *lh1, doublereal *h );
void CUTEST_cdh( integer *status, const integer *n, const integer *m,
                 const doublereal *x, const doublereal *y,
                 const integer *lh1, doublereal *h );
void CUTEST_cdhc( integer *status, const integer *n, const integer *m,
                  const doublereal *x, const doublereal *y,
                  const integer *lh1, doublereal *h );
void CUTEST_cdhj( integer *status, const integer *n, const integer *m,
                  const doublereal *x, const doublereal *y0,
                  const doublereal *y, const integer *lh1,
                  doublereal *h );
void CUTEST_cshp( integer *status, const integer *n, integer *nnzh,
                  const integer *lh, integer *irnh, integer *icnh );
void CUTEST_csh( integer *status, const integer *n, const integer *m,
                 const doublereal *x, const doublereal *y, integer *nnzh,
                 const integer *lh, doublereal *h, integer *irnh,
                 integer *icnh );
void CUTEST_cshc( integer *status, const integer *n, const integer *m,
                  const doublereal *x, const doublereal *y, integer *nnzh,
                  const integer *lh, doublereal *h,
                  integer *irnh, integer *icnh );
void CUTEST_cshj( integer *status, const integer *n, const integer *m,
                  const doublereal *x, const doublereal *y0,
                  const doublereal *y, integer *nnzh,
                  const integer *lh, doublereal *h,
                  integer *irnh, integer *icnh );
void CUTEST_ceh( integer *status, const integer *n, const integer *m,
                 const doublereal *x, const doublereal *y,
                 integer *ne, const integer *le, integer *iprnhi,
                 integer *iprhi, const integer *lirnhi, integer *irnhi,
                 const integer *lhi, doublereal *hi,
                 const logical *byrows );
void CUTEST_cifn( integer *status, const integer *n, const integer *iprob,
                  const doublereal *x, doublereal *f );
void CUTEST_cigr( integer *status, const integer *n, const integer *iprob,
                  const doublereal *x, doublereal *g );
void CUTEST_cisgr( integer *status, const integer *n, const integer *iprob,
                   const doublereal *x, integer *nnzg, const integer *lg,
                   doublereal *sg, integer *ivsg );
void CUTEST_cisgrp( integer *status, const integer *n, const integer *iprob,
                    integer *nnzg, const integer *lg, integer *ivsg );
void CUTEST_cidh( integer *status, const integer *n, const doublereal *x,
                  const integer *iprob, const integer *lh1, 
                  doublereal *h );
void CUTEST_cish( integer *status, const integer *n, const doublereal *x,
                  const integer *iprob, integer *nnzh, const integer *lh,
                  doublereal *h, integer *irnh, integer *icnh );
void CUTEST_csgrsh( integer *status, const integer *n, const integer *m,
                    const doublereal *x, const doublereal *y,
                    const logical *grlagf, integer *nnzj,
                    const integer *lcjac, doublereal *cjac, integer *indvar,
                    integer *indfun, integer *nnzh, const integer *lh,
                    doublereal *h, integer *irnh, integer *icnh );
void CUTEST_csgrshp( integer *status, const integer *n, integer *nnzj,
                     const integer *lcjac, integer *indvar,
                     integer *indfun, integer *nnzh, const integer *lh,
                     integer *irnh, integer *icnh );
void CUTEST_csgreh( integer *status, const integer *n, const integer *m,
                    const doublereal *x, const doublereal *y,
                    const logical *grlagf, integer *nnzj,
                    const integer *lcjac, doublereal *cjac,
                    integer *indvar, integer *indfun,
                    integer *ne, const integer *le, integer *iprnhi,
                    integer *iprhi, const integer *lirnhi,
                    integer *irnhi, const integer *lhi, doublereal *hi,
                    const logical *byrows );
void CUTEST_chprod( integer *status, const integer *n, const integer *m,
                    const logical *goth, const doublereal *x,
                    const doublereal *y, doublereal *p, doublereal *q );
void CUTEST_cshprod( integer *status, const integer *n, const integer *m,
                     const logical *goth, const doublereal *x,
                     const doublereal *y, const integer *nnzp,
                     const integer *indp, const doublereal *p,
                     integer *nnzr, integer *indr, doublereal *r );
void CUTEST_chcprod( integer *status, const integer *n, const integer *m,
                      const logical *goth, const doublereal *x,
                      const doublereal *y, doublereal *p, doublereal *q );
void CUTEST_cshcprod( integer *status, const integer *n, const integer *m,
                      const logical *goth, const doublereal *x,
                      const doublereal *y, integer *nnzp, integer *indp,
                      doublereal *p, integer *nnzr, integer *indr,
                      doublereal *r );
void CUTEST_chjprod( integer *status, const integer *n, const integer *m,
                     const logical *goth, const doublereal *x,
                     const doublereal *y0,
                     const doublereal *y, doublereal *p, doublereal *q );
void CUTEST_cjprod( integer *status, const integer *n, const integer *m,
                    const logical *gotj, const logical *jtrans,
                    const doublereal *x, const doublereal *p,
                    const integer *lp, doublereal *r, const integer *lr );
void CUTEST_csjprod( integer *status, const integer *n, const integer *m,
                     const logical *gotj, const logical *jtrans,
                     const doublereal *x, const integer *nnzp,
                     const integer *indp, const doublereal *p,
                     const integer *lp, integer *nnzr,
                     integer *indr, doublereal *r, const integer *lr );
void CUTEST_cchprods( integer *status, const integer *n, const integer *m,
                      const logical *goth, const doublereal *x,
                      const doublereal *p, const integer *lchp,
                      doublereal *chpval, integer *chpind, integer *chpptr );
void CUTEST_cchprodsp( integer *status, const integer *m,
                      const integer *lchp, integer *chpind, 
                      integer *chpptr );
void CUTEST_cohprods( integer *status, const integer *n,
                       const logical *goth, const doublereal *x,
                       const doublereal *p, integer *nnzohp, 
                       const integer *lohp, doublereal *ohpval, 
                       integer *ohpind );
void CUTEST_cohprodsp( integer *status, integer *nnzohp,
                       const integer *lohp, integer *chpind );

/* Termination routines */
void CUTEST_uterminate( integer *status );
void CUTEST_cterminate( integer *status );

/* FORTRAN auxiliary subroutines to retrieve stream unit numbers */
void FORTRAN_open(  const integer *funit, const char *fname, integer *ierr );
void FORTRAN_close( const integer *funit, integer *ierr );

/*
 * Define shortcuts for the CUTEst library functions,
 * and try to avoid the trailing underscore.
 * single-precision(float) procedures
 */

#define CUTEST_usetup_s         FUNDERSCORE(cutest_usetup_s)
#define CUTEST_csetup_s         FUNDERSCORE(cutest_cint_csetup_s)

#define CUTEST_udimen_s         FUNDERSCORE(cutest_udimen_s)
#define CUTEST_udimsh_s         FUNDERSCORE(cutest_udimsh_s)
#define CUTEST_udimse_s         FUNDERSCORE(cutest_udimse_s)
#define CUTEST_uvartype_s       FUNDERSCORE(cutest_uvartype_s)
#define CUTEST_unames_s         FUNDERSCORE(cutest_cint_unames_s)
#define CUTEST_ureport_s        FUNDERSCORE(cutest_ureport_s)

#define CUTEST_cdimen_s         FUNDERSCORE(cutest_cdimen_s)
#define CUTEST_cnoobj_s         FUNDERSCORE(cutest_cint_cnoobj_s)
#define CUTEST_cdimsg_s         FUNDERSCORE(cutest_cdimsg_s)
#define CUTEST_cdimsj_s         FUNDERSCORE(cutest_cdimsj_s)
#define CUTEST_cdimsh_s         FUNDERSCORE(cutest_cdimsh_s)
#define CUTEST_cdimohp_s        FUNDERSCORE(cutest_cdimohp_s)
#define CUTEST_cdimchp_s        FUNDERSCORE(cutest_cdimchp_s)
#define CUTEST_cdimse_s         FUNDERSCORE(cutest_cdimse_s)
#define CUTEST_cstats_s         FUNDERSCORE(cutest_cstats_s)
#define CUTEST_cvartype_s       FUNDERSCORE(cutest_cvartype_s)
#define CUTEST_cnames_s         FUNDERSCORE(cutest_cint_cnames_s)
#define CUTEST_creport_s        FUNDERSCORE(cutest_creport_s)

#define CUTEST_classification_s FUNDERSCORE(cutest_cint_classification_s)
#define CUTEST_connames_s       FUNDERSCORE(cutest_cint_connames_s)
#define CUTEST_pname_s          FUNDERSCORE(cutest_cint_pname_s)
#define CUTEST_probname_s       FUNDERSCORE(cutest_cint_probname_s)
#define CUTEST_varnames_s       FUNDERSCORE(cutest_cint_varnames_s)

#define CUTEST_ufn_s            FUNDERSCORE(cutest_ufn_s)
#define CUTEST_ugr_s            FUNDERSCORE(cutest_ugr_s)
#define CUTEST_uofg_s           FUNDERSCORE(cutest_cint_uofg_s)
#define CUTEST_ubandh_s         FUNDERSCORE(cutest_ubandh_s)
#define CUTEST_udh_s            FUNDERSCORE(cutest_udh_s)
#define CUTEST_ushp_s           FUNDERSCORE(cutest_ushp_s)
#define CUTEST_ush_s            FUNDERSCORE(cutest_ush_s)
#define CUTEST_ueh_s            FUNDERSCORE(cutest_cint_ueh_s)
#define CUTEST_ugrdh_s          FUNDERSCORE(cutest_ugrdh_s)
#define CUTEST_ugrsh_s          FUNDERSCORE(cutest_ugrsh_s)
#define CUTEST_ugreh_s          FUNDERSCORE(cutest_cint_ugreh_s)
#define CUTEST_uhprod_s         FUNDERSCORE(cutest_cint_uhprod_s)
#define CUTEST_ushprod_s        FUNDERSCORE(cutest_cint_ushprod_s)

#define CUTEST_cfn_s            FUNDERSCORE(cutest_cfn_s)
#define CUTEST_cconst_s         FUNDERSCORE(cutest_cconst_s)
#define CUTEST_cofg_s           FUNDERSCORE(cutest_cint_cofg_s)
#define CUTEST_cofsg_s          FUNDERSCORE(cutest_cint_cofsg_s)
#define CUTEST_ccfg_s           FUNDERSCORE(cutest_cint_ccfg_s)
#define CUTEST_ccf_s            FUNDERSCORE(cutest_ccf_s)
#define CUTEST_clfg_s           FUNDERSCORE(cutest_cint_clfg_s)
#define CUTEST_cgr_s            FUNDERSCORE(cutest_cint_cgr_s)
#define CUTEST_csgr_s           FUNDERSCORE(cutest_cint_csgr_s)
#define CUTEST_csgrp_s          FUNDERSCORE(cutest_csgrp_s)
#define CUTEST_csjp_s           FUNDERSCORE(cutest_csjp_s)
#define CUTEST_ccfsg_s          FUNDERSCORE(cutest_cint_ccfsg_s)
#define CUTEST_ccifg_s          FUNDERSCORE(cutest_cint_ccifg_s)
#define CUTEST_ccifsg_s         FUNDERSCORE(cutest_cint_ccifsg_s)
#define CUTEST_cgrdh_s          FUNDERSCORE(cutest_cint_cgrdh_s)
#define CUTEST_cdh_s            FUNDERSCORE(cutest_cdh_s)
#define CUTEST_cdhc_s           FUNDERSCORE(cutest_cdhc_s)
#define CUTEST_cdhj_s           FUNDERSCORE(cutest_cdhj_s)
#define CUTEST_cshp_s           FUNDERSCORE(cutest_cshp_s)
#define CUTEST_csh_s            FUNDERSCORE(cutest_csh_s)
#define CUTEST_cshc_s           FUNDERSCORE(cutest_cshc_s)
#define CUTEST_cshj_s           FUNDERSCORE(cutest_cshj_s)
#define CUTEST_ceh_s            FUNDERSCORE(cutest_cint_ceh_s)
#define CUTEST_cifn_s           FUNDERSCORE(cutest_cifn_s)
#define CUTEST_cigr_s           FUNDERSCORE(cutest_cigr_s)
#define CUTEST_cisgr_s          FUNDERSCORE(cutest_cisgr_s)
#define CUTEST_cisgrp_s         FUNDERSCORE(cutest_cisgrp_s)
#define CUTEST_cidh_s           FUNDERSCORE(cutest_cidh_s)
#define CUTEST_cish_s           FUNDERSCORE(cutest_cish_s)
#define CUTEST_csgrsh_s         FUNDERSCORE(cutest_cint_csgrsh_s)
#define CUTEST_csgrshp_s        FUNDERSCORE(cutest_csgrshp_s)
#define CUTEST_csgreh_s         FUNDERSCORE(cutest_cint_csgreh_s)
#define CUTEST_chprod_s         FUNDERSCORE(cutest_cint_chprod_s)
#define CUTEST_cshprod_s        FUNDERSCORE(cutest_cint_cshprod_s)
#define CUTEST_chcprod_s        FUNDERSCORE(cutest_cint_chcprod_s)
#define CUTEST_cshcprod_s       FUNDERSCORE(cutest_cint_cshcprod_s)
#define CUTEST_chjprod_s        FUNDERSCORE(cutest_cint_chjprod_s)
#define CUTEST_cjprod_s         FUNDERSCORE(cutest_cint_cjprod_s)
#define CUTEST_csjprod_s        FUNDERSCORE(cutest_cint_csjprod_s)
#define CUTEST_cchprods_s       FUNDERSCORE(cutest_cint_cchprods_s)
#define CUTEST_cchprodsp_s      FUNDERSCORE(cutest_cchprodsp_s)
#define CUTEST_cohprods_s       FUNDERSCORE(cutest_cint_cohprods_s)
#define CUTEST_cohprodsp_s      FUNDERSCORE(cutest_cohprodsp_s)

#define CUTEST_uterminate_s     FUNDERSCORE(cutest_uterminate_s)
#define CUTEST_cterminate_s     FUNDERSCORE(cutest_cterminate_s)

#define FORTRAN_open_s          FUNDERSCORE(fortran_open_s)
#define FORTRAN_close_s         FUNDERSCORE(fortran_close_s)

/*
 * Prototypes for CUTEst FORTRAN routines found in libcutest.a/libcutest.so
 * See https://github.com/ralna/CUTEst
 */

/* Setup routines */
void CUTEST_usetup_s( integer *status, const integer *funit,
                      const integer *iout, const integer *io_buffer,
                      integer *n, real *x, real *bl,
                      real *bu );
void CUTEST_csetup_s( integer *status, const integer *funit,
                      const integer *iout,
                      const integer *io_buffer, integer *n, integer *m,
                      real *x, real *bl, real *bu,
                      real *v, real *cl, real *cu,
                      logical *equatn, logical *linear, 
                      const integer *e_order, const integer *l_order, 
                      const integer *v_order );

/* Unconstrained dimensioning and report routines */
void CUTEST_udimen_s( integer *status, const integer *funit, integer *n );
void CUTEST_udimsh_s( integer *status, integer *nnzh );
void CUTEST_udimse_s( integer *status, integer *ne, integer *nzh,
                      integer *nzirnh );
void CUTEST_uvartype_s( integer *status, const integer *n, integer *ivarty );
void CUTEST_unames_s( integer *status, const integer *n, char *pname,
                      char *vnames );
void CUTEST_ureport_s( integer *status, real *calls, real *time );

/* Constrained dimensioning and report routines */
void CUTEST_cdimen_s( integer *status, const integer *funit, integer *n,
                      integer *m );
void CUTEST_cnoobj_s( integer *status, const integer *funit, 
                      logical *noobj );
void CUTEST_cdimsg_s( integer *status, integer *nnzg );
void CUTEST_cdimsj_s( integer *status, integer *nnzj );
void CUTEST_cdimsh_s( integer *status, integer *nnzh );
void CUTEST_cdimohp_s( integer *status, integer *nnzohp );
void CUTEST_cdimchp_s( integer *status, integer *nnzchp );
void CUTEST_cdimse_s( integer *status, integer *ne, integer *nzh,
                      integer *nzirnh );
void CUTEST_cstats_s( integer *status, 
                      integer *nonlinear_variables_objective,
                      integer *nonlinear_variables_constraints,
                      integer *equality_constraints,
                      integer *linear_constraints );
void CUTEST_cvartype_s( integer *status, const integer *n, integer *ivarty );
void CUTEST_cnames_s( integer *status, const integer *n, const integer *m,
                       char *pname, char *vnames, char *gnames );
void CUTEST_creport_s( integer *status, real *calls, real *time );

void CUTEST_connames_s( integer *status, const integer *m, char *gname );
void CUTEST_pname_s( integer *status, const integer *funit, char *pname );
void CUTEST_classification_s( integer *status, const integer *funit, 
                              char *classification );
void CUTEST_probname_s( integer *status, char *pname );
void CUTEST_varnames_s( integer *status, const integer *n, char *vname );

/* Unconstrained optimization routines */
void CUTEST_ufn_s( integer *status, const integer *n, const real *x,
                   real *f );
void CUTEST_ugr_s( integer *status, const integer *n, const real *x,
                   real *g );
void CUTEST_uofg_s( integer *status, const integer *n, const real *x,
                    real *f, real *g, const logical *grad );
void CUTEST_udh_s( integer *status, const integer *n, const real *x,
                    const integer *lh1, real *h );
void CUTEST_ushp_s( integer *status, const integer *n, integer *nnzh,
                    const integer *lh, integer *irnh, integer *icnh );
void CUTEST_ush_s( integer *status, const integer *n, const real *x,
                   integer *nnzh, const integer *lh, real *h,
                    integer *irnh, integer *icnh );
void CUTEST_ueh_s( integer *status, const integer *n, const real *x,
                   integer *ne, const integer *le, integer *iprnhi,
                   integer *iprhi, const integer *lirnhi, integer *irnhi,
                   const integer *lhi, real *hi,
                   const logical *byrows );
void CUTEST_ugrdh_s( integer *status, const integer *n, const real *x,
                     real *g, const integer *lh1, real *h);
void CUTEST_ugrsh_s( integer *status, const integer *n, const real *x,
                     real *g, integer *nnzh, integer *lh, real *h,
                     integer *irnh, integer *icnh );
void CUTEST_ugreh_s( integer *status, const integer *n, const real *x,
                     real *g, integer *ne, const integer *le,
                     integer *iprnhi, integer *iprhi, 
                     const integer *lirnhi, integer *irnhi, 
                     const integer *lhi, real *hi, 
                     const logical *byrows );
void CUTEST_uhprod_s( integer *status, const integer *n, 
                      const logical *goth, const real *x, const real *p, 
                      real *r );
void CUTEST_ushprod_s( integer *status, const integer *n, 
                       const logical *goth,
                       const real *x, const integer *nnzp,
                       const integer *indp, const real *p,
                       integer *nnzr, integer *indr, real *r );
void CUTEST_ubandh_s( integer *status, const integer *n, const real *x,
                      const integer *nsemib, real *bandh,
                      const integer *lbandh, integer *maxsbw );

/* Constrained optimization routines */
void CUTEST_cfn_s( integer *status,  const integer *n, const integer *m,
                   const real *x, real *f, real *c );
void CUTEST_cconst_s( integer *status,  const integer *m, real *c );
void CUTEST_cofg_s( integer *status, const integer *n, const real *x,
                     real *f, real *g, const logical *grad );
void CUTEST_cofsg_s( integer *status, const integer *n, const real *x,
                     real *f, integer *nnzg, const integer *lg,
                     real *sg, integer *ivsg, const logical *grad );
void CUTEST_ccf_s( integer *status, const integer *n, const integer *m,
                   const real *x, real *c );
void CUTEST_ccfg_s( integer *status, const integer *n, const integer *m,
                    const real *x, real *c, const logical *jtrans,
                    const integer *lcjac1, const integer *lcjac2,
                    real *cjac, const logical *grad );
void CUTEST_clfg_s( integer *status, const integer *n, const integer *m,
                    const real *x, const real *y, real *f,
                    real *g, const logical *grad );
void CUTEST_cgr_s( integer *status,  const integer *n, const integer *m,
                   const real *x, const real *y,
                   const logical *grlagf, real *g,
                   const logical *jtrans, const integer *lcjac1,
                   const integer *lcjac2, real *cjac );
void CUTEST_csgr_s( integer *status, const integer *n, const integer *m,
                    const real *x, const real *y,
                    const logical *grlagf, integer *nnzj,
                    const integer *lcjac, real *cjac,
                    integer *indvar, integer *indfun );
void CUTEST_csgrp_s( integer *status, const integer *n, integer *nnzj,
                          const integer *lj, integer *jvar, integer *jcon );
void CUTEST_csjp_s( integer *status, integer *nnzj, const integer *lj,
                          integer *jvar, integer *jcon );
void CUTEST_ccfsg_s( integer *status,  const integer *n, const integer *m,
                     const real *x, real *c, integer *nnzj,
                     const integer *lcjac, real *cjac, integer *indvar,
                     integer *indfun, const logical *grad );
void CUTEST_ccifg_s( integer *status,  const integer *n, 
                     const integer *icon, const real *x, real *ci, 
                     real *gci, const logical *grad );
void CUTEST_ccifsg_s( integer *status, const integer *n, const integer *con,
                      const real *x, real *ci, integer *nnzsgc,
                      const integer *lsgci, real *sgci, integer *ivsgci,
                      const logical *grad );
void CUTEST_cgrdh_s( integer *status, const integer *n, const integer *m,
                     const real *x, const real *y,
                     const logical *grlagf, real *g,
                     const logical *jtrans, const integer *lcjac1,
                     const integer *lcjac2, real *cjac,
                     const integer *lh1, real *h );
void CUTEST_cdh_s( integer *status, const integer *n, const integer *m,
                   const real *x, const real *y,
                   const integer *lh1, real *h );
void CUTEST_cdhc_s( integer *status, const integer *n, const integer *m,
                    const real *x, const real *y,
                    const integer *lh1, real *h );
void CUTEST_cdhj_s( integer *status, const integer *n, const integer *m,
                    const real *x, const real *y0, const real *y,
                    const integer *lh1, real *h );
void CUTEST_cshp_s( integer *status, const integer *n, integer *nnzh,
                    const integer *lh, integer *irnh, integer *icnh );
void CUTEST_csh_s( integer *status, const integer *n, const integer *m,
                   const real *x, const real *y, integer *nnzh,
                   const integer *lh, real *h, integer *irnh,
                   integer *icnh );
void CUTEST_cshc_s( integer *status, const integer *n, const integer *m,
                    const real *x, const real *y, integer *nnzh,
                    const integer *lh, real *h,
                    integer *irnh, integer *icnh );
void CUTEST_cshj_s( integer *status, const integer *n, const integer *m,
                    const real *x, const real *y0,
                    const real *y, integer *nnzh,
                    const integer *lh, real *h,
                    integer *irnh, integer *icnh );
void CUTEST_ceh_s( integer *status, const integer *n, const integer *m,
                   const real *x, const real *y,
                   integer *ne, const integer *le, integer *iprnhi,
                   integer *iprhi, const integer *lirnhi, integer *irnhi,
                   const integer *lhi, real *hi,
                   const logical *byrows );
void CUTEST_cifn_s( integer *status, const integer *n, 
                     const integer *iprob, const real *x, real *f );
void CUTEST_cigr_s( integer *status, const integer *n, 
                     const integer *iprob, const real *x, real *g );
void CUTEST_cisgr_s( integer *status, const integer *n, 
                     const integer *iprob, const real *x, 
                     integer *nnzg, const integer *lg,
                     real *sg, integer *ivsg );
void CUTEST_cisgrp_s( integer *status, const integer *n, 
                      const integer *iprob, integer *nnzg, 
                      const integer *lg, integer *ivsg );
void CUTEST_cidh_s( integer *status, const integer *n, const real *x,
                    const integer *iprob, const integer *lh1, real *h );
void CUTEST_cish_s( integer *status, const integer *n, const real *x,
                    const integer *iprob, integer *nnzh, 
                    const integer *lh, real *h, integer *irnh, 
                    integer *icnh );
void CUTEST_csgrsh_s( integer *status, const integer *n, const integer *m,
                      const real *x, const real *y,
                      const logical *grlagf, integer *nnzj,
                      const integer *lcjac, real *cjac, integer *indvar,
                      integer *indfun, integer *nnzh, const integer *lh,
                      real *h, integer *irnh, integer *icnh );
void CUTEST_csgrshp_s( integer *status, const integer *n, integer *nnzj,
                       const integer *lcjac, integer *indvar,
                       integer *indfun, integer *nnzh, const integer *lh,
                       integer *irnh, integer *icnh );
void CUTEST_csgreh_s( integer *status, const integer *n, const integer *m,
                      const real *x, const real *y,
                      const logical *grlagf, integer *nnzj,
                      const integer *lcjac, real *cjac,
                      integer *indvar, integer *indfun,
                      integer *ne, const integer *le, integer *iprnhi,
                      integer *iprhi, const integer *lirnhi,
                      integer *irnhi, const integer *lhi, real *hi,
                      const logical *byrows );
void CUTEST_chprod_s( integer *status, const integer *n, const integer *m,
                      const logical *goth, const real *x,
                      const real *y, real *p, real *q );
void CUTEST_cshprod_s( integer *status, const integer *n, const integer *m,
                       const logical *goth, const real *x,
                       const real *y, const integer *nnzp,
                       const integer *indp, const real *p,
                       integer *nnzr, integer *indr, real *r );
void CUTEST_chcprod_s( integer *status, const integer *n, const integer *m,
                       const logical *goth, const real *x,
                       const real *y, real *p, real *q );
void CUTEST_cshcprod_s( integer *status, const integer *n, const integer *m,
                        const logical *goth, const real *x,
                        const real *y, integer *nnzp, integer *indp,
                        real *p, integer *nnzr, integer *indr,
                        real *r );
void CUTEST_chjprod_s( integer *status, const integer *n, const integer *m,
                       const logical *goth, const real *x,
                       const real *y0,
                       const real *y, real *p, real *q );
void CUTEST_cjprod_s( integer *status, const integer *n, const integer *m,
                      const logical *gotj, const logical *jtrans,
                      const real *x, const real *p,
                      const integer *lp, real *r, const integer *lr );
void CUTEST_csjprod_s( integer *status, const integer *n, const integer *m,
                       const logical *gotj, const logical *jtrans,
                       const real *x, const integer *nnzp,
                       const integer *indp, const real *p,
                       const integer *lp, integer *nnzr,
                       integer *indr, real *r, const integer *lr );
void CUTEST_cchprods_s( integer *status, const integer *n, const integer *m,
                        const logical *goth, const real *x,
                        const real *p, const integer *lchp,
                        real *chpval, integer *chpind, integer *chpptr );
void CUTEST_cchprodsp_s( integer *status, const integer *m,
                         const integer *lchp, integer *chpind, 
                         integer *chpptr );
void CUTEST_cohprods_s( integer *status, const integer *n,
                        const logical *goth, const real *x,
                        const real *p, integer *nnzohp, const integer *lohp,
                        real *ohpval, integer *ohpind );
void CUTEST_cohprodsp_s( integer *status, integer *nnzohp,
                          const integer *lohp, integer *chpind );

/* Termination routines */
void CUTEST_uterminate_s( integer *status );
void CUTEST_cterminate_s( integer *status );

/* FORTRAN auxiliary subroutines to retrieve stream unit numbers */
void FORTRAN_open_s( const integer *funit, const char *fname, 
                      integer *ierr );
void FORTRAN_close_s( const integer *funit, integer *ierr );

/*
 * Define shortcuts for the CUTEst library functions,
 * and try to avoid the trailing underscore.
 * quadruple-precision (quad) procedures, if available
 */

#ifdef REAL_128

#define CUTEST_usetup_q     FUNDERSCORE(cutest_usetup_q)
#define CUTEST_csetup_q     FUNDERSCORE(cutest_cint_csetup_q)

#define CUTEST_udimen_q     FUNDERSCORE(cutest_udimen_q)
#define CUTEST_udimsh_q     FUNDERSCORE(cutest_udimsh_q)
#define CUTEST_udimse_q     FUNDERSCORE(cutest_udimse_q)
#define CUTEST_uvartype_q   FUNDERSCORE(cutest_uvartype_q)
#define CUTEST_unames_q     FUNDERSCORE(cutest_cint_unames_q)
#define CUTEST_ureport_q    FUNDERSCORE(cutest_ureport_q)

#define CUTEST_cdimen_q     FUNDERSCORE(cutest_cdimen_q)
#define CUTEST_cnoobj_q     FUNDERSCORE(cutest_cint_cnoobj_q)
#define CUTEST_cdimsg_q     FUNDERSCORE(cutest_cdimsg_q)
#define CUTEST_cdimsj_q     FUNDERSCORE(cutest_cdimsj_q)
#define CUTEST_cdimsh_q     FUNDERSCORE(cutest_cdimsh_q)
#define CUTEST_cdimohp_q    FUNDERSCORE(cutest_cdimohp_q)
#define CUTEST_cdimchp_q    FUNDERSCORE(cutest_cdimchp_q)
#define CUTEST_cdimse_q     FUNDERSCORE(cutest_cdimse_q)
#define CUTEST_cstats_q     FUNDERSCORE(cutest_cstats_q)
#define CUTEST_cvartype_q   FUNDERSCORE(cutest_cvartype_q)
#define CUTEST_cnames_q     FUNDERSCORE(cutest_cint_cnames_q)
#define CUTEST_creport_q    FUNDERSCORE(cutest_creport_q)

#define CUTEST_classification_q FUNDERSCORE(cutest_cint_classification_q)
#define CUTEST_connames_q   FUNDERSCORE(cutest_cint_connames_q)
#define CUTEST_pname_q      FUNDERSCORE(cutest_cint_pname_q)
#define CUTEST_probname_q   FUNDERSCORE(cutest_cint_probname_q)
#define CUTEST_varnames_q   FUNDERSCORE(cutest_cint_varnames_q)

#define CUTEST_ufn_q        FUNDERSCORE(cutest_ufn_q)
#define CUTEST_ugr_q        FUNDERSCORE(cutest_ugr_q)
#define CUTEST_uofg_q       FUNDERSCORE(cutest_cint_uofg_q)
#define CUTEST_ubandh_q     FUNDERSCORE(cutest_ubandh_q)
#define CUTEST_udh_q        FUNDERSCORE(cutest_udh_q)
#define CUTEST_ushp_q       FUNDERSCORE(cutest_ushp_q)
#define CUTEST_ush_q        FUNDERSCORE(cutest_ush_q)
#define CUTEST_ueh_q        FUNDERSCORE(cutest_cint_ueh_q)
#define CUTEST_ugrdh_q      FUNDERSCORE(cutest_ugrdh_q)
#define CUTEST_ugrsh_q      FUNDERSCORE(cutest_ugrsh_q)
#define CUTEST_ugreh_q      FUNDERSCORE(cutest_cint_ugreh_q)
#define CUTEST_uhprod_q     FUNDERSCORE(cutest_cint_uhprod_q)
#define CUTEST_ushprod_q    FUNDERSCORE(cutest_cint_ushprod_q)

#define CUTEST_cfn_q        FUNDERSCORE(cutest_cfn_q)
#define CUTEST_cconst_q     FUNDERSCORE(cutest_cconst_q)
#define CUTEST_cofg_q       FUNDERSCORE(cutest_cint_cofg_q)
#define CUTEST_cofsg_q      FUNDERSCORE(cutest_cint_cofsg_q)
#define CUTEST_ccf_q        FUNDERSCORE(cutest_ccf_q)
#define CUTEST_ccfg_q       FUNDERSCORE(cutest_cint_ccfg_q)
#define CUTEST_clfg_q       FUNDERSCORE(cutest_cint_clfg_q)
#define CUTEST_cgr_q        FUNDERSCORE(cutest_cint_cgr_q)
#define CUTEST_csgr_q       FUNDERSCORE(cutest_cint_csgr_q)
#define CUTEST_csgrp_q      FUNDERSCORE(cutest_csgrp_q)
#define CUTEST_csjp_q       FUNDERSCORE(cutest_csjp_q)
#define CUTEST_ccfsg_q      FUNDERSCORE(cutest_cint_ccfsg_q)
#define CUTEST_ccifg_q      FUNDERSCORE(cutest_cint_ccifg_q)
#define CUTEST_ccifsg_q     FUNDERSCORE(cutest_cint_ccifsg_q)
#define CUTEST_cgrdh_q      FUNDERSCORE(cutest_cint_cgrdh_q)
#define CUTEST_cdh_q        FUNDERSCORE(cutest_cdh_q)
#define CUTEST_cdhc_q       FUNDERSCORE(cutest_cdhc_q)
#define CUTEST_cdhj_q       FUNDERSCORE(cutest_cdhj_q)
#define CUTEST_cshp_q       FUNDERSCORE(cutest_cshp_q)
#define CUTEST_csh_q        FUNDERSCORE(cutest_csh_q)
#define CUTEST_cshc_q       FUNDERSCORE(cutest_cshc_q)
#define CUTEST_cshj_q       FUNDERSCORE(cutest_cshj_q)
#define CUTEST_ceh_q        FUNDERSCORE(cutest_cint_ceh_q)
#define CUTEST_cifn_q       FUNDERSCORE(cutest_cifn_q)
#define CUTEST_cigr_q       FUNDERSCORE(cutest_cigr_q)
#define CUTEST_cisgr_q      FUNDERSCORE(cutest_cisgr_q)
#define CUTEST_cisgrp_q     FUNDERSCORE(cutest_cisgrp_q)
#define CUTEST_cidh_q       FUNDERSCORE(cutest_cidh_q)
#define CUTEST_cish_q       FUNDERSCORE(cutest_cish_q)
#define CUTEST_csgrsh_q     FUNDERSCORE(cutest_cint_csgrsh_q)
#define CUTEST_csgrshp_q    FUNDERSCORE(cutest_csgrshp_q)
#define CUTEST_csgreh_q     FUNDERSCORE(cutest_cint_csgreh_q)
#define CUTEST_chprod_q     FUNDERSCORE(cutest_cint_chprod_q)
#define CUTEST_cshprod_q    FUNDERSCORE(cutest_cint_cshprod_q)
#define CUTEST_chcprod_q    FUNDERSCORE(cutest_cint_chcprod_q)
#define CUTEST_cshcprod_q   FUNDERSCORE(cutest_cint_cshcprod_q)
#define CUTEST_chjprod_q    FUNDERSCORE(cutest_cint_chjprod_q)
#define CUTEST_cjprod_q     FUNDERSCORE(cutest_cint_cjprod_q)
#define CUTEST_csjprod_q    FUNDERSCORE(cutest_cint_csjprod_q)
#define CUTEST_cchprods_q   FUNDERSCORE(cutest_cint_cchprods_q)
#define CUTEST_cchprodsp_q  FUNDERSCORE(cutest_cchprodsp_q)
#define CUTEST_cohprods_q   FUNDERSCORE(cutest_cint_cohprods_q)
#define CUTEST_cohprodsp_q  FUNDERSCORE(cutest_cohprodsp_q)

#define CUTEST_uterminate_q FUNDERSCORE(cutest_uterminate_q)
#define CUTEST_cterminate_q FUNDERSCORE(cutest_cterminate_q)

#define FORTRAN_open_q      FUNDERSCORE(fortran_open_q)
#define FORTRAN_close_q     FUNDERSCORE(fortran_close_q)

/*
 * Prototypes for CUTEst FORTRAN routines found in libcutest.a/libcutest.so
 * See https://github.com/ralna/CUTEst
 */

/* Setup routines */
void CUTEST_usetup_q( integer *status, const integer *funit,
                      const integer *iout, const integer *io_buffer,
                      integer *n, quadreal *x, quadreal *bl,
                      quadreal *bu );
void CUTEST_csetup_q( integer *status, const integer *funit,
                      const integer *iout,
                      const integer *io_buffer, integer *n, integer *m,
                      quadreal *x, quadreal *bl, quadreal *bu,
                      quadreal *v, quadreal *cl, quadreal *cu,
                      logical *equatn, logical *linear, 
                      const integer *e_order, const integer *l_order, 
                      const integer *v_order );

/* Unconstrained dimensioning and report routines */
void CUTEST_udimen_q( integer *status, const integer *funit, integer *n );
void CUTEST_udimsh_q( integer *status, integer *nnzh );
void CUTEST_udimse_q( integer *status, integer *ne, integer *nzh,
                      integer *nzirnh );
void CUTEST_uvartype_q( integer *status, const integer *n, integer *ivarty );
void CUTEST_unames_q( integer *status, const integer *n, char *pname,
                      char *vnames );
void CUTEST_ureport_q( integer *status, quadreal *calls, quadreal *time );

/* Constrained dimensioning and report routines */
void CUTEST_cdimen_q( integer *status, const integer *funit, integer *n,
                      integer *m );
void CUTEST_cnoobj_q( integer *status, const integer *funit, 
                      logical *noobj );
void CUTEST_cdimsg_q( integer *status, integer *nnzg );
void CUTEST_cdimsj_q( integer *status, integer *nnzj );
void CUTEST_cdimsh_q( integer *status, integer *nnzh );
void CUTEST_cdimohp_q( integer *status, integer *nnzohp );
void CUTEST_cdimchp_q( integer *status, integer *nnzchp );
void CUTEST_cdimse_q( integer *status, integer *ne, integer *nzh,
                      integer *nzirnh );
void CUTEST_cstats_q( integer *status, 
                      integer *nonlinear_variables_objective,
                      integer *nonlinear_variables_constraints,
                      integer *equality_constraints,
                      integer *linear_constraints );
void CUTEST_cvartype_q( integer *status, const integer *n, integer *ivarty );
void CUTEST_cnames_q( integer *status, const integer *n, const integer *m,
                      char *pname, char *vnames, char *gnames );
void CUTEST_creport_q( integer *status, quadreal *calls, quadreal *time );

void CUTEST_connames_q( integer *status, const integer *m, char *gname );
void CUTEST_pname_q( integer *status, const integer *funit, char *pname );
void CUTEST_classification_q( integer *status, const integer *funit, 
                              char *classification );
void CUTEST_probname_q( integer *status, char *pname );
void CUTEST_varnames_q( integer *status, const integer *n, char *vname );

/* Unconstrained optimization routines */
void CUTEST_ufn_q( integer *status, const integer *n, const quadreal *x,
                   quadreal *f );
void CUTEST_ugr_q( integer *status, const integer *n, const quadreal *x,
                   quadreal *g );
void CUTEST_uofg_q( integer *status, const integer *n, const quadreal *x,
                   quadreal *f, quadreal *g, const logical *grad );
void CUTEST_udh_q( integer *status, const integer *n, const quadreal *x,
                   const integer *lh1, quadreal *h );
void CUTEST_ushp_q( integer *status, const integer *n, integer *nnzh,
                    const integer *lh, integer *irnh, integer *icnh );
void CUTEST_ush_q( integer *status, const integer *n, const quadreal *x,
                   integer *nnzh, const integer *lh, quadreal *h,
                   integer *irnh, integer *icnh );
void CUTEST_ueh_q( integer *status, const integer *n, const quadreal *x,
                   integer *ne, const integer *le, integer *iprnhi,
                   integer *iprhi, const integer *lirnhi, integer *irnhi,
                   const integer *lhi, quadreal *hi,
                   const logical *byrows );
void CUTEST_ugrdh_q( integer *status, const integer *n, const quadreal *x,
                     quadreal *g, const integer *lh1, quadreal *h);
void CUTEST_ugrsh_q( integer *status, const integer *n, const quadreal *x,
                     quadreal *g, integer *nnzh, integer *lh, quadreal *h,
                     integer *irnh, integer *icnh );
void CUTEST_ugreh_q( integer *status, const integer *n, const quadreal *x,
                     quadreal *g, integer *ne, const integer *le,
                     integer *iprnhi, integer *iprhi, 
                     const integer *lirnhi, integer *irnhi, 
                     const integer *lhi, quadreal *hi,
                     const logical *byrows );
void CUTEST_uhprod_q( integer *status, const integer *n, 
                       const logical *goth, const quadreal *x, 
                       const quadreal *p, quadreal *r );
void CUTEST_ushprod_q( integer *status, const integer *n, 
                       const logical *goth,
                       const quadreal *x, const integer *nnzp,
                       const integer *indp, const quadreal *p,
                       integer *nnzr, integer *indr, quadreal *r );
void CUTEST_ubandh_q( integer *status, const integer *n, const quadreal *x,
                      const integer *nsemib, quadreal *bandh,
                      const integer *lbandh, integer *maxsbw );

/* Constrained optimization routines */
void CUTEST_cfn_q( integer *status,  const integer *n, const integer *m,
                   const quadreal *x, quadreal *f, quadreal *c );
void CUTEST_cconst_q( integer *status,  const integer *m, quadreal *c );
void CUTEST_cofg_q( integer *status, const integer *n, const quadreal *x,
                    quadreal *f, quadreal *g, const logical *grad );
void CUTEST_cofsg_q( integer *status, const integer *n, const quadreal *x,
                     quadreal *f, integer *nnzg, const integer *lg,
                     quadreal *sg, integer *ivsg, const logical *grad );
void CUTEST_ccf_q( integer *status, const integer *n, const integer *m,
                   const quadreal *x, quadreal *c );
void CUTEST_ccfg_q( integer *status, const integer *n, const integer *m,
                    const quadreal *x, quadreal *c, const logical *jtrans,
                    const integer *lcjac1, const integer *lcjac2,
                    quadreal *cjac, const logical *grad );
void CUTEST_clfg_q( integer *status, const integer *n, const integer *m,
                    const quadreal *x, const quadreal *y, quadreal *f,
                    quadreal *g, const logical *grad );
void CUTEST_cgr_q( integer *status,  const integer *n, const integer *m,
                   const quadreal *x, const quadreal *y,
                   const logical *grlagf, quadreal *g,
                   const logical *jtrans, const integer *lcjac1,
                   const integer *lcjac2, quadreal *cjac );
void CUTEST_csgr_q( integer *status, const integer *n, const integer *m,
                    const quadreal *x, const quadreal *y,
                    const logical *grlagf, integer *nnzj,
                    const integer *lcjac, quadreal *cjac,
                    integer *indvar, integer *indfun );
void CUTEST_csgrp_q( integer *status, const integer *n, integer *nnzj,
                     const integer *lj, integer *jvar, integer *jcon );
void CUTEST_csjp_q( integer *status, integer *nnzj, const integer *lj,
                     integer *jvar, integer *jcon );
void CUTEST_ccfsg_q( integer *status,  const integer *n, const integer *m,
                     const quadreal *x, quadreal *c, integer *nnzj,
                     const integer *lcjac, quadreal *cjac, integer *indvar,
                     integer *indfun, const logical *grad );
void CUTEST_ccifg_q( integer *status,  const integer *n, 
                     const integer *icon, const quadreal *x, 
                     quadreal *ci, quadreal *gci, const logical *grad );
void CUTEST_ccifsg_q( integer *status, const integer *n, const integer *con,
                      const quadreal *x, quadreal *ci, integer *nnzsgc,
                      const integer *lsgci, quadreal *sgci, integer *ivsgci,
                      const logical *grad );
void CUTEST_cgrdh_q( integer *status, const integer *n, const integer *m,
                     const quadreal *x, const quadreal *y,
                     const logical *grlagf, quadreal *g,
                     const logical *jtrans, const integer *lcjac1,
                     const integer *lcjac2, quadreal *cjac,
                     const integer *lh1, quadreal *h );
void CUTEST_cdh_q( integer *status, const integer *n, const integer *m,
                   const quadreal *x, const quadreal *y,
                   const integer *lh1, quadreal *h );
void CUTEST_cdhc_q( integer *status, const integer *n, const integer *m,
                    const quadreal *x, const quadreal *y,
                    const integer *lh1, quadreal *h );
void CUTEST_cdhj_q( integer *status, const integer *n, const integer *m,
                    const quadreal *x, const quadreal *y0, 
                    const quadreal *y, const integer *lh1, quadreal *h );
void CUTEST_cshp_q( integer *status, const integer *n, integer *nnzh,
                    const integer *lh, integer *irnh, integer *icnh );
void CUTEST_csh_q( integer *status, const integer *n, const integer *m,
                   const quadreal *x, const quadreal *y, integer *nnzh,
                   const integer *lh, quadreal *h, integer *irnh,
                   integer *icnh );
void CUTEST_cshc_q( integer *status, const integer *n, const integer *m,
                    const quadreal *x, const quadreal *y, integer *nnzh,
                    const integer *lh, quadreal *h,
                    integer *irnh, integer *icnh );
void CUTEST_cshj_q( integer *status, const integer *n, const integer *m,
                    const quadreal *x, const quadreal *y0,
                    const quadreal *y, integer *nnzh,
                    const integer *lh, quadreal *h,
                    integer *irnh, integer *icnh );
void CUTEST_ceh_q( integer *status, const integer *n, const integer *m,
                   const quadreal *x, const quadreal *y,
                   integer *ne, const integer *le, integer *iprnhi,
                   integer *iprhi, const integer *lirnhi, integer *irnhi,
                   const integer *lhi, quadreal *hi,
                   const logical *byrows );
void CUTEST_cifn_q( integer *status, const integer *n, 
                    const integer *iprob,
                    const quadreal *x, quadreal *f );
void CUTEST_cigr_q( integer *status, const integer *n, 
                    const integer *iprob, const quadreal *x, 
                    quadreal *g );
void CUTEST_cisgr_q( integer *status, const integer *n, 
                     const integer *iprob, const quadreal *x, 
                     integer *nnzg, const integer *lg,
                     quadreal *sg, integer *ivsg );
void CUTEST_cisgrp_q( integer *status, const integer *n, 
                      const integer *iprob, integer *nnzg, 
                      const integer *lg, integer *ivsg );
void CUTEST_cidh_q( integer *status, const integer *n, const quadreal *x,
                     const integer *iprob, const integer *lh1, 
                     quadreal *h );
void CUTEST_cish_q( integer *status, const integer *n, const quadreal *x,
                    const integer *iprob, integer *nnzh, 
                    const integer *lh, quadreal *h, 
                    integer *irnh, integer *icnh );
void CUTEST_csgrsh_q( integer *status, const integer *n, const integer *m,
                      const quadreal *x, const quadreal *y,
                      const logical *grlagf, integer *nnzj,
                      const integer *lcjac, quadreal *cjac, integer *indvar,
                      integer *indfun, integer *nnzh, const integer *lh,
                      quadreal *h, integer *irnh, integer *icnh );
void CUTEST_csgrshp_q( integer *status, const integer *n, integer *nnzj,
                       const integer *lcjac, integer *indvar,
                       integer *indfun, integer *nnzh, const integer *lh,
                       integer *irnh, integer *icnh );
void CUTEST_csgreh_q( integer *status, const integer *n, const integer *m,
                      const quadreal *x, const quadreal *y,
                      const logical *grlagf, integer *nnzj,
                      const integer *lcjac, quadreal *cjac,
                      integer *indvar, integer *indfun,
                      integer *ne, const integer *le, integer *iprnhi,
                      integer *iprhi, const integer *lirnhi,
                      integer *irnhi, const integer *lhi, quadreal *hi,
                      const logical *byrows );
void CUTEST_chprod_q( integer *status, const integer *n, const integer *m,
                      const logical *goth, const quadreal *x,
                      const quadreal *y, quadreal *p, quadreal *q );
void CUTEST_cshprod_q( integer *status, const integer *n, const integer *m,
                       const logical *goth, const quadreal *x,
                       const quadreal *y, const integer *nnzp,
                       const integer *indp, const quadreal *p,
                       integer *nnzr, integer *indr, quadreal *r );
void CUTEST_chcprod_q( integer *status, const integer *n, const integer *m,
                       const logical *goth, const quadreal *x,
                       const quadreal *y, quadreal *p, quadreal *q );
void CUTEST_cshcprod_q( integer *status, const integer *n, const integer *m,
                        const logical *goth, const quadreal *x,
                        const quadreal *y, integer *nnzp, integer *indp,
                        quadreal *p, integer *nnzr, integer *indr,
                        quadreal *r );
void CUTEST_chjprod_q( integer *status, const integer *n, const integer *m,
                       const logical *goth, const quadreal *x,
                       const quadreal *y0,
                       const quadreal *y, quadreal *p, quadreal *q );
void CUTEST_cjprod_q( integer *status, const integer *n, const integer *m,
                      const logical *gotj, const logical *jtrans,
                      const quadreal *x, const quadreal *p,
                      const integer *lp, quadreal *r, const integer *lr );
void CUTEST_csjprod_q( integer *status, const integer *n, const integer *m,
                       const logical *gotj, const logical *jtrans,
                       const quadreal *x, const integer *nnzp,
                       const integer *indp, const quadreal *p,
                       const integer *lp, integer *nnzr,
                       integer *indr, quadreal *r, const integer *lr );
void CUTEST_cchprods_q( integer *status, const integer *n, const integer *m,
                        const logical *goth, const quadreal *x,
                        const quadreal *p, const integer *lchp,
                        quadreal *chpval, integer *chpind, integer *chpptr );
void CUTEST_cchprodsp_q( integer *status, const integer *m,
                         const integer *lchp, integer *chpind, 
                         integer *chpptr );
void CUTEST_cohprods_q( integer *status, const integer *n,
                        const logical *goth, const quadreal *x,
                        const quadreal *p, integer *nnzohp, 
                        const integer *lohp, quadreal *ohpval, 
                        integer *ohpind );
void CUTEST_cohprodsp_q( integer *status, integer *nnzohp,
                         const integer *lohp, integer *chpind );

/* Termination routines */
void CUTEST_uterminate_q( integer *status );
void CUTEST_cterminate_q( integer *status );

/* FORTRAN auxiliary subroutines to retrieve stream unit numbers */
void FORTRAN_open_q(  const integer *funit, const char *fname, 
                        integer *ierr );
void FORTRAN_close_q( const integer *funit, integer *ierr );

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
