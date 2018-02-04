/*
 * ======================================================================
 *
 * cutest.h
 * Data type definitions, constants definitions and function prototypes
 * to interface the CUTEst testing environment Fortran library with C
 *
 * This header file is built from different sources and different authors
 * have contributed to it. In any case, many thanks to Andreas Waechter.
 *
 * D. Orban. CUTEr version, July 10 2002.
 * Nick Gould, CUTEst evolution, January 4 2013.
 *             Boolean logicals provided, August 21 2013
 *             fortran intent(in) variables defined as const, Dec 2 2015
 * ======================================================================
 */

#include <stdlib.h>
#include <stdbool.h>

#ifndef CUTEST_DOT_H_INCLUDED
#define CUTEST_DOT_H_INCLUDED
#endif

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
typedef _Bool    logical;
/* typedef bool    logical; */
#define FALSE_ (0)     /* Fortran FALSE */
#define TRUE_  (1)     /* Fortran  TRUE */
/* #define max(a,b) ((a)>(b)?(a):(b)) */

#define ZERO     0e0
#define ONE      1e0
#define CUTE_INF 1e20    /* 'infinity' in CUTEst interface */
#define FSTRING_LEN 10   /* Length of Fortran strings     */

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
 *
 */

#define CUTEST_usetup     FUNDERSCORE(cutest_usetup)
#define CUTEST_csetup     FUNDERSCORE(cutest_cint_csetup)

#define CUTEST_udimen     FUNDERSCORE(cutest_udimen)
#define CUTEST_udimsh     FUNDERSCORE(cutest_udimsh)
#define CUTEST_udimse     FUNDERSCORE(cutest_udimse)
#define CUTEST_uvartype   FUNDERSCORE(cutest_uvartype)
#define CUTEST_unames     FUNDERSCORE(cutest_unames)
#define CUTEST_ureport    FUNDERSCORE(cutest_ureport)

#define CUTEST_cdimen     FUNDERSCORE(cutest_cdimen)
#define CUTEST_cdimsj     FUNDERSCORE(cutest_cdimsj)
#define CUTEST_cdimsh     FUNDERSCORE(cutest_cdimsh)
#define CUTEST_cdimchp    FUNDERSCORE(cutest_cdimchp)
#define CUTEST_cdimse     FUNDERSCORE(cutest_cdimse)
#define CUTEST_cdstats    FUNDERSCORE(cutest_cstats)
#define CUTEST_cvartype   FUNDERSCORE(cutest_cvartype)
#define CUTEST_cnames     FUNDERSCORE(cutest_cnames)
#define CUTEST_creport    FUNDERSCORE(cutest_creport)

#define CUTEST_connames   FUNDERSCORE(cutest_connames)
#define CUTEST_pname      FUNDERSCORE(cutest_pname)
#define CUTEST_probname   FUNDERSCORE(cutest_probname)
#define CUTEST_varnames   FUNDERSCORE(cutest_varnames)

#define CUTEST_ufn        FUNDERSCORE(cutest_ufn)
#define CUTEST_ugr        FUNDERSCORE(cutest_ugr)
#define CUTEST_uofg       FUNDERSCORE(cutest_cint_uofg)
#define CUTEST_ubandh     FUNDERSCORE(cutest_ubandh)
#define CUTEST_udh        FUNDERSCORE(cutest_udh)
#define CUTEST_ushp       FUNDERSCORE(cutest_ushp)
#define CUTEST_ush        FUNDERSCORE(cutest_ush)
#define CUTEST_ueh        FUNDERSCORE(cutest_cint_ueh)
#define CUTEST_ugrdh      FUNDERSCORE(cutest_ugrdh)
#define CUTEST_ugrsh      FUNDERSCORE(cutest_ugrsh)
#define CUTEST_ugreh      FUNDERSCORE(cutest_cint_ugreh)
#define CUTEST_uhprod     FUNDERSCORE(cutest_cint_uhprod)
#define CUTEST_ushprod    FUNDERSCORE(cutest_cint_ushprod)

#define CUTEST_cfn        FUNDERSCORE(cutest_cfn)
#define CUTEST_cofg       FUNDERSCORE(cutest_cint_cofg)
#define CUTEST_cofsg      FUNDERSCORE(cutest_cint_cofsg)
#define CUTEST_ccfg       FUNDERSCORE(cutest_cint_ccfg)
#define CUTEST_clfg       FUNDERSCORE(cutest_cint_clfg)
#define CUTEST_cgr        FUNDERSCORE(cutest_cint_cgr)
#define CUTEST_csgr       FUNDERSCORE(cutest_cint_csgr)
#define CUTEST_ccfsg      FUNDERSCORE(cutest_cint_ccfsg)
#define CUTEST_ccifg      FUNDERSCORE(cutest_cint_ccifg)
#define CUTEST_ccifsg     FUNDERSCORE(cutest_cint_ccifsg)
#define CUTEST_cgrdh      FUNDERSCORE(cutest_cint_cgrdh)
#define CUTEST_cdh        FUNDERSCORE(cutest_cdh)
#define CUTEST_cdhc       FUNDERSCORE(cutest_cdhc)
#define CUTEST_cshp       FUNDERSCORE(cutest_cshp)
#define CUTEST_csh        FUNDERSCORE(cutest_csh)
#define CUTEST_cshc       FUNDERSCORE(cutest_cshc)
#define CUTEST_ceh        FUNDERSCORE(cutest_cint_ceh)
#define CUTEST_cifn       FUNDERSCORE(cutest_cifn)
#define CUTEST_cigr       FUNDERSCORE(cutest_cigr)
#define CUTEST_cisgr       FUNDERSCORE(cutest_cisgr)
#define CUTEST_cidh       FUNDERSCORE(cutest_cidh)
#define CUTEST_cish       FUNDERSCORE(cutest_cish)
#define CUTEST_csgrsh     FUNDERSCORE(cutest_cint_csgrsh)
#define CUTEST_csgreh     FUNDERSCORE(cutest_cint_csgreh)
#define CUTEST_chprod     FUNDERSCORE(cutest_cint_chprod)
#define CUTEST_cshprod    FUNDERSCORE(cutest_cint_chsprod)
#define CUTEST_chcprod    FUNDERSCORE(cutest_cint_chcprod)
#define CUTEST_cshcprod   FUNDERSCORE(cutest_cint_cshcprod)
#define CUTEST_cjprod     FUNDERSCORE(cutest_cint_cjprod)
#define CUTEST_csjprod    FUNDERSCORE(cutest_cint_csjprod)
#define CUTEST_cchprods   FUNDERSCORE(cutest_cint_cchprods)

#define CUTEST_uterminate FUNDERSCORE(cutest_uterminate)
#define CUTEST_cterminate FUNDERSCORE(cutest_cterminate)

#define FORTRAN_open      FUNDERSCORE(fortran_open)
#define FORTRAN_close     FUNDERSCORE(fortran_close)

/*
 * Prototypes for CUTEst FORTRAN routines found in libcutest.a
 * See  http://ccpforge.cse.rl.ac.uk/gf/project/cutest/
 */

/* Setup routines */
void CUTEST_usetup  ( integer *status, const integer *funit,
                      const integer *iout, const integer *io_buffer,
                      integer *n, doublereal *x, doublereal *bl,
                      doublereal *bu );
void CUTEST_csetup  ( integer *status, const integer *funit,
                      const integer *iout,
                      const integer *io_buffer, integer *n, integer *m,
        	      doublereal *x, doublereal *bl, doublereal *bu,
                      doublereal *v, doublereal *cl, doublereal *cu,
      	              logical *equatn, logical *linear, const integer *e_order,
                      const integer *l_order, const integer *v_order );

/* Unconstrained dimensioning and report routines */
void CUTEST_udimen  ( integer *status, const integer *funit, integer *n );
void CUTEST_udimsh  ( integer *status, integer *nnzh );
void CUTEST_udimse  ( integer *status, integer *ne, integer *nzh,
                      integer *nzirnh );
void CUTEST_uvartype( integer *status, const integer *n, integer *ivarty );
void CUTEST_unames  ( integer *status, const integer *n, char *pname,
                      char *vnames );
void CUTEST_ureport ( integer *status, doublereal *calls, doublereal *time );

/* Constrained dimensioning and report routines */
void CUTEST_cdimen  ( integer *status, const integer *funit, integer *n,
                      integer *m );
void CUTEST_cdimsj  ( integer *status, integer *nnzj );
void CUTEST_cdimsh  ( integer *status, integer *nnzh );
void CUTEST_cdimchp ( integer *status, integer *nnzchp );
void CUTEST_cdimse  ( integer *status, integer *ne, integer *nzh,
                      integer *nzirnh );
void CUTEST_cstats  ( integer *status, integer *nonlinear_variables_objective,
                      integer *nonlinear_variables_constraints,
                      integer *equality_constraints,
                      integer *linear_constraints );
void CUTEST_cvartype( integer *status, const integer *n, integer *ivarty );
void CUTEST_cnames  ( integer *status, const integer *n, const integer *m,
                      char *pname, char *vnames, char *gnames );
void CUTEST_creport ( integer *status, doublereal *calls, doublereal *time );

void CUTEST_connames( integer *status, const integer *m, char *gname );
void CUTEST_pname   ( integer *status, const integer *funit, char *pname );
void CUTEST_probname( integer *status, char *pname );
void CUTEST_varnames( integer *status, const integer *n, char *vname );

/* Unconstrained optimization routines */
void CUTEST_ufn     ( integer *status, const integer *n, const doublereal *x,
                      doublereal *f );
void CUTEST_ugr     ( integer *status, const integer *n, const doublereal *x,
                      doublereal *g );
void CUTEST_uofg    ( integer *status, const integer *n, const doublereal *x,
                      doublereal *f, doublereal *g, const logical *grad );
void CUTEST_udh     ( integer *status, const integer *n, const doublereal *x,
                      const integer *lh1, doublereal *h );
void CUTEST_ushp    ( integer *status, const integer *n, integer *nnzh,
                      const integer *lh, integer *irnh, integer *icnh );
void CUTEST_ush     ( integer *status, const integer *n, const doublereal *x,
                      integer *nnzh, const integer *lh, doublereal *h,
                      integer *irnh, integer *icnh );
void CUTEST_ueh     ( integer *status, const integer *n, const doublereal *x,
                      integer *ne, const integer *le, integer *iprnhi,
                      integer *iprhi, const integer *lirnhi, integer *irnhi,
                      const integer *lhi, doublereal *hi, logical *byrows );
void CUTEST_ugrdh   ( integer *status, const integer *n, const doublereal *x,
                      doublereal *g, const integer *lh1, doublereal *h);
void CUTEST_ugrsh   ( integer *status, const integer *n, const doublereal *x,
                      doublereal *g, integer *nnzh, integer *lh, doublereal *h,
                      integer *irnh, integer *icnh );
void CUTEST_ugreh   ( integer *status, const integer *n, const doublereal *x,
                      doublereal *g, integer *ne, const integer *le,
                      integer *iprnhi, integer *iprhi, const integer *lirnhi,
                      integer *irnhi, const integer *lhi, doublereal *hi,
                      const logical *byrows );
void CUTEST_uhprod  ( integer *status, const integer *n, const logical *goth,
                      const doublereal *x, const doublereal *p, doublereal *r );
void CUTEST_ushprod ( integer *status, const integer *n, const logical *goth,
                      const doublereal *x, const integer *nnzp,
                      const integer *indp, const doublereal *p,
                      integer *nnzr, integer *indr, doublereal *r );
void CUTEST_ubandh  ( integer *status, const integer *n, const doublereal *x,
                      const integer *nsemib, doublereal *bandh,
                      const integer *lbandh, integer *maxsbw );

/* Constrained optimization routines */
void CUTEST_cfn     ( integer *status,  const integer *n, const integer *m,
                      const doublereal *x, doublereal *f, doublereal *c );
void CUTEST_cofg    ( integer *status, const integer *n, const doublereal *x,
                      doublereal *f, doublereal *g, logical *grad );
void CUTEST_cofsg   ( integer *status, const integer *n, const doublereal *x,
                      doublereal *f, integer *nnzg, const integer *lg,
                      doublereal *sg, integer *ivsg, logical *grad );
void CUTEST_ccfg    ( integer *status, const integer *n, const integer *m,
                      const doublereal *x, doublereal *c, const logical *jtrans,
                      const integer *lcjac1, const integer *lcjac2,
   	              doublereal *cjac, logical *grad );
void CUTEST_clfg    ( integer *status, const integer *n, const integer *m,
                      const doublereal *x, const doublereal *y, doublereal *f,
                      doublereal *g, logical *grad );
void CUTEST_cgr     ( integer *status,  const integer *n, const integer *m,
                      const doublereal *x, const doublereal *y,
                      const logical *grlagf, doublereal *g,
                      const logical *jtrans, const integer *lcjac1,
                      const integer *lcjac2, doublereal *cjac );
void CUTEST_csgr    ( integer *status, const integer *n, const integer *m,
                      const doublereal *x, const doublereal *y,
                      const logical *grlagf, integer *nnzj,
                      const integer *lcjac, doublereal *cjac,
                      integer *indvar, integer *indfun );
void CUTEST_ccfsg   ( integer *status,  const integer *n, const integer *m,
                      const doublereal *x, doublereal *c, integer *nnzj,
                      const integer *lcjac, doublereal *cjac, integer *indvar,
                      integer *indfun, const logical *grad );
void CUTEST_ccifg   ( integer *status,  const integer *n, const integer *icon,
                      const doublereal *x, doublereal *ci, doublereal *gci,
                      const logical *grad );
void CUTEST_ccifsg  ( integer *status, const integer *n, const integer *con,
                      const doublereal *x, doublereal *ci, integer *nnzsgc,
                      const integer *lsgci, doublereal *sgci, integer *ivsgci,
                      const logical *grad );
void CUTEST_cgrdh   ( integer *status, const integer *n, const integer *m,
                      const doublereal *x, const doublereal *y,
                      const logical *grlagf, doublereal *g,
                      const logical *jtrans, const integer *lcjac1,
                      const integer *lcjac2, doublereal *cjac,
                      const integer *lh1, doublereal *h );
void CUTEST_cdh     ( integer *status, const integer *n, const integer *m,
                      const doublereal *x, const doublereal *y,
                      const integer *lh1, doublereal *h );
void CUTEST_cdhc    ( integer *status, const integer *n, const integer *m,
                      const doublereal *x, const doublereal *y,
                      const integer *lh1, doublereal *h );
void CUTEST_cshp    ( integer *status, const integer *n, integer *nnzh,
                      const integer *lh, integer *irnh, integer *icnh );
void CUTEST_csh     ( integer *status, const integer *n, const integer *m,
                      const doublereal *x, const doublereal *y, integer *nnzh,
                      const integer *lh, doublereal *h, integer *irnh,
                      integer *icnh );
void CUTEST_cshc    ( integer *status, const integer *n, const integer *m,
                      const doublereal *x, const doublereal *y, integer *nnzh,
                      const integer *lh, doublereal *h,
                      integer *irnh, integer *icnh );
void CUTEST_ceh     ( integer *status, const integer *n, const integer *m,
                      const doublereal *x, const doublereal *y,
                      integer *ne, const integer *le, integer *iprnhi,
                      integer *iprhi, const integer *lirnhi, integer *irnhi,
                      const integer *lhi, doublereal *hi,
                      const logical *byrows );
void CUTEST_cifn    ( integer *status, const integer *n, const integer *iprob,
                      const doublereal *x, doublereal *f );
void CUTEST_cigr    ( integer *status, const integer *n, const integer *iprob,
                      const doublereal *x, doublereal *g );
void CUTEST_cisgr   ( integer *status, const integer *n, const integer *iprob,
                      const doublereal *x, integer *nnzg, const integer *lg,
                      doublereal *sg, integer *ivsg );
void CUTEST_cidh    ( integer *status, const integer *n, const doublereal *x,
                      const integer *iprob, const integer *lh1, doublereal *h );
void CUTEST_cish    ( integer *status, const integer *n, const doublereal *x,
                      const integer *iprob, integer *nnzh, const integer *lh,
                      doublereal *h, integer *irnh, integer *icnh );
void CUTEST_csgrsh  ( integer *status, const integer *n, const integer *m,
                      const doublereal *x, const doublereal *y,
                      const logical *grlagf, integer *nnzj,
                      const integer *lcjac, doublereal *cjac, integer *indvar,
                      integer *indfun, integer *nnzh, const integer *lh,
                      doublereal *h, integer *irnh, integer *icnh );
void CUTEST_csgreh  ( integer *status, const integer *n, const integer *m,
                      const doublereal *x, const doublereal *y,
                      const logical *grlagf, integer *nnzj,
                      const integer *lcjac, doublereal *cjac,
                      integer *indvar, integer *indfun,
	              integer *ne, const integer *le, integer *iprnhi,
                      integer *iprhi, const integer *lirnhi,
                      integer *irnhi, const integer *lhi, doublereal *hi,
                      const logical *byrows );
void CUTEST_chprod  ( integer *status, const integer *n, const integer *m,
                      const logical *goth, const doublereal *x,
                      const doublereal *y, doublereal *p, doublereal *q );
void CUTEST_cshprod ( integer *status, const integer *n, const integer *m,
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
void CUTEST_cjprod  ( integer *status, const integer *n, const integer *m,
                      const logical *gotj, const logical *jtrans,
                      const doublereal *x, const doublereal *p,
                      const integer *lp, doublereal *r, const integer *lr );
void CUTEST_csjprod ( integer *status, const integer *n, const integer *m,
                      const logical *gotj, const logical *jtrans,
                      const doublereal *x, const integer *nnzp,
                      const integer *indp, const doublereal *p,
                      const integer *lp, integer *nnzr,
                      integer *indr, doublereal *r, const integer *lr );
void CUTEST_cchprods( integer *status, const integer *n, const integer *m,
                      const logical *goth, const doublereal *x,
                      const doublereal *p, const integer *lchp,
                      doublereal *chpval, integer *chpind, integer *chpptr );

/* Termination routines */
void CUTEST_uterminate( integer *status );
void CUTEST_cterminate( integer *status );

/* FORTRAN auxiliary subroutines to retrieve stream unit numbers */
void FORTRAN_open(  const integer *funit, const char *fname, integer *ierr );
void FORTRAN_close( const integer *funit, integer *ierr );

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

