/*********************************************************************/
/***    Copyright (c) Robert J. Vanderbei, 1994                    ***/
/***    All Rights Reserved                                        ***/
/*********************************************************************/

#include <stdio.h>
/* #include <sigfpe.h> */

#define	TRUE 1
#define	FALSE 0

#define LP 0
#define QP 1

#define	HEADER 0
#define	NAME   1
#define	ROWS   2
#define	COLS   3
#define	RHS    4
#define	RNGS   5
#define	BNDS   6
#define	QUADS  7
#define	END    8

#define	UNSET  0
#define	PRIMAL 1
#define	DUAL   2

#define	MAX(x,y)  ((x) > (y) ? (x) : (y))
#define	MIN(x,y)  ((x) > (y) ? (y) : (x))
#define	ABS(x)	  ((x) > 0   ? (x) : -(x))
#define	SGN(x)	  ((x) > 0   ? (1.0) : (-1.0))

#define FINITE   0x1
#define INFINITE 0x2
#define UNCONST  0x4
#define FROW     0x8
#define FREEVAR   0x1
#define BDD_BELOW 0x2
#define BDD_ABOVE 0x4
#define BOUNDED   0x8

#define	LP_OPEN_MAX 20 /* max #	lp problems open at once */

typedef	struct loqo {
	int m;		/* number of rows */
	int n;		/* number of columns */
	int nz;		/* number of nonzeros */
	double *A;	/* pointer to array of nonzero values in A */
	int *iA;	/* pointer to array of corresponding row indices */
	int *kA;	/* pointer to array of indices into A (and iA)
				indicating where each new column of A begins */
	double *b;	/* pointer to array containing right-hand side */
	double *c;	/* pointer to array containing objective function */
	double f;	/* fixed adjustment to objective function */
	double *r;	/* pointer to array containing range vector */
	double *l;	/* pointer to array containing lower bounds */
	double *u;	/* pointer to array containing upper bounds */
	int *varsgn;	/* array indicating which variables were declared to
				be non-positive	*/
	char **rowlab;	/* array of strings containing row labels */
	char **collab;	/* array of strings containing column labels */

	int qnz;	/* number of nonzeros in lower triangle	of Q */
	double *Q;	/* pointer to array of nonzero values of Q */
	int *iQ;	/* pointer to array of corresponding row indices */
	int *kQ;	/* pointer to array of indices into Q (and iQ)
				indicating where each new column of Q begins */

	double *At;	/* pointer to array of nonzero values in At */
	int *iAt;	/* pointer to array of corresponding row indices */
	int *kAt;	/* pointer to array of indices into At (and iAt) */

	int *bndmark;	/* pointer to array of bound marks */
	int *rngmark;	/* pointer to array of range marks */

	double *w;	/* pointer to array containing primal surpluses	*/
	double *x;	/* pointer to array containing primal solution */
	double *y;	/* pointer to array containing dual solution */
	double *z;	/* pointer to array containing dual slacks */
	double *p;	/* pointer to array containing range slacks */
	double *q;	/* pointer to array containing dual range slacks */
	double *s;	/* pointer to array containing dual for	ub slacks */
	double *t;	/* pointer to array containing upper bound slacks */
	double *v;	/* pointer to array containing dual for	range (w) */
	double *g;	/* pointer to array containing lower bound slacks */

	double *dw;	/* step direction for primal surpluses	*/
	double *dx;	/* step direction for primal solution */
	double *dy;	/* step direction for dual solution */
	double *dz;	/* step direction for dual slacks */
	double *dp;	/* step direction for range slacks */
	double *dq;	/* step direction for dual range slacks */
	double *ds;	/* step direction for dual for	ub slacks */
	double *dt;	/* step direction for upper bound slacks */
	double *dv;	/* step direction for dual for	range (w) */
	double *dg;	/* step direction for lower bound slacks */

	double *ub;	/* pointer to array containing shifted upper bounds */

	int max;	/* max = -1, min = 1 */
	double inftol;	/* infeasibility tolerance */
	double inftol2;	/* infeasibility for stopping rule */
	double steplen;	/* step length relative to nearest face */
	double mufactor;/* factor for centrality parameter */
	int pred_corr;  /* if 0, predictor only, else predictor-corrector */
	int sf_req;	/* significant figures requested */
	int itnlim;	/* iteration limit */
	double timlim;	/* time limit */
	int verbose;	/* level of verbosity */
	double epssol;	/* epsilon tolerance in f/b solve */
	double epsnum;	/* epsilon tolerance in num fact */
	double epscdn;	/* epsilon tolerance for conditioning */
	double epsdiag; /* epsilon for diagonal perturbation */
	double stablty;	/* mixing factor for stability */
	int method;	/* reordering method */
	int dense;	/* threshold for dense columns/rows */
	int pdf;	/* order to favor primal (ADA^T) or dual (A^TDA) */
	double bndpush; /* initial distance from bounds */
	int honor_bnds; /* honor variable bounds */
	int honor_bnds_init;/* honor variable bounds initially */
	int convex;	/* assert problem is convex */
	int quadratic;	/* assert problem is quadratic */
	int lincons;	/* assert problem has only linear constraints */
	int sdp;	/* 1 if asserting problem is an SDP, 0 otherwise */
	char name[15];	/* string containing problem name */
	char obj[11];	/* string containing objective function	name */
	char rhs[11];	/* string containing right-hand	side name */
	char ranges[11];/* string containing range set name */
	char bounds[11];/* string containing bound set name */

	int *tier;	/* tier for factorization priorities */

	char **param;	/* array of strings containing user parameters */
	int np;		/* number of user parameters */

	int  (*stopping_rule)(void *);/* pointer to stopping	rule fcn */
	void (*init_vars)(void *);    /* pointer to initialization fcn */

	void (*h_init)(void *);       /* pointer to initialization hook fcn */
	int  (*h_update)(void *,int);     /* pointer to f,g,h update hook fcn */
	int  (*h_update_fg)(void *);  /* pointer to f,g update hook fcn */
	int  (*h_update_h )(void *);  /* pointer to h update hook fcn */
	void (*h_close)(void *);      /* pointer to update hook fcn */
	void (*h_step)(void *);       /* pointer to step hook fcn */

	double  (*objval)       ( double * );
	void    (*objgrad)      ( double *, double * );
	void    (*hessian)      ( double *, double *, double * );
	void    (*conval)       ( double *, double * );
	void    (*congrad)      ( double *, double *, double * );

	int    iter;	    /* current iteration number	*/
	double elaptime;    /* elapsed time */
	double pres;	    /* primal residual (i.e. infeasibility) */
	double dres;	    /* dual   residual (i.e. infeasibility) */
	int    sigfig;	    /* significant figures */
	double primal_obj;  /* primal objective	value */
	double dual_obj;    /* dual   objective	value */
	double *R;	    /* array of constraint function values */
	double *b0;	    /* original rhs */

	double pertval;
	double pertval2;

        int    sdpnum;	    /* number of SDP blocks */
        int    *sdpblock;   /* sizes of SDP blocks */

	int    ncomp;       /* number of complementarity constraints */
	int    nmcomp;      /* number of mixed complementarity constraints */
	int    *comp;       /* the ith additional variable is
			       complementary to the original variable comp[i] */
        int    *comptype;   /* type of complementarity constraint:  there are 6
                               l1 <= g(x) <= u1 complements l2 <= y <= u2
                               with exactly 2 of l1, u1, l2, u2 finite
                               constraints with (l1,u1), (l1,l2), (l1,u2), (u1,l2),
                               (u1,u2), and (l2,u2) */

    int		perturb;

    double	penalty_param;

} LOQO;

LOQO	*openlp(void);

void	readlp(
	int	argc,
	char	*argv[],
	LOQO	*lp
);

int prep (LOQO *lp);
int unprep (LOQO *lp);

int	solvelp(
	LOQO	*lp
);

void	closelp(
	LOQO	*lp
);

int	getparam(
	char *label0
);

void	my_exit(
	int num,
	char *str
);

extern void message();

double sdotprod(
	double	*a,
	int	*ja,
	double  *x,
	int	na
);

double dotprod(		/* inner product between n-vectors x and y */
	double	*x,
	double	*y,
	int	n
);

double dotprodm(	/* inner product between n-vectors x and y */
	double	*x,
	double	*y,
	int	n,
	int	*mask,
	int	val
);

double compl(	/* mu-complementarity between n-vectors x and y */
	double  mu,
	double	*x,
	double	*y,
	int	n,
	int	*mask,
	int	val
);

double mincompl(	/* min-complementarity between n-vectors x and y */
	double  min,
	double	*x,
	double	*y,
	int	n,
	int     *mask,
	int	val
);

void smx(		/* y = sparse matrix (A,kA,iA) times x */
	int	m,
	int	n,
	double	*A,
	int	*kA,
	int	*iA,
	double	*x,
	double	*y
);

void smtx(		/* y = x times sparse matrix (A,kA,iA) */
	int	n,
	double	*A,
	int	*kA,
	int	*iA,
	double	*x,
	double	*y
);

void atnum(		/* (kAt,iAt,At)	= transpose of (kA,iA,A) */
	int	m,
	int	n,
	int	*kA,
	int	*iA,
	double	*A,
	int	*kAt,
	int	*iAt,
	double	*At
);

double maxv(		/* compute componentwise maximum of n-vector x */
	double *x,
	int	n
);

void	writelp(
	LOQO	*lp,
	char	*fname
);

void	writesol(
	LOQO	*lp,
	char	fname[]
);

void inv_num(
	LOQO 	*lp,	/* pointer to the linear program's data */
        double  *dn,    /* diagonal matrix for upper-left  corner */
        double  *dm    /* diagonal matrix for lower-right corner */
/*	int	SD */
);

int solve(
	LOQO 	*lp,
	double	*Dn,
	double	*Dm,
        double  *c,
        double  *b
);

int getpertflag(void);

double getpertval(
    int m,
    int n
);

int deflt_st_rule(
    void *
);

void deflt_init(
    void *
);

void deflt_hook(
    void *
);

void inv_clo(void);

char *my_strdup(
	char	*s1
);

double cputimer(
);

void nl_init_mps(
    void *vlp
);

int  nl_update_mps(
    void *vlp
);

int  nl_update_dummy(
    void *vlp,
    int valswitch
);

void nl_close_mps(
    void *lp
);

void nlsetup(
    void *vlp
);

void nlobjterm(
        void (*func)( double *z, double *param,
		      double *pval, double *grad, double **hessian),
			   /* function that computes val, grad, hessian at z */
        int k,             /* number of arguments for func() */
        char **collabs,    /* col labels for arugments to func() */
	int np,	           /* number of parameters in param array */
	double *param	   /* parameters to pass to func() */
);

void nlconstr(
        void (*func)( double *z, double *param,
		      double *pval, double *grad, double **hessian),
			   /* function that computes val, grad, hessian at z */
	char *rowlab,      /* row label for constraint */
        int k,             /* number of arguments for func() */
        char **collabs,    /* col labels for arugments to func() */
	int np,            /* number of parameters in param */
	double *param      /* parameters passed to func() */
);

int rd_specs(char *Spec_name);
void *binsearch(char **sp);
void set_opns(LOQO *lp);

double objval_dummy( double *x );
void objgrad_dummy( double *c, double *x );
void hessian_dummy( double *Q, double *x, double *y );
void conval_dummy( double *h, double *x );
void congrad_dummy( double *A, double *At, double *x );
