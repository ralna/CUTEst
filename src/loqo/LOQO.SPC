* Further description of LOQO specifications/options can be found in the 
* LOQO User's Manual, available at www.princeton.edu/~rvdb.

bndpush        1          * Initial value for slacks.
convex         F          * Assert that problem is convex.
dense         -1          * Column density threshold for factorization.
dual           F          * Assert dual ordering for factorization.
epsdiag        1.0e-14    * Min value for diagonals in factorization.
epsnum         0.0        * Min diagonal to declare row dependence.
epssol         1.0e-6     * Min rhs to declare row dependence.
honor_bnds     F          * Honor bounds on the problem variables.
inftol         1.0e-6     * Primal and dual nfeasibility tolerance.
inftol2        1.0e+5     * Tolerance to declare a problem infeasible.
iterlim        500        * Iteration limit.
mindeg         T          * Multiple min degree for symbolic Cholesky.
minlocfil      F          * Min local fill for symbolic Cholesky.
mufactor       0.1        * Scale factor for the barrier parameter.
noreord        F          * Do not reorder cols for symbolic Cholesky.
pred_corr      F          * Compute a corrector direction.
primal         F          * Assert primal ordering for fact.
sigfig         8          * Digits of accuracy in the duality gap.
steplen        0.95       * Steplength reduction factor.
verbose        2          * Output level
