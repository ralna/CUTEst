       200    NNIMAX = max number of nonlinear iterations 
         1    IJACV  = method for J*v eval. (0 => finite diffs, 1 => analytic)
         0    IKRYSL = Krylov solver: (0 => GMRES 1 => BiCGSTAB, 2 => TFQMR)
        20    KDMAX  = max Krylov subspace dimension when GMRES is used 
         0    IRPRE  = right preconditioning: (0 => no, 1 => yes)
      1000    IKSMAX = max allowable number iterations per call to Krylov solver
         0    IRESUP = residual update for GMRES (0 => combination, 1 => direct)
         2    IFDORD = order of the finite-difference formula (sometimes) used 
        10    IBTMAX = maximum allowable number of backtracks per linesearch
         0    IETA   = flag determining the forcing term eta (0,1,2,3)
         1    IPLVL  = printlevel (0 => none, => 1 it+f, => 2+ increasing detail)
         6    IPUNIT = printout unit number
         0    IPSOL  = print solution to output unit (0 => no, 1 => yes)
    1.0D-6    FTOL   = stopping tolerance on the f-norm
    1.0D-6    STPTOL = stopping tolerance on the steplength

