       1.0   typical size for each component of X
       1.0   estimate of scale of minimization function
         1   algorithm to use to solve minimization problem
         0   =0 if minimization function not expensive to evaluate (0/1)
         8   message to inhibit certain automatic checks + output (see code)
        -1   number of good digits in minimization function (-1 = all)
      1000   maximum number of allowable iterations
         1   =0 if analytic gradient not supplied (0/1)
         1   =0 if analytic hessian  not supplied (0/1)
      -1.0   initial trust region radius (-1 = uncmin chooses)
   0.00001   gradient tolerance for convergence
   0.00001   minimum steplength
