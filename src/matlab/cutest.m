% CUTEST -
% A grand unified Matlab gateway for the CUTEst tools.
% This interface brings together the unconstrained, constrained,
% dense and sparse versions of the CUTEst tools.
%
% --------------------------------------------------------------------------
% Matlab Tool    CUTEst subrutine(s)  Purpose
% --------------------------------------------------------------------------
% cutest_dims     cdimen              Obtain problem dimensions
% cutest_setup    usetup / csetup     Setup problem data structure
% cutest_obj      uofg / cofg         Evaluate objective function value
%                                     and its gradient if requested
%
% cutest_grad     ugr / cgr           Evaluate objective gradient
%
% cutest_objcons  cfn                 Evaluate objective and constraints
%
% cutest_cons     ccfg / ccifg        Evaluate constraint bodies
%                                     and their gradients if requested.
%                                     Evaluate a single constraint value
%                                     and its gradient if requested
%
% cutest_scons    ccfsg / ccifsg      Evaluate constraint bodies and
%                                     Jacobian in sparse format.
%                                     Evaluate a single constraint value
%                                     and its gradient as a sparse vector
%
% cutest_lag      clfg                Evaluate Lagrangian function value
%                                     and its gradient if requested
%
% cutest_lagjac   cgr                 Evaluate Jacobian and gradient of
%                                     either objective or Lagrangian
%
% cutest_slagjac  csgr                Evaluate Jacobian in sparse format
%                                     and gradient of either objective or
%                                     Lagrangian as a sparse vector
%
% cutest_Jprod    cjprod              Evaluate the matrix-vector product
%                                     between the Jacobian and a vector
%
% cutest_Jtprod   cjprod              Evaluate the matrix-vector product
%                                     between the transpose Jacobian and
%                                     a vector
%
% cutest_hess     udh / cdh           Evaluate the Hessian matrix of the
%                                     Lagrangian, or of the objective if
%                                     the problem is unconstrained
%
% cutest_ihess    udh / cidh          Evaluate the Hessian matrix of the
%                                     i-th problem function (i=0 is the
%                                     objective function), or of the
%                                     objective if problem is unconstrained
%
% cutest_hprod    uprod / cprod       Evaluate the matrix-vector product
%                                     between the Hessian of the
%                                     Lagrangian
%                                     (or the objective if unconstrained)
%                                     and a vector
%
% cutest_gradhess ugrdh / cgrdh       Evaluate the gradient of either the
%                                     objective or the Lagrangian, the
%                                     Jacobian (or its transpose) and the
%                                     Hessian of the Lagrangian in dense
%                                     format
%
% cutest_sphess   ush / csh           Evaluate the Hessian matrix of the
%                                     Lagrangian, or of the objective if
%                                     the problem is unconstrained, in
%                                     sparse format
%
% cutest_isphess  ush / cish          Evaluate the Hessian matrix of the
%                                     i-th problem function (i=0 is the
%                                     objective function), or of the
%                                     objective if problem is
%                                     unconstrained, in sparse format
%
% varnames       varnames             Obtain variable names as strings
%
% connames       cnames               Obtain constraint names as strings
%
% cutest_terminate uterminate /
%                  cterminate         Remove existing internal workspace
% --------------------------------------------------------------------------
%
% In order to unify the tools and be able to use the same Matlab commands on
% both constrained and unconstrained problems, the tool names in this
% interface differ from the those in the old Fortran gateway routine.
%
% This version copyright GALAHAD productions 30/January/2013
