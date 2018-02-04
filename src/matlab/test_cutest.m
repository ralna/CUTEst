% Test script to exercise the Matlab/CUTEst interface.
% D. Orban, 2011.

clear all;

fprintf('Reading problem dimensions...\n');
[nvar,ncon] = cutest_dims();

fprintf('Initializing problem...\n');
prob = cutest_setup();
assert(nvar == prob.n);
assert(ncon == prob.m);

fprintf('Reading variable names...\n');
vnames = cutest_varnames();

fprintf('Evaluating objective function...\n');
f = cutest_obj(prob.x);

fprintf('Evaluating objective and gradient...\n');
[f,g] = cutest_obj(prob.x);

if prob.m > 0
  fprintf('Evaluating objective function...\n');
  f = cutest_sobj(prob.x);

  fprintf('Evaluating objective and sparse gradient...\n');
  [f,g] = cutest_sobj(prob.x);

    fprintf('Evaluating objective gradient...\n');
    g = cutest_grad(prob.x);

    fprintf('Evaluating a constraint gradient...\n');
    g = cutest_grad(prob.x,1);

    fprintf('Evaluating a sparse constraint gradient...\n');
    sg = cutest_sgrad(prob.x,1);

    fprintf('Reading constraint names...\n');
    cnames = cutest_connames();

    fprintf('Evaluating objective and constraints...\n');
    [f,c] = cutest_objcons(prob.x);

    fprintf('Evaluating Lagrangian value and gradient...\n');
    [f,g] = cutest_lag(prob.x,prob.v);

    fprintf('Evaluating dense Hessian of Lagrangian...\n');
    H = cutest_hess(prob.x,prob.v);

    fprintf('Evaluating gradient and Hessian of Lagrangian\n');
    fprintf('  and Jacobian of constraints...\n');
    [g,J,H] = cutest_gradhess(prob.x,prob.v,false,false);

    fprintf('Evaluating gradient and Hessian of Lagrangian\n');
    fprintf('  and transpose Jacobian of constraints...\n');
    [g,Jt,H] = cutest_gradhess(prob.x,prob.v,false,true);

    fprintf('Computing Hessian-vector product...\n');
    p = rand(prob.n,1);
    Hp = cutest_hprod(p);  % Assume Hessian was evaluated earlier.
    fprintf('  error = %7.1e\n', norm(Hp-H*p)/(1+norm(Hp)));

    fprintf('Computing dense Hessian of first constraint...\n');
    H1 = cutest_ihess(prob.x,1);

    %fprintf('Computing sparse Hessian of first constraint...\n');
    %H1sp = cutest_isphess(prob.x,1);

    fprintf('Computing Jacobian-vector product...\n');
    Jp = cutest_jprod(p);  % Assume J was evaluated earlier.
    fprintf('  error = %7.1e\n', norm(Jp-J*p)/(1+norm(Jp)));

    fprintf('Computing transpose Jacobian-vector product...\n');
    q = rand(prob.m,1);
    Jtq = cutest_jtprod(q);  % Assume J was evaluated earlier.
    fprintf('  error = %7.1e\n', norm(Jtq-Jt*q)/(1+norm(Jtq)));

    fprintf('Computing gradient of Lagrangian and dense constraint Jacobian...\n');
    [g,J] = cutest_lagjac(prob.x,prob.v);

    fprintf('Computing constraints and sparse Jacobian...\n');
    [c,J] = cutest_scons(prob.x);

    fprintf('Computing first constraint body and sparse gradient...\n');
    [c1,g1] = cutest_scons(prob.x,1);

    %fprintf('Computing gradient of Lagrangian and sparse constraint Jacobian...\n');
    %[sg,sJ] = cutest_slagjac(prob.x,prob.v);

    fprintf('Computing sparse Lagrangian Hessian...\n');
    Hsp = cutest_sphess(prob.x,prob.v);

else

    fprintf('Evaluating objective gradient...\n');
    g = cutest_grad(prob.x);

    fprintf('Evaluating dense objective Hessian...\n');
    H = cutest_hess(prob.x);

    fprintf('Evaluating objective gradient and dense Hessian...\n');
    [g,H2] = cutest_gradhess(prob.x);

    fprintf('  error = %7.1e\n', norm(H-H2)/(1+norm(H)));

    fprintf('Computing Hessian-vector product...\n');
    p = rand(prob.n,1);
    Hp = cutest_hprod(p);  % Assume Hessian was evaluated earlier.
    fprintf('  error = %7.1e\n', norm(Hp-H*p)/(1+norm(Hp)));

    fprintf('Computing sparse objective Hessian...\n');
    Hsp = cutest_sphess(prob.x);

    fprintf('  error = %7.1e\n', norm(H-full(Hsp))/(1+norm(H)));
end
fprintf('Terminating run...\n');
cutest_terminate( )
fprintf('Tests completed\n' );
clear all
