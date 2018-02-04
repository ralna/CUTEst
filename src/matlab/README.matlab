A grand unified Matlab gateway for the CUTEst tools.
This interface brings together the unconstrained, constrained,
dense and sparse versions of the CUTEst tools.

In order to unify the tools and be able to use the same Matlab commands on
both constrained and unconstrained problems, the tool names in this
interface differ from the those in the old Fortran gateway routine.


Getting started
---------------

Make sure you have correctly set the following environment variables:

  MYMATLAB      directory containing Matlab. Matlab's mex executable
                should be found under $MYMATLAB/bin.
  MYMATLABARCH  CUTEst architecture built with Matlab support.


Let's try and decode a problem and load it up in Matlab:

    $ cutest2matlab LUBRIFC    # This is the shell prompt
    $ matlab &
    >> prob = cutest_setup()   % This is the Matlab prompt

    prob =

         n: 3751                 % number of variables
         m: 2500                 % number of constraints
      nnzh: 8749                 % number of nonzeros in Hessian of Lagrangian
      nnzj: 1572494              % number of nonzeros in constraint Jacobian
         x: [3751x1 double]      % initial guess
        bl: [3751x1 double]      % lower bounds on variables
        bu: [3751x1 double]      % upper bounds on variables
         v: [2500x1 double]      % initial Lagrange multipliers
        cl: [2500x1 double]      % left-hand side of constraints
        cu: [2500x1 double]      % right-hand side of constraints
    equatn: [2500x1 logical]     % array indicating equality constraints
    linear: [2500x1 logical]     % array indicating linear constraints
      name: 'LUBRIFC   '         % problem name

    >> [f,g] = cutest_obj(prob.x);


If the above does not work as expected for you, please e-mail
dominique.orban@gerad.ca with a description of the commands you run, the
results you get and error messages, if any.


Decoding a problem from inside Matlab
-------------------------------------

At the Matlab prompt, type

    >> setenv('DYLD_LIBRARY_PATH', '/usr/local/bin:/opt/local/lib:')
    >> !sifdecoder LUBRIFC

The problem should be decoded in the current directory. More programmatically,
you may use constructions such as

    >> probname='LUBRIFC';
    >> cmd = ['sifdecoder ', probname];  % Note the trailing blank.
    >> unix(cmd);

In order to create a MEX file, you can proceed similarly

    >> cmd=['runcutest -p matlab -D ', probname];
    >> unix(cmd);


Troubleshooting
---------------

If Matlab complains that

    Symbol not found: __gfortran_transfer_array_write
      Referenced from: /private/tmp/cutest-matlab/mcutest.mexmaci64
      Expected in: /Applications/Matlab/MATLAB_R2015b.app/sys/os/maci64/libgfortran.3.dylib

(the paths may look different on your system) the solution consists in editing `.matlab7rc.sh`. On OSX, this file is found in /Applications/Matlab/MATLAB_R2015b.app/bin. Open the file and search for your MEX architecture (e.g., `maci64`) and look for the line

    LDPATH_PREFIX=''

Insert the path where your `libgfortran` is found, e.g.,

    LDPATH_PREFIX='/usr/local/lib/gcc/5'

and restart Matlab.


Reference
---------

The table below may be obtained with the command 'help cutest' at the
Matlab prompt.

--------------------------------------------------------------------------
Matlab Tool  CUTEst library function(s)  Purpose
--------------------------------------------------------------------------
dims         cdimen                      Obtain problem dimensions
setup        usetup / csetup             Setup problem data structure
obj          uofg / cofg                 Evaluate objective function value
                                         and its gradient if requested

grad         ugr / cgr                   Evaluate the objective gradient

objcons      cfn                         Evaluate objective and constraints

cons         ccfg / ccifg                Evaluate constraint bodies
                                          and their gradients if requested.
                                         Evaluate a single constraint value
                                          and its gradient if requested

scons        ccfsg / ccifsg              Evaluate constraint bodies and
                                          Jacobian in sparse format.
                                         Evaluate a single constraint value
                                          and its gradient as a sparse vector

lag          clfg                        Evaluate Lagrangian function value
                                         and its gradient if requested

lagjac       cgr                         Evaluate Jacobian and gradient of
                                          either objective or Lagrangian

slagjac      csgr                        Evaluate Jacobian in sparse format
                                          and gradient of either objective or
                                          Lagrangian as a sparse vector

Jprod        cjprod                      Evaluate the matrix-vector product
                                          between the Jacobian and a vector

Jtprod       cjprod                      Evaluate the matrix-vector product
                                          between the transpose Jacobian and
                                          a vector

hess         udh / cdh                   Evaluate the Hessian matrix of the
                                          Lagrangian, or of the objective if
                                          the problem is unconstrained

ihess        udh / cidh                  Evaluate the Hessian matrix of the
                                          i-th problem function (i=0 is the
                                          objective function), or of the
                                          objective if problem is unconstrained

hprod        uprod / cprod               Evaluate the matrix-vector product
                                          between the Hessian of the
                                          Lagrangian
                                          (or the objective if unconstrained)
                                          and a vector

gradhess      ugrdh / cgrdh              Evaluate the gradient of either the
                                          objective or the Lagrangian, the
                                          Jacobian (or its transpose) and the
                                          Hessian of the Lagrangian in dense
                                          format

sphess       ush / csh                   Evaluate the Hessian matrix of the
                                          Lagrangian, or of the objective if
                                          the problem is unconstrained, in
                                          sparse format

isphess      ush / cish                  Evaluate the Hessian matrix of the
                                          i-th problem function (i=0 is the
                                          objective function), or of the
                                          objective if problem is
                                          unconstrained, in sparse format

varnames     varnames                    Obtain variable names as strings

connames     cnames                      Obtain constraint names as strings

terminate    uterminate / cterminate     Remove existing internal workspace
--------------------------------------------------------------------------

* To create the Matlab/CUTEst interface, use the cutest2matlab command or,
  for more control, the runcutest command with the  -p matlab option.
  This will create a binary file mcutest.mexglx (32bit Linux),
  mcutest.mexa64 (64bit Linux) mcutest.mexmaci (32bit OSX) or
  mcutest.mexmaci64 (64bit OSX).

* Both this binary and the Matlab .m files in $CUTEST/src/matlab must be
  placed on the Matlab search path.

* See the man page for runcutest for more details of other options, and the
  Matlab help files for the tools mentioned above.

CUTEr version:
 D. Orban, Montreal, January 2007
CUTEst version additions:
 Nick Gould, January 2013
