
                      ******************************
		              *                            *
                      *   USING NOMAD WITH CUTEst  *
		              *                            *
                      ******************************

              ( Last modified on 25 Apr 2013 at 19:47:00 )

WHAT IS NOMAD?
--------------

NOMAD is a C++ derivative-free code for constrained and  unconstrained
optimization implementing the mesh-adaptive direct-search framework.
The package was written by Sebastien Le Digabel and Charles Audet.

HOW DO I GET NOMAD?
-------------------

Download NOMAD pre-compiled binaries and source code from

  http://www.gerad.ca/nomad

and follow the installation instructions.

CREATING THE EXECUTABLE FOR NOMAD
---------------------------------

NOMAD comes pre-compiled for your platform but you may also compile it
from source. The instructions packaged with NOMAD instruct you how to
build an executable, which is all that is required.

USING THE NOMAD INTERFACE TOOL
------------------------------

Suppose you wish to solve the problem written in SIF format contained
in the file probname.SIF.

The NOMAD interface tools require two input files:

   probname.SIF  specifies the problem in SIF format
   NOMAD.SPC     sets values for NOMAD run-time parameters

If no NOMAD.SPC file is present in the current directory, the default
version is copied from the $CUTEST/src/nomad directory.
This default file is as follows:

BB_OUTPUT_TYPE     OBJ
X0                 x0.txt
INITIAL_MESH_SIZE  1
MAX_BB_EVAL        100
DISPLAY_STATS      %5dBBE  %8.1eOBJ

The reader is referred to the NOMAD user's manual if he or
she wishes to modify these parameters.

The NOMAD interface works by building a black box representing probname.SIF,
i.e., an executable that, when supplied with an input vector, returns the
objective and constraint values, if any, evaluated at that point. Normally,
the user need not interact directly with the black box but its usage is
described below if need be.

To run with CUTEst, use the runcutest command with the -p nomad option.
See the man page for runcutest for more details of other options.

REFERENCE
---------

See

 http://www.gerad.ca/nomad

for references.


BLACK BOX USAGE
===============

    ./nomad_main [arg]

where arg is one of:

    --nvar    return the number of variables of the problem

    --x0      return the initial guess

    filename  evaluate the objective and constraint valuess,
              if any, at the vector of unknowns specified in the
              file `filename`.

When called with no argument, `nomad_main` returns the objective
value and constraint values, if any, at the initial guess.
