                      *************************
		      *                       *
                      * Using Uno with CUTEst *
		      *                       *
                      *************************

              ( Last modified on 15 May 2026 at 16:10:00 )

WHAT IS Uno?
------------

Uno is a code for solving large-scale nonlinear programming
problems of the form

  minimize           f(x)
  subject to  l_x <=   x  <= u_x
              l_c <= c(x) <= u_c

The code allows one to try variants of a number of existing and new methods
that may be derived from core components. It uses first, and benefits from
second, derivatives of the problem functions. 

The code was written by Charlie Vanaret and Sven Leyffer, with conderable
help from Alexis Montoison, to whom all technical questions should 
be addressed.

HOW DO I GET Uno?
-----------------

See

  https://github.com/cvanaret/Uno

USING Uno WITH CUTEst
---------------------------

Install as per the instructions in

  https://github.com/cvanaret/Uno/blob/main/INSTALL.md

for the architecture and precsion you intend to use.

USING THE Uno INTERFACE TOOL
----------------------------------

Suppose you wish to solve the problem written in SIF format contained
in the file probname.SIF.

The Uno interface tools require two input files:

	probname.SIF   specifies the problem in SIF format
	uno.opt        sets non-default values for Uno parameters

The uno.opt file should be present in the current directory.

To run with CUTEst, use the runcutest command with the -p uno option.
See the man page for runcutest for more details of other options.

THE uno.opt FILE
-----------------

The file uno.opt specifies parameters for algorithm selected, and
has a sequence of lines of the form

	keyword_list

where

	keyword_list = 	keyword   value
               		{keyword_list}

Each keyword must be on a new line. keyword is
one of the following, default values are also given

keyword         default        meaning
-----------------------------------------------------
iprint          1              controls printing

(to be finished)

A default file is provided in the file uno.opt in $CUTEST/src/Uno

REFERENCE
---------

Charlie Vanaret and Sven Leyffer,
Implementing a unified solver for nonlinearly constrained optimization,
to appear Mathematical Programming Computation (2026)
