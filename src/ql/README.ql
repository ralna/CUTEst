
                    *****************************
      	            *                           *
                    *   USING QL WITH CUTEst    *
                    *                           *
                    *****************************

              ( Last modified on 20 Feb 2013 at 11:50:00 )

WHAT IS QL?
----------

QL is a small-scale convex quadratic programming code.

HOW DO I GET QL?
---------------

QL was written and is maintained by 

Klaus Schittkowski
 Siedlerstr. 3, D-95488 Eckersdorf, Germany
 http://www.klaus-schittkowski.de/home.htm

It is available from 

 http://www.klaus-schittkowski.de/ql.htm


CREATING THE OBJECT FILES FOR QL
--------------------------------

Having obtained the file QL.zip, create the directories, say, ql, 
and set an environment variable $QL to point to this. Now issue the command

  cd $QL ; gfortran -c -O QL.FOR

substituting your favourite fortran compiler, and additional 
optimizations, for gfortran if desired. This will produce the 
object file QL.o in $QL. These may be moved if desired to 
 $CUTEST/objects/(architecture)/(precision)/ 
for the architecture and precsion you intend to use;
binary files here will be chosen in preference to anything in $QL.

QL is only available in double precision.

USING THE QL INTERFACE TOOL
---------------------------

Suppose you wish to solve the problem written in SIF format contained
in the file probname.SIF.

The QL interface tools require two input files:

   probname.SIF   specifies the problem in SIF format
   QL.SPC     sets values for QL run-time parameters

If no QL.SPC file is present in the current directory, the default version 
is copied from the $CUTEST/src/QL directory. This default contains the 
following:

 1         iprint  controls output level (0 = no print)
 1.0d-12   eps     tolerance for the convergence criterion

The reader is referred to the paper quoted below and the code itself if he or 
she wishes to modify these parameters.

To run with CUTEst, use the runcutest command with the -p QL option.
See the man page for runcutest for more details of other options.

REFERENCES
----------

K. Schittkowski, "QL: A Fortran code for convex quadratic programming - 
User's guide, Version 2.11", 
Report, Department of Mathematics, University of Bayreuth (2005)
 http://www.klaus-schittkowski.de/ql_rep.htm


