6                     error unit
6                     normal output unit
1                     controls the amount of printing --- set to 0, 1, 2 or 3
4                     method used (1=dogleg, 2=AINT, 3=More-Sorensen, 4=gltr)
1                     model used (1=first order, 2=Newton)
1000                  maxits
2                     TR update strategy
0                     relative trust region? (1=relative)
1.0                   TR scaling parameter
100.0                 initial TR radius
2.0                   radius_increase
0.5                   radius_reduce
1.0E-7                absolute stopping tolerance
1.0E-7                relative stopping tolerance
2.0E-2                hybrid_tol
3                     hybrid_switch_its
T  		      output_progress_vectors
F                     exact second derivatives
44                    summary unit (not + = don't write)
GN_TR2.out            summary file (max 20 chars)
45                    iteration summary (not + = don't write)
GN_TR2_iter.out       iteration summary file (max 20 chars)
