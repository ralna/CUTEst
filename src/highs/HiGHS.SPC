# Options file for HiGHS
# Comments start with a # on the first entry of a line.
# Each option must be specified on a new line
# Empty lines are ignored.

 presolve = "choose"
 solver = "choose"
 parallel = "choose"
 run_crossover = "on"
 time_limit = 1e35
 infinite_cost = 1e20
 infinite_bound = 1e20
 small_matrix_value = 1e-9
 large_matrix_value = 1e15
 kkt_tolerance = 1e-7
 primal_feasibility_tolerance = 1e-7
 dual_feasibility_tolerance = 1e-7
 objective_bound = 1e35
 objective_target = -1e35
 random_seed = 0
 highs_debug_level = 0
 highs_analysis_level = 0
 qp_iteration_limit = 2147483647
 qp_nullspace_limit = 4000
 qp_regularization_value = 1e-7
 simplex_strategy = 1
 simplex_scale_strategy = 2
 simplex_crash_strategy = 0
 simplex_dual_edge_weight_strategy = -1
 simplex_primal_edge_weight_strategy = -1
 simplex_iteration_limit = 2147483647
 simplex_update_limit = 5000
 threads = 0
 output_flag = true
 write_solution_to_file = false
 solution_file = "HiGHS.sol"
 write_solution_style = 0
 log_to_console = true
 log_file = "HiGHS.log"
 print_full_solution = false
