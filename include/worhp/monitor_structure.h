#pragma once

// Struct for communication with WorhpMonitor
struct MonitorStructure {
	int iteration_index;
	int refinement_index;
	int qp_count;
	double obj;
	double con;
	double kkt;
	double alpha;
	double dx;
	double time;
	char flags[3];
	char line[500];
};
