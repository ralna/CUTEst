#pragma once

#include <iostream>
#include <vector>
#include <string>

#ifdef _WIN32
#include <windows.h>
#endif

#include "C_Worhp_Data.h"
#include "monitor_structure.h"

class ModuleBase {
public:
	ModuleBase();
	virtual ~ModuleBase();

	void Close();
	int OpenDll(const std::string &s);
	void CloseDll();

	std::string openname;

#ifdef _WIN32
	FARPROC GetFunction(const std::string &name, int optional);
	HINSTANCE hDll;
#else
	void *GetFunction(const std::string &name, int optional);
	void *hDll;
#endif

};

class ModuleMonitor : public ModuleBase {
public:

	ModuleMonitor(const std::string &s);
	~ModuleMonitor();

	void Text();
	void Iter(OptVar *opt, Workspace *wsp, Params *par, Control *cnt);

	typedef void(*WorhpMonitorText_t) (const char *buf, int mode);
	WorhpMonitorText_t WorhpMonitorText;

	typedef void(*WorhpMonitorIter_t) (const OptVar *opt, const  Workspace *wsp, const Params *par, const Control *cnt);
	WorhpMonitorIter_t WorhpMonitorIter;

	typedef void(*WorhpMonitorInit_t) ();
	WorhpMonitorInit_t WorhpMonitorInit;

	typedef void(*WorhpMonitorExit_t) ();
	WorhpMonitorExit_t WorhpMonitorExit;

	struct message {
		message(const std::string &s, int i) : s(s), i(i) {}
		std::string s;
		int i;
	};
};
