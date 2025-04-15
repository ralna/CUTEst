# CUTEst
The Constrained and Unconstrained Testing Environment with safe threads (CUTEst) for optimization software

See the [wiki](https://github.com/ralna/CUTEst/wiki) for download and installation instructions

[![Build Status](https://img.shields.io/github/actions/workflow/status/ralna/CUTEst/ci.yml?branch=master)](https://github.com/ralna/CUTEst/actions/workflows/ci.yml)

[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)

## Installation

### Precompiled libraries and executables

We provide precompiled CUTEst libraries and executables in the [releases tab](https://github.com/ralna/CUTEst/releases/latest/) for Linux, macOS (Intel & Silicon) and Windows.

### Installation from source

CUTEst can be installed using the [Meson build system](https://mesonbuild.com) (all commands below are to be run from the top of the source tree):

```shell
meson setup builddir -Dtests=true
meson compile -C builddir
meson install -C builddir
meson test -C builddir
```

For more comprehensive Meson options (`-Doption=value`), including how to compile CUTEst in **quaduple** precision, please see [meson_options.txt](https://raw.githubusercontent.com/ralna/CUTEst/refs/heads/master/meson_options.txt).

CUTEst can also be installed via the "make" build system based on [ARCHDefs](https://github.com/ralna/ARCHDefs).
To use this variant, follow the instructions in the CUTEst [wiki](https://github.com/ralna/CUTEst/wiki).

### How to use the static libraries generated by CUTEst

By default, the CUTEst Meson build system compiles static libraries: `libcutest_single.a` and `libcutest_double.a` (as well as `libcutest_quadruple.a` if built with `-Dquadruple=true`).
These libraries are incomplete by design, they contain undefined symbols corresponding to problem-specific functions that must be provided by the decoded SIF problem.

The package [SIFDecode](https://github.com/ralna/SIFDecode) describes how to decode a SIF problem and generate the corresponding problem library.
This results in a workflow like the following:
```shell
# Linux / FreeBSD
gfortran -shared -Wl,--whole-archive libcutest_double.a libsif.a -Wl,--no-whole-archive -o libpb.so

# Windows
gfortran -shared -Wl,--whole-archive libcutest_double.a libsif.a -Wl,--no-whole-archive -o libpb.dll

# macOS
gfortran -dynamiclib -Wl,-all_load libcutest_double.a libsif.a -Wl,-noall_load -o libpb.dylib
```

This is the classical approach, where a custom library is built for each decoded SIF problem.
It has been in use for decades and remains fully supported.

### New approach with shared libraries and trampolines

A modern alternative is now available.
It is possible to build shared libraries where missing symbols are resolved dynamically using symbol forwarding, via the mechanism in [`cutest_trampoline.f90`](https://github.com/ralna/CUTEst/blob/master/src/tools/cutest_trampoline.f90).

To enable this feature, CUTEst must be built with shared library support:
```shell
meson setup builddir -Ddefault_library=shared
meson compile -C builddir
meson install -C builddir
```
It requires the version `v2.5.0` of CUTEst.

With this approach, there is no need to generate a new combined library for each decoded SIF problem.
Instead, you dynamically load the decoded problem library at runtime.

The only user-facing addition is the subroutines `load_routines_` and `unload_routines_`,
which allows you to specify the path to the SIF problem library.

At runtime, the CUTEst shared library will forward the symbols `elfun_`, `group_`, and `range_` to the specified library.

A Fortran module named `CUTEST_TRAMPOLINE_*` is also available for seamless integration in Fortran projects.

## Interfaces

- [CUTEst.jl](https://github.com/JuliaSmoothOptimizers/CUTEst.jl) is the Julia interface of CUTEst.

- [PyCUTEst](https://github.com/jfowkes/pycutest) is the Python interface of CUTEst.

- [MatCUTEst](https://github.com/matcutest/matcutest) is the MATLAB interface of CUTEst.

- CUTEst also provides a C interface; details can be found in the [README.C](https://raw.githubusercontent.com/ralna/CUTEst/refs/heads/master/doc/README.C).

## Note

The latest features added in the release `v2.3.0` of CUTEst require at least the version `v2.6.1` of [SIFDecode](https://github.com/ralna/SIFDecode).
