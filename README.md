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
meson setup builddir
meson compile -C builddir
meson install -C builddir
meson test -C builddir
```

CUTEst can also be installed via the "make" build system based on [ARCHDefs](https://github.com/ralna/ARCHDefs).
To use this variant, follow the instructions in the CUTEst [wiki](https://github.com/ralna/CUTEst/wiki).

## Interfaces

- [CUTEst.jl](https://github.com/JuliaSmoothOptimizers/CUTEst.jl) is the Julia interface of CUTEst.

- [PyCUTEst](https://github.com/jfowkes/pycutest) is the Python interface of CUTEst.

- [MatCUTEst](https://github.com/matcutest/matcutest) is the MATLAB interface of CUTEst.

## Note

The latest features added in the release `v2.3.0` of CUTEst require at least the version `v2.6.1` of [SIFDecode](https://github.com/ralna/SIFDecode).
