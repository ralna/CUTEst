project(
  'CUTEst',
  'fortran', 'c',
  version: '2023.10.22',
  meson_version: '>= 0.61.0',
  default_options: [
    'buildtype=release',
    'libdir=lib',
    'default_library=shared',
    'warning_level=0',
  ],
)

libcutest_src = []
cutest_binaries = []

libcutest_include = []
libcutest_include += include_directories('include')

# Sources
# subdir('src/algencan')
# subdir('src/bobyqa')
# subdir('src/cg_descent')
# subdir('src/cgplus')
# subdir('src/cobyla')
# subdir('src/curvi')
# subdir('src/derchk')
# subdir('src/dfo')
# subdir('src/directsearch')
# subdir('src/e04nqf')
# subdir('src/filtersd')
# subdir('src/filtersqp')
# subdir('src/gen')
# subdir('src/gen77')
# subdir('src/gen90')
# subdir('src/genc')
# subdir('src/gsl')
# subdir('src/highs')
# subdir('src/hrb')
# subdir('src/ipopt')
# subdir('src/knitro')
# subdir('src/la04')
# subdir('src/lbfgs')
# subdir('src/lbfgsb')
# subdir('src/lincoa')
# subdir('src/loqo')
# subdir('src/matlab')
# subdir('src/minos')
# subdir('src/newuoa')
# subdir('src/nitsol')
# subdir('src/nlpqlp')
# subdir('src/nomad')
# subdir('src/npsol')
# subdir('src/octave')
# subdir('src/osqp')
# subdir('src/pds')
# subdir('src/pennlp')
# subdir('src/praxis')
# subdir('src/ql')
# subdir('src/qplib')
# subdir('src/ral_nlls')
# subdir('src/snopt')
# subdir('src/spg')
# subdir('src/sqic')
# subdir('src/stats')
# subdir('src/stenmin')
# subdir('src/tao')
# subdir('src/tenmin')
# subdir('src/test')
subdir('src/tools')
# subdir('src/tron')
# subdir('src/uncmin')
# subdir('src/vf13')
# subdir('src/worhp')

# Library
libcutest = library('cutest',
                    sources : libcutest_src,
                    include_directories : libcutest_include,
                    install : true)

# Binaries
foreach binary: cutest_binaries
  binname = binary[0]
  binfile = binary[1]
  executable(binname,
             sources : binfile,
             link_with : libcutest,
             install : true)
endforeach

# Fortran modules
script_modules = files('install_modules.py')
meson.add_install_script(script_modules)
