project(
  'CUTEst',
  'fortran', 'c',
  version: '2023.10.22',
  meson_version: '>= 0.61.0',
  default_options: [
    'buildtype=release',
    'libdir=lib',
    'default_library=static',
    'warning_level=0',
  ],
)

cc = meson.get_compiler('c')
fc = meson.get_compiler('fortran')
fc_compiler = find_program(fc.cmd_array())

libcutest_src = []
libcutest_c_src = []

cutest_binaries = []
cutest_tests = []

cutest_c_binaries = []
cutest_c_tests = []

libcutest_include = []
libcutest_include += include_directories('include')

# Sources
subdir('src/algencan')
subdir('src/bobyqa')
subdir('src/cg_descent')
subdir('src/cgplus')
subdir('src/cobyla')
subdir('src/curvi')
subdir('src/derchk')
subdir('src/dfo')
subdir('src/directsearch')
subdir('src/e04nqf')
subdir('src/filtersd')
subdir('src/filtersqp')
# subdir('src/gen77')
subdir('src/gen90')
subdir('src/genc')
subdir('src/gsl')
subdir('src/highs')
subdir('src/hrb')
subdir('src/ipopt')
subdir('src/knitro')
subdir('src/la04')
subdir('src/lbfgs')
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
subdir('src/tao')
subdir('src/tenmin')
# subdir('src/test')
subdir('src/tools')
subdir('src/tron')
subdir('src/uncmin')
subdir('src/vf13')
subdir('src/worhp')

# Preprocessing
if fc.get_id() == 'intel-cl' or fc.get_id() == 'intel-llvm-cl'
  output_generator = '/Fi:@OUTPUT@'
else
  output_generator = ['-o', '@OUTPUT@']
endif

pp_options = []
pp_flag = []
if fc.get_id() == 'gcc'
  pp_flag += '-cpp'
  pp_options += ['-cpp', '-E']
elif fc.get_id() == 'intel' or fc.get_id() == 'intel-llvm'
  pp_flag += '-fpp'
  pp_options += ['-fpp', '-P']
elif fc.get_id() == 'intel-cl' or fc.get_id() == 'intel-llvm-cl'
  pp_flag += '/fpp'
  pp_options += ['/fpp', '/P']
elif fc.get_id() == 'nagfor'
  pp_flag += '-fpp'
  pp_options += ['-fpp', '-F']
elif fc.get_id() == 'nvidia_hpc'
  pp_flag += '-Mcpp'
  pp_options += ['-Mcpp', '-F']
endif

gen_single = generator(fc_compiler,
                       output : 'double_@BASENAME@.f90',
                       arguments : pp_options + '-DSINGLE' +
                                   ['-I', '@CURRENT_SOURCE_DIR@/include', '@INPUT@'] +
                                   output_generator)

gen_double = generator(fc_compiler,
                         output : 'double_@BASENAME@.f90',
                         arguments : pp_options + '-DDOUBLE' +
                                   ['-I', '@CURRENT_SOURCE_DIR@/include', '@INPUT@'] +
                                   output_generator)

pp_sources_single = [gen_single.process(libcutest_src)]
pp_sources_double = [gen_double.process(libcutest_src)]

# Library
libcutest_single = library('cutest_single',
                           sources : pp_sources_single + libcutest_c_src,
                           include_directories : libcutest_include,
                           install : true)

libcutest_double = library('cutest_double',
                           sources : pp_sources_double + libcutest_c_src,
                           include_directories : libcutest_include,
                           install : true)

# Binaries
# foreach binary: cutest_binaries
#   binname = binary[0]
#   binfile = binary[1]
#   executable(binname,
#              sources : binfile,
#              include_directories : libcutest_include,
#              link_with : libcutest_double,
#              install : true)
# endforeach

# Fortran modules
script_modules = files('install_modules.py')
meson.add_install_script(script_modules)
