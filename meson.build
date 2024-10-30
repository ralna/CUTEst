project(
  'CUTEst',
  'fortran', 'c',
  version: '2.4.0',
  meson_version: '>= 0.62.0',
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

# Remove messages about deprecated Intel compilers
if cc.get_id() == 'intel'
  add_global_arguments('-diag-disable=10441', language : 'c')
  add_global_link_arguments('-diag-disable=10441', language : 'c')
endif
if cc.get_id() == 'intel-cl'
  add_global_arguments('/Qdiag-disable=10441', language : 'c')
  add_global_link_arguments('/Qdiag-disable=10441', language : 'c')
endif

# Options
install_modules = get_option('modules')
build_quadruple = get_option('quadruple')
int64 = get_option('int64')
build_tests = get_option('tests')

libcutest_src = []
libcutest_f_src = []
libcutest_c_src = []
libcutest_delegate_src = []

cutest_binaries = []
cutest_tests = []

cutest_c_binaries = []
cutest_c_tests = []

# Headers
cutest_headers = []
libcutest_include = []
libcutest_include += include_directories('include')

# Dependency
libdl = fc.find_library('dl', required : false)

# Sources
subdir('include')
# subdir('src/algencan')     # needs algencan_
# subdir('src/bobyqa')       # needs bobyqa_
# subdir('src/cg_descent')   # needs cg_descent_
# subdir('src/cgplus')       # needs cgfam_
# subdir('src/cobyla')       # needs cobyla_
# subdir('src/curvi')        # needs curvih_, curvif_, curvig_
subdir('src/derchk')
# subdir('src/dfo')          # needs dfo_
# subdir('src/directsearch') # needs nm_, pattrn_, smd_, shh_
# subdir('src/e04nqf')       # needs e04npf_, e04nrf_, e04nqf_
# subdir('src/filtersd')     # needs filtersd_
# subdir('src/filtersqp')    # needs readpar_, readscale_, filtersqp_
# subdir('src/gen77')        # ?
# subdir('src/gen90')        # ?
# subdir('src/genc')         # ?
# subdir('src/gsl')          # needs <gsl/gsl_vector_complex_long_double.h>
# subdir('src/highs')        # needs Fortran module highs_fortran_api
subdir('src/hrb')
# subdir('src/ipopt')        # needs ipcreate_, ipsolve_, ipfree_
# subdir('src/knitro')       # needs KN_get_release, KN_new, ...
# subdir('src/la04')         # needs mc49ad_, la04id_, la04ad_
# subdir('src/lbfgs')        # needs lbfgs_
# subdir('src/lbfgsb')       # needs setulb_
# subdir('src/lincoa')       # needs lincoa_
# subdir('src/loqo')         # needs openlp, nlsetup, solvelp
# subdir('src/matlab')       # --- not supported by build system ---
# subdir('src/minos')        # needs m1open_, mispec_, minoss_
# subdir('src/newuoa')       # needs newuoa_
# subdir('src/nitsol')       # needs nitsol_, ...
# subdir('src/nlpqlp')       # needs nlpqlp_
subdir('src/nomad')
# subdir('src/npsol')        # needs npfile_, npoptn_, npsol_
# subdir('src/octave')       # --- not supported by build system ---
# subdir('src/osqp')         # needs osqp.h
# subdir('src/pds')          # needs search_, getss_, pds_, result_
# subdir('src/pennlp')       # needs pennlpf_
# subdir('src/praxis')       # needs praxis_
# subdir('src/ql')           # needs ql_
subdir('src/qplib')
# subdir('src/ral_nlls')     # needs the Fortran module RAL_NLLS_precision
# subdir('src/snopt')        # needs sninit_, snspec_, snoptb_
# subdir('src/spg')          # needs spg_
# subdir('src/sqic')         # needs crd2spr_, crd2spr_, crd2spr_, ctsqic_
subdir('src/stats')
# subdir('src/stenmin')      # needs stumcd_
# subdir('src/tao')          # needs petsc/finclude/petsctao.h and Fortran module petsctao
# subdir('src/tenmin')       # needs tensor_
subdir('src/test')
subdir('src/tools')
# subdir('src/tron')         # needs dnrm2_, dgpnrm2_
# subdir('src/uncmin')       # needs optif9_
# subdir('src/vf13')         # needs vf13ad_
# subdir('src/worhp')        # needs worhp.h

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
if int64
  pp_flag += '-DINTEGER_64'
  pp_options += '-DINTEGER_64'
endif
if host_machine.system() == 'windows'
  pp_flag += '-DWINDOWS'
  pp_options += '-DWINDOWS'
endif

# Library libcutest_single or libcutest_single_64
libcutest_single_name = 'cutest_single'
if int64
  libcutest_single_name += '_64'
endif

gen_single = generator(fc_compiler,
                       output : 'single_@BASENAME@.f90',
                       arguments : pp_options + '-DREAL_32' +
                                   ['-I', '@CURRENT_SOURCE_DIR@/include', '@INPUT@'] +
                                   output_generator)

pp_sources_single = [gen_single.process(libcutest_src)]

libcutest_single = library(libcutest_single_name,
                           dependencies : libdl,
                           sources : pp_sources_single + libcutest_c_src,
                           include_directories : libcutest_include,
                           install : true)

# Library libcutest_double or libcutest_double_64
libcutest_double_name = 'cutest_double'
if int64
  libcutest_double_name += '_64'
endif

gen_double = generator(fc_compiler,
                         output : 'double_@BASENAME@.f90',
                         arguments : pp_options +
                                     ['-I', '@CURRENT_SOURCE_DIR@/include', '@INPUT@'] +
                                     output_generator)

pp_sources_double = [gen_double.process(libcutest_src)]

libcutest_double = library(libcutest_double_name,
                           dependencies : libdl,
                           sources : pp_sources_double + libcutest_c_src,
                           include_directories : libcutest_include,
                           install : true)

# Library libcutest_quadruple or libcutest_quadruple_64
if build_quadruple
  libcutest_quadruple_name = 'cutest_quadruple'
  if int64
    libcutest_quadruple_name += '_64'
  endif

  gen_quadruple = generator(fc_compiler,
                            output : 'quadruple_@BASENAME@.f90',
                            arguments : pp_options + '-DREAL_128' + '-DCUTEST_16btye_reals_exist' +
                                        ['-I', '@CURRENT_SOURCE_DIR@/include', '@INPUT@'] +
                                        output_generator)

  pp_sources_quadruple = [gen_quadruple.process(libcutest_src)]

  libcutest_quadruple = library(libcutest_quadruple_name,
                                dependencies : libdl,
                                sources : pp_sources_quadruple + libcutest_c_src,
                                include_directories : libcutest_include,
                                install : true)
endif

# Binaries
# if get_option('default_library') == 'shared'
#   foreach binary: cutest_binaries + cutest_c_binaries
#     binname = binary[0]
#     binfile = binary[1]
#     executable(binname,
#                sources : binfile,
#                include_directories : libcutest_include,
#                link_with : libcutest_double,
#                install : true)
#   endforeach
# endif

# Headers
install_headers(cutest_headers)

# Fortran modules
if install_modules
  script_modules = files('install_modules.py')
  meson.add_install_script(script_modules)
endif

# Fortran tests
if build_tests

  fortran_tests_folder = 'tests/Fortran'

  foreach test: cutest_tests
    package = test[0]
    precision = test[1]
    name = test[2]
    file = test[3]

    if precision == 'single'
      libcutest_precision = libcutest_single
      args_precision = pp_flag + '-DREAL_32'
    endif
    if precision == 'double'
      libcutest_precision = libcutest_double
      args_precision = pp_flag
    endif
    if precision == 'quadruple'
      libcutest_precision = libcutest_quadruple
      args_precision = pp_flag + '-DREAL_128' + '-DCUTEST_16btye_reals_exist'
    endif

    test(name,
         executable(name, file,
                    fortran_args : args_precision,
                    link_with : libcutest_precision,
                    link_language : 'fortran',
                    include_directories: libcutest_include,
                    install : true,
                    install_dir : fortran_tests_folder),
         suite : [package, precision, 'fortran'],
         workdir : join_paths(meson.project_source_root(), 'src', 'test'),
         is_parallel : false)
    endforeach
endif

# C tests
if build_tests and (fc.get_id() == 'gcc')

  fortran_tests_folder = 'tests/C'

  foreach test: cutest_c_tests
    package = test[0]
    precision = test[1]
    name = test[2]
    file = test[3]

    if precision == 'single'
      libcutest_precision = libcutest_single
      args_precision = pp_flag + '-DREAL_32'
    endif
    if precision == 'double'
      libcutest_precision = libcutest_double
      args_precision = pp_flag
    endif
    if precision == 'quadruple'
      libcutest_precision = libcutest_quadruple
      args_precision = pp_flag + '-DREAL_128' + '-DCUTEST_16btye_reals_exist'
    endif

    test(name,
         executable(name, file,
                    fortran_args : args_precision,
                    c_args : args_precision,
                    link_with : libcutest_precision,
                    link_language : 'fortran',
                    include_directories: libcutest_include,
                    install : true,
                    install_dir : fortran_tests_folder),
         suite : [package, precision, 'C'],
         workdir : join_paths(meson.project_source_root(), 'src', 'test'),
         is_parallel : false)
    endforeach
endif
