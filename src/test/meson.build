c_single = files('c_elfun_single.f', 'c_group_single.f', 'c_range_single.f')
u_single = files('u_elfun_single.f', 'u_group_single.f', 'u_range_single.f')
q_single = files('q_elfun_single.f', 'q_group_single.f', 'q_range_single.f')

c_double = files('c_elfun_double.f', 'c_group_double.f', 'c_range_double.f')
u_double = files('u_elfun_double.f', 'u_group_double.f', 'u_range_double.f')
q_double = files('q_elfun_double.f', 'q_group_double.f', 'q_range_double.f')

c_quadruple = files('c_elfun_quadruple.f', 'c_group_quadruple.f', 'c_range_quadruple.f')
u_quadruple = files('u_elfun_quadruple.f', 'u_group_quadruple.f', 'u_range_quadruple.f')
q_quadruple = files('q_elfun_quadruple.f', 'q_group_quadruple.f', 'q_range_quadruple.f')

cutest_tests += [['cutest', 'single', 'ctest_single'         , files('ctest.F90'         ) + c_single],
                 ['cutest', 'single', 'ctest_threaded_single', files('ctest_threaded.F90') + c_single],
                 ['cutest', 'single', 'utest_single'         , files('utest.F90'         ) + u_single],
                 ['cutest', 'single', 'utest_threaded_single', files('utest_threaded.F90') + u_single],
                 ['cutest', 'single', 'lqp_test_single'      , files('lqptest.F90'       ) + q_single],

                 ['cutest', 'double', 'ctest_double'         , files('ctest.F90'         ) + c_double],
                 ['cutest', 'double', 'ctest_threaded_double', files('ctest_threaded.F90') + c_double],
                 ['cutest', 'double', 'utest_double'         , files('utest.F90'         ) + u_double],
                 ['cutest', 'double', 'utest_threaded_double', files('utest_threaded.F90') + u_double],
                 ['cutest', 'double', 'lqp_test_double'      , files('lqptest.F90'       ) + q_double]]

cutest_c_tests += [['cutest', 'single', 'ctest_c_single' , files('ctest.c'  ) + c_single],
                   ['cutest', 'single', 'utest_c_single' , files('utest.c'  ) + u_single],
                   ['cutest', 'single', 'ctest2_c_single', files('ctest_c.c') + c_single],
                   ['cutest', 'single', 'utest2_c_single', files('utest_c.c') + u_single],

                   ['cutest', 'double', 'ctest_c_double' , files('ctest.c'  ) + c_double],
                   ['cutest', 'double', 'utest_c_double' , files('utest.c'  ) + u_double],
                   ['cutest', 'double', 'ctest2_c_double', files('ctest_c.c') + c_double],
                   ['cutest', 'double', 'utest2_c_double', files('utest_c.c') + u_double]]

if build_quadruple
    cutest_tests += [['cutest', 'quadruple', 'ctest_quadruple'         , files('ctest.F90'         ) + c_quadruple],
                     ['cutest', 'quadruple', 'ctest_threaded_quadruple', files('ctest_threaded.F90') + c_quadruple],
                     ['cutest', 'quadruple', 'utest_quadruple'         , files('utest.F90'         ) + u_quadruple],
                     ['cutest', 'quadruple', 'utest_threaded_quadruple', files('utest_threaded.F90') + u_quadruple],
                     ['cutest', 'quadruple', 'lqp_test_quadruple'      , files('lqptest.F90'       ) + q_quadruple]]

    cutest_c_tests += [['cutest', 'quadruple', 'ctest_c_quadruple' , files('ctest.c'  ) + c_quadruple],
                       ['cutest', 'quadruple', 'utest_c_quadruple' , files('utest.c'  ) + u_quadruple],
                       ['cutest', 'quadruple', 'ctest2_c_quadruple', files('ctest_c.c') + c_quadruple],
                       ['cutest', 'quadruple', 'utest2_c_quadruple', files('utest_c.c') + u_quadruple]]
endif
