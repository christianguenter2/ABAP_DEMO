REPORT z_test_random.

DATA(int) = cl_abap_random_int=>create( seed = 1555
                                        min = 0
                                        max = 1500 )->get_next( ).

cl_demo_output=>display( int ).
