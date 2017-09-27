REPORT z_test_escape.

cl_demo_output=>display( escape( val    = 'Test'
                                 format = cl_abap_format=>e_xss_js ) ).
