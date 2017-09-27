REPORT z_test_conversion_exit.

DATA(pernr) = CONV persno( '12667' ).

cl_demo_output=>display( |{ pernr ALPHA = OUT }| ).
