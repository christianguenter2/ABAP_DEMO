REPORT z_test_bigint.

DATA: i TYPE i.

DATA(test) = cl_abap_exceptional_values=>get_max_value( in = i ).

ASSIGN test->* TO FIELD-SYMBOL(<i>).

cl_demo_output=>display( <i> ).
