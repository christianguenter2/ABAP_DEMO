*&---------------------------------------------------------------------*
*& Report z_test_count
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_count.

DATA(text) = |12345678901234567890|.

cl_demo_output=>write( count( val = text sub = `0` ) ).
cl_demo_output=>write( find( val = text sub = `x` ) ).
cl_demo_output=>display( ).
