*&---------------------------------------------------------------------*
*& Report  Z_TEST_WRITE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_write.

DATA: in  TYPE figlq_acytd_bal VALUE '1856308.18-',
      out TYPE c LENGTH 19.

out = |{ in STYLE = (0) }|.

cl_demo_output=>write_data( in ).
cl_demo_output=>write_data( out ).
cl_demo_output=>display( ).
