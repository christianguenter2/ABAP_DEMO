*&---------------------------------------------------------------------*
*& Report z_test_750
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_750.

DATA(test1234) = |Hallo Welt!|.

cl_demo_output=>display( test1234 ).
