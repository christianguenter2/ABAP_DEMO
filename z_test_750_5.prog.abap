*&---------------------------------------------------------------------*
*& Report z_test_750_5
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_750_5.

DATA(test) = VALUE ztest_750( name = 'Christian' surname = 'Günter' ).

cl_demo_output=>display( test ).
