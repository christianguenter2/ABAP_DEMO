*&---------------------------------------------------------------------*
*& Report z_test_uzeit
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_uzeit.

DATA: uzeit  TYPE sy-uzeit,
      uzeit2 TYPE sy-uzeit.

uzeit  = sy-uzeit.
uzeit2 = uzeit - 30.

cl_demo_output=>write( uzeit ).
cl_demo_output=>write( uzeit2 ).
cl_demo_output=>display( ).
