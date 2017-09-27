*&---------------------------------------------------------------------*
*& Report  Z_TEST_CONVERSION_EXIT_2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_conversion_exit_2.

DATA: belnr_vorg      TYPE zdocflow_ewm-belnr_vorg VALUE '008100000662',
      delivery_number TYPE vbeln.

CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
    input  = belnr_vorg
  IMPORTING
    output = delivery_number.

cl_demo_output=>write_data( belnr_vorg ).
cl_demo_output=>write_data( delivery_number ).
cl_demo_output=>display( ).
