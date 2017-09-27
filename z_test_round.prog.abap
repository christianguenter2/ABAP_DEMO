*&---------------------------------------------------------------------*
*& Report  Z_TEST_ROUND
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_round.

DATA: deta_rerat(6)  TYPE p DECIMALS 6.

deta_rerat = '0.099999'.

CALL FUNCTION 'ROUND'
  EXPORTING
    decimals      = 3
    input         = deta_rerat
    sign          = 'X'
  IMPORTING
    output        = deta_rerat
  EXCEPTIONS
    input_invalid = 1
    overflow      = 2
    type_invalid  = 3
    OTHERS        = 4.

cl_demo_output=>display_data( deta_rerat ).
