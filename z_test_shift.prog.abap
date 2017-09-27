*&---------------------------------------------------------------------*
*& Report  Z_TEST_SHIFT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_shift.

DATA: lifnr      TYPE lifnr,
      qals       TYPE qals,
      ls_ext_ewm TYPE bapiext.

IF ls_ext_ewm-value(1) NS '0123456789'.
  SHIFT lifnr.
ENDIF.

lifnr         = ls_ext_ewm-value.
qals-sellifnr = lifnr.
qals-lifnr    = lifnr.

cl_demo_output=>display_data( lifnr ).

cl_demo_output=>display_data( shift_left( val = '00009207'
                                          sub = '0' ) ).
