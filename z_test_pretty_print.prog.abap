*&---------------------------------------------------------------------*
*& Report  Z_TEST_PRETTY_PRINT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_pretty_print.

DATA: i_name         TYPE string,
*      i_langu       TYPE sy-langu value sy-langu,
      i_domvalue_l TYPE domvalue_l,
      r_text       TYPE ddtext.

zcl_bc_ddic_doma=>get_text_of_fixed_value(
  EXPORTING
    i_name       = `1 = 2`
    i_langu      = sy-langu    " Language Key of Current Text Environment
    i_domvalue_l = i_domvalue_l    " Values for Domains: Single Value / Upper Limit
*  RECEIVING
*    r_text      = r_text    " Explanatory short text
).

DATA: x TYPE i.
x = find( val = `Test`
          sub     = `1234` ).

x1 = 1.
x  = 2.
