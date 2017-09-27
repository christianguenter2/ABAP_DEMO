*&---------------------------------------------------------------------*
*& Report  Z_TEST_IH_PRINT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_ih_print.

PARAMETERS: p_aufnr TYPE aufnr DEFAULT '50079304' OBLIGATORY.

DATA: lo_error TYPE REF TO zcx_lo_error.

TRY.
    zcl_pm_order_printer=>print_del_note( i_aufnr = p_aufnr ).
  CATCH zcx_lo_error INTO lo_error.
    MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
ENDTRY.
