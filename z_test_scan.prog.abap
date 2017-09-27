*&---------------------------------------------------------------------*
*& Report  Z_TEST_SCAN
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_scan.

DATA: l_scan_object TYPE REF TO cl_tpda_script_scan,
      l_curr_itab   TYPE string.

l_scan_object = cl_tpda_script_scan=>scan(
    p_program     = 'Z_TEST_SCAN_2'
    p_include     = 'Z_TEST_SCAN_2'
    p_line        = 35 ).

IF sy-subrc = 0.

ENDIF.
