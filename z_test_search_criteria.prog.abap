*&---------------------------------------------------------------------*
*& Report  Z_TEST_SEARCH_CRITERIA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_search_criteria.

DATA: lt_tablenames TYPE tttabname,
      lv_tablename  LIKE LINE OF lt_tablenames,
      lo_error      TYPE REF TO zcx_lo_error,
      text          TYPE string.

sy-subrc = 18.

TRY.
    zcx_lo_error=>raise_syst_subrc(
      EXPORTING
        i_function_module = |FREE_SELECTIONS_DIALOG| ).
  CATCH zcx_lo_error INTO lo_error.
    text = lo_error->get_text( ).
    MESSAGE text TYPE 'S' DISPLAY LIKE 'E'.
ENDTRY.

lv_tablename = `ZEHS_MAT_HEAD`.
INSERT lv_tablename INTO TABLE lt_tablenames.
lv_tablename = `ZEHS_MAT_DETAIL`.
INSERT lv_tablename INTO TABLE lt_tablenames.

TRY .
    zcl_bc_search_criteria=>gui_free_select(
      EXPORTING
        i_tablenames       = lt_tablenames
        i_as_popup         = abap_true ).
  CATCH zcx_lo_error INTO lo_error.
    text = lo_error->get_text( ).
    MESSAGE text TYPE 'S' DISPLAY LIKE 'E'.
ENDTRY.
