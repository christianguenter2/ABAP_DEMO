*&---------------------------------------------------------------------*
*& Report  z_test_search_help
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT z_test_search_help.

PARAMETER: p_shlp TYPE shlpname OBLIGATORY DEFAULT 'ZZH_KTTXID'.

START-OF-SELECTION.

  TYPES: BEGIN OF ty_f4,
           tdobject TYPE tdobject,
           txtob    TYPE txtob,
           txtgr    TYPE txtgr,
           tdid     TYPE tdid,
           tdtext   TYPE tdtext,
         END OF ty_f4,
         tty_f4 TYPE STANDARD TABLE OF ty_f4.

  DATA: lt_f4 TYPE tty_f4,
        error TYPE REF TO zcx_lo_error.

  TRY.
      zcl_bc_f4_functions=>get_values_for_search_help(
        EXPORTING i_shlpname = p_shlp
        IMPORTING et_f4      = lt_f4 ).

      cl_demo_output=>display_data( lt_f4 ).

    CATCH zcx_lo_error INTO error.
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
