*&---------------------------------------------------------------------*
*& Report  Z_TEST_BRF_PLUS_MAINTAIN_DT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_brf_plus_maintain_dt.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_ins TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X',
            p_dis TYPE abap_bool RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: insert,
                   display.

  PRIVATE SECTION.
    CLASS-DATA: lo_decision_table TYPE REF TO zcl_brf_dt_mat_master_template.
ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD insert.
    DATA: error TYPE REF TO cx_root,
          row   TYPE zcl_brf_dt_mat_master_template=>ty_dt_short.

    TRY.
        lo_decision_table = zcl_brf_dt_mat_master_template=>new( ).

        row-uname          = sy-uname.
        row-tabname        = 'MARA'.
        row-fieldname      = 'MEINS'.
        row-template_value = 'ST'.
        lo_decision_table->insert_row_at_index_one( row ).

        row-uname          = sy-uname.
        row-tabname        = 'MARC'.
        row-fieldname      = 'EKGRP'.
        row-template_value = 'E08'.
        lo_decision_table->insert_row_at_index_one( row ).

        lo_decision_table->save( ).

        MESSAGE 'Erfolg!' TYPE 'I'.
      CATCH cx_fdt_input cx_fdt cx_root INTO error.
        MESSAGE error TYPE 'I'.
        RETURN.
    ENDTRY.
  ENDMETHOD.                    "insert

  METHOD display.
    DATA: lt_user_data      TYPE zcl_brf_dt_mat_master_template=>tty_dt_short.

    lo_decision_table = zcl_brf_dt_mat_master_template=>new( ).

    lt_user_data = lo_decision_table->get_user_data( ).

    cl_demo_output=>display_data( lt_user_data ).
  ENDMETHOD.                    "display
ENDCLASS.                    "lcl_application IMPLEMENTATION

START-OF-SELECTION.
  CASE abap_true.
    WHEN p_ins.
      lcl_application=>insert( ).
    WHEN p_dis.
      lcl_application=>display( ).
  ENDCASE.
