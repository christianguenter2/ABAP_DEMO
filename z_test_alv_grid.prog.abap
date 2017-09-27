REPORT z_test_alv_grid.

DATA: ok_code TYPE sy-ucomm.

CLASS lcl_alv DEFINITION.
  PUBLIC SECTION.
    METHODS:
      start,
      pbo,
      user_command_0100.

  PRIVATE SECTION.
    DATA: lt_t100          TYPE STANDARD TABLE OF t100,
          detail_visible   TYPE abap_bool VALUE abap_true,
          custom_container TYPE REF TO cl_gui_custom_container,
          alv              TYPE REF TO cl_gui_alv_grid,
          fcat             TYPE lvc_t_fcat.
ENDCLASS.

CLASS lcl_alv IMPLEMENTATION.
  METHOD start.
    SELECT * FROM t100
             INTO TABLE @lt_t100
             UP TO 20 ROWS.
  ENDMETHOD.

  METHOD pbo.

    IF custom_container IS NOT BOUND.
      custom_container = NEW cl_gui_custom_container( container_name = 'CUST_CONTAINER' ).
    ENDIF.

    IF alv IS NOT BOUND.
      alv = NEW cl_gui_alv_grid( custom_container ).

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name = 'T100'
        CHANGING
          ct_fieldcat      = fcat.

      alv->set_table_for_first_display(
        CHANGING
          it_outtab                     = lt_t100
          it_fieldcatalog               = fcat ) .
    ENDIF.

    SET PF-STATUS 'STANDARD'.

  ENDMETHOD.

  METHOD user_command_0100.

    CASE ok_code.
      WHEN '&TOGGLE'.

        detail_visible = boolc( detail_visible = abap_false ).

        IF detail_visible = abap_false.
          DATA(lt_exclude) = VALUE ui_functions( ( cl_gui_alv_grid=>mc_fc_detail ) ).
        ENDIF.

        alv->set_table_for_first_display(
           EXPORTING
             it_toolbar_excluding          = lt_exclude
           CHANGING
             it_outtab                     = lt_t100
             it_fieldcatalog               = fcat ).

      WHEN OTHERS.
        SET SCREEN 0.
        LEAVE SCREEN.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA(application) = NEW lcl_alv( ).

  application->start( ).

  CALL SCREEN 0100.

MODULE status_0100 OUTPUT.
  application->pbo( ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  application->user_command_0100( ).
ENDMODULE.
