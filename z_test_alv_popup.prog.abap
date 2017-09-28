*&---------------------------------------------------------------------*
*& Report z_test_alv_popup
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_alv_popup.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: run.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_data,
             selected TYPE flag.
        INCLUDE TYPE t100.
    TYPES: END OF ty_data.

    TYPES: tty_data TYPE STANDARD TABLE OF ty_data
                         WITH NON-UNIQUE DEFAULT KEY.

    DATA: alv     TYPE REF TO cl_salv_table,
          mt_t100 TYPE tty_data.

    METHODS:
      on_select_list_function_click FOR EVENT added_function OF cl_salv_events_table
        IMPORTING e_salv_function,

      on_select_list_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING column row.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD run.

    DATA: column TYPE REF TO cl_salv_column_table.

    SELECT FROM t100
           FIELDS *
           INTO CORRESPONDING FIELDS OF TABLE @mt_t100
           UP TO 100 ROWS.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = alv
          CHANGING
            t_table      = mt_t100 ).

        alv->set_screen_popup( start_column = 5
                               end_column   = 100
                               start_line   = 5
                               end_line     = 25 ).

        alv->set_screen_status( pfstatus = 'ST850'
                                report   = 'Z_TEST_ALV_POPUP' ).

        DATA(events) = alv->get_event( ).

        SET HANDLER on_select_list_link_click FOR events.
        SET HANDLER on_select_list_function_click FOR events.

        column ?= alv->get_columns( )->get_column( `SELECTED` ).
        column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).

        alv->display( ).

      CATCH cx_salv_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD on_select_list_function_click.

    CASE e_salv_function.
      WHEN 'ALL'.

        LOOP AT mt_t100 ASSIGNING FIELD-SYMBOL(<t100>).
          <t100>-selected = abap_true.
        ENDLOOP.

        alv->refresh( ).

      WHEN 'NONE'.

        LOOP AT mt_t100 ASSIGNING <t100>.
          <t100>-selected = abap_false.
        ENDLOOP.

        alv->refresh( ).

      WHEN OTHERS.

        alv->close_screen( ).

    ENDCASE.

  ENDMETHOD.


  METHOD on_select_list_link_click.

    ASSIGN mt_t100[ row ] TO FIELD-SYMBOL(<t100>).
    IF sy-subrc = 0.
      <t100>-selected = boolc( <t100>-selected = abap_false ).
    ENDIF.

    alv->refresh( ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW controller( )->run( ).
