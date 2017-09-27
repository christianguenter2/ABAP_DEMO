*&---------------------------------------------------------------------*
*& Report z_test_2017_03_11
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_03_11.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-DATA: okcode TYPE sy-ucomm.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(r_instance) TYPE REF TO controller.

    METHODS:
      start,
      pbo_0100,
      pai_0100.

  PRIVATE SECTION.
    CLASS-DATA:
      _instance TYPE REF TO controller.

    DATA:
      t100_tab TYPE STANDARD TABLE OF t100,
      alv      TYPE REF TO cl_salv_table.

    METHODS:
      _select,
      _process,
      _display,
      _added_function_handler
            FOR EVENT added_function OF cl_salv_events_table
        IMPORTING
            e_salv_function,
      _dispatch_fcode
        IMPORTING
          i_fcode TYPE sy-ucomm.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD get_instance.

    IF _instance IS NOT BOUND.

      _instance = NEW controller( ).

    ENDIF.

    r_instance = _instance.

  ENDMETHOD.

  METHOD start.

    _select( ).
    _process( ).
    _display( ).

  ENDMETHOD.

  METHOD _display.

    CALL SCREEN 0100.

  ENDMETHOD.

  METHOD _process.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).

    ENDLOOP.

  ENDMETHOD.

  METHOD _select.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @t100_tab
           UP TO 100 ROWS.

  ENDMETHOD.

  METHOD pbo_0100.

    SET PF-STATUS 'STATUS_0100'.

    CHECK alv IS NOT BOUND.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = cl_gui_container=>screen0
          IMPORTING
            r_salv_table   = alv
          CHANGING
            t_table        = t100_tab ).

        alv->get_functions( )->add_function( name     = |TEST|
                                             icon     = |{ icon_led_yellow }|
                                             text     = |Test|
                                             tooltip  = |Test|
                                             position = if_salv_c_function_position=>left_of_salv_functions ).

        DATA(event) = alv->get_event( ).

        SET HANDLER _added_function_handler FOR event.

        alv->display( ).

      CATCH cx_salv_error INTO DATA(error).    "
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD pai_0100.

    DATA(save_ok_code) = controller=>okcode.

    CLEAR: controller=>okcode.

    _dispatch_fcode( save_ok_code ).

  ENDMETHOD.

  METHOD _dispatch_fcode.

    CASE i_fcode.
      WHEN 'BACK'
      OR   'EXIT'
      OR   'CANC'.

        SET SCREEN 0.

      WHEN OTHERS.

        MESSAGE i_fcode TYPE 'I'.

    ENDCASE.

  ENDMETHOD.

  METHOD _added_function_handler.

    _dispatch_fcode( e_salv_function ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  controller=>get_instance( )->start( ).

MODULE pbo_0100 OUTPUT.
  controller=>get_instance( )->pbo_0100( ).
ENDMODULE.

MODULE pai_0100 INPUT.
  controller=>get_instance( )->pai_0100( ).
ENDMODULE.
