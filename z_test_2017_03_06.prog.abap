*&---------------------------------------------------------------------*
*& Report z_test_2017_03_06
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_03_06.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(r_result) TYPE REF TO controller.

    CLASS-DATA: okcode TYPE sy-ucomm.

    METHODS:
      start,
      pai_0100,
      pbo_0100.

  PRIVATE SECTION.
    CLASS-DATA _instance TYPE REF TO controller.
    DATA: t100_tab TYPE STANDARD TABLE OF t100,
          m_alv    TYPE REF TO cl_salv_table.

    METHODS:
      display,
      process,
      select,
      _added_function_handler
        FOR EVENT added_function OF cl_salv_events_table.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD get_instance.

    IF _instance IS NOT BOUND.

      _instance = NEW controller( ).

    ENDIF.

    r_result = _instance.

  ENDMETHOD.

  METHOD start.

    select( ).
    process( ).
    display( ).

  ENDMETHOD.

  METHOD select.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @t100_tab
           UP TO 100 ROWS.

  ENDMETHOD.

  METHOD process.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).

    ENDLOOP.

  ENDMETHOD.

  METHOD display.

    CALL SCREEN 0100.

  ENDMETHOD.

  METHOD pbo_0100.

    SET PF-STATUS 'STATUS_0100'.

    CHECK m_alv IS NOT BOUND.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = cl_gui_container=>screen0
          IMPORTING
            r_salv_table   = m_alv
          CHANGING
            t_table        = t100_tab ).

        m_alv->get_functions( )->add_function( name     = |TEST|
                                               icon     = |{ icon_led_green }|
                                               text     = |Test|
                                               tooltip  = |Test|
                                               position = if_salv_c_function_position=>right_of_salv_functions ).

        DATA(event) = m_alv->get_event( ).

        SET HANDLER _added_function_handler FOR event.

        m_alv->display( ).

      CATCH cx_salv_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD pai_0100.

    DATA(save_ok_code) = okcode.

    CLEAR okcode.

    CASE save_ok_code.
      WHEN 'EXIT'
      OR   'BACK'
      OR   'CANC' .

        SET SCREEN 0.

    ENDCASE.

  ENDMETHOD.


  METHOD _added_function_handler.

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
