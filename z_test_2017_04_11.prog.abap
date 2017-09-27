*&---------------------------------------------------------------------*
*& Report z_test_2017_04_11
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_04_11.

class controller DEFINITION DEFERRED.

CLASS lcx_error DEFINITION FINAL
                INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    CLASS-METHODS:
      raise_text
        IMPORTING
          i_text TYPE csequence
        RAISING
          lcx_error.

    METHODS:
      constructor
        IMPORTING
          textid   LIKE textid OPTIONAL
          previous LIKE previous OPTIONAL
          text     TYPE csequence OPTIONAL,

      get_text REDEFINITION.


  PRIVATE SECTION.
    DATA:
      _text TYPE string.

ENDCLASS.

CLASS view DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor,
      display,
      pai_0100,
      pbo_0100.

    CLASS-DATA: okcode TYPE sy-ucomm.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(r_instance) TYPE REF TO view.

  PRIVATE SECTION.
    CLASS-DATA _instance TYPE REF TO view.
    DATA:
      alv TYPE REF TO cl_salv_table,
      controller TYPE REF TO controller.

    METHODS:
      _on_added_function FOR EVENT added_function OF cl_salv_events_table
        IMPORTING
            e_salv_function.

ENDCLASS.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(r_instance) TYPE REF TO controller.

    DATA:
      table TYPE REF TO data READ-ONLY.

    METHODS:
      constructor,
      start,
      dispatch_fcode
        IMPORTING
          i_fcode TYPE sy-ucomm
        RAISING
          lcx_error.

  PRIVATE SECTION.

    CLASS-DATA _instance TYPE REF TO controller.
    DATA: t100_tab TYPE STANDARD TABLE OF t100.

    METHODS:
      _select,
      _process,
      _display.

ENDCLASS.

CLASS lcx_error IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid = textid previous = previous ).

    _text = text.

  ENDMETHOD.


  METHOD raise_text.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        text = i_text.

  ENDMETHOD.

  METHOD get_text.

    result = COND #( WHEN _text IS NOT INITIAL THEN _text
                     ELSE super->get_text( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS view IMPLEMENTATION.

  METHOD constructor.

    controller = controller=>get_instance( ).

  ENDMETHOD.

  METHOD get_instance.

    r_instance = _instance = COND #( WHEN _instance IS NOT BOUND THEN NEW view( )
                                     ELSE _instance ).

  ENDMETHOD.

  METHOD display.

    CALL SCREEN 0100.

  ENDMETHOD.

  METHOD pbo_0100.

    SET PF-STATUS 'STATUS_0100'.

    CHECK alv IS NOT BOUND.

    ASSIGN controller->table->* TO FIELD-SYMBOL(<table>).
    ASSERT sy-subrc = 0.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container  = cl_gui_container=>screen0
          IMPORTING
            r_salv_table = alv
          CHANGING
            t_table 		 = <table> ).

        alv->get_functions( )->add_function( name 		= |TEST|
                                             icon 		= |{ icon_led_green }|
                                             text 		= |Test|
                                             tooltip	= |Test|
                                             position = if_salv_c_function_position=>right_of_salv_functions ).

        alv->get_functions( )->set_all( abap_true ).

        DATA(event) = alv->get_event( ).

        SET HANDLER _on_added_function FOR event.

        alv->display( ).

      CATCH cx_salv_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD pai_0100.

    DATA(save_ok_code) = okcode.

    CLEAR okcode.

    TRY.
        controller->dispatch_fcode( save_ok_code ).

      CATCH lcx_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD _on_added_function.

    TRY.
        controller->dispatch_fcode( e_salv_function ).

      CATCH lcx_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD constructor.

    table = REF #( t100_tab ).

  ENDMETHOD.

  METHOD get_instance.

    r_instance = _instance = COND #( WHEN _instance IS NOT BOUND THEN NEW controller( )
                                     ELSE _instance ).

  ENDMETHOD.

  METHOD start.

    _select( ).
    _process( ).
    _display( ).

  ENDMETHOD.

  METHOD _select.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @t100_tab
           UP TO 100 ROWS.

  ENDMETHOD.

  METHOD _process.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).

    ENDLOOP.

  ENDMETHOD.

  METHOD _display.

    view=>get_instance( )->display( ).

  ENDMETHOD.

  METHOD dispatch_fcode.

    CASE i_fcode.
      WHEN 'BACK'
      OR   'EXIT'
      OR   'CANC'.

        SET SCREEN 0.

      WHEN OTHERS.

        lcx_error=>raise_text( |Fcode {  i_fcode } not yet implemented!|  ).

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  controller=>get_instance( )->start( ).

MODULE pbo_0100 OUTPUT.
  view=>get_instance( )->pbo_0100( ).
ENDMODULE.

MODULE pai_0100 INPUT.
  view=>get_instance( )->pai_0100( ).
ENDMODULE.
