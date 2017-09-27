*&---------------------------------------------------------------------*
*& Report  z_test_2017_03_15
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_03_15.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(r_instance) TYPE REF TO controller.

    CLASS-DATA: okcode TYPE syucomm.

    METHODS:
      constructor,
      start,
      pbo_0100,
      pai_0100.

  PRIVATE SECTION.

    CONSTANTS: n_times TYPE i VALUE 20.

    CLASS-DATA:
      _instance TYPE REF TO controller.

    DATA: t100_tab TYPE STANDARD TABLE OF t100,
          alv 		 TYPE REF TO cl_salv_table,
          icon_tab TYPE STANDARD TABLE OF icon.

    METHODS:
      _select,
      			_process,
      			_display,

      _handle_added_function FOR EVENT added_function OF  cl_salv_events_table
        IMPORTING
            e_salv_function,

      _dispatch_fcode
        IMPORTING
          i_fcode TYPE syucomm.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD constructor.

    SELECT FROM icon
           FIELDS *
           INTO TABLE @icon_tab
           UP TO @n_times ROWS.

  ENDMETHOD.

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

    CALL SCREEN 0100.

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

        DO n_times TIMES.

          alv->get_functions( )->add_function( name 		= |TEST{ sy-index }|
                                               icon 		= |{ VALUE #( icon_tab[ sy-index ]-id OPTIONAL ) }|
                                               tooltip	= |Test|
                                               position = if_salv_c_function_position=>right_of_salv_functions ).

        ENDDO.

        DATA(event) = alv->get_event( ).

        SET HANDLER _handle_added_function FOR event.

        alv->display( ).

      CATCH cx_salv_error INTO DATA(error).    "
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD pai_0100.

    DATA(save_ok_code) = okcode.

    CLEAR okcode.

    _dispatch_fcode( save_ok_code ).

  ENDMETHOD.

  METHOD _handle_added_function.

    _dispatch_fcode( e_salv_function ).

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

ENDCLASS.

START-OF-SELECTION.
  controller=>get_instance( )->start( ).

MODULE pbo_0100 OUTPUT.
  controller=>get_instance( )->pbo_0100( ).
ENDMODULE.

MODULE pai_0100 INPUT.
  controller=>get_instance( )->pai_0100( ).
ENDMODULE.
