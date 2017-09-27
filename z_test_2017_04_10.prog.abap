*&---------------------------------------------------------------------*
*& Report z_test_2017_04_10
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_04_10.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(r_instance) TYPE REF TO controller.

    CLASS-DATA:
      okcode TYPE sy-ucomm.

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
      _display.

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
              r_container  = cl_gui_container=>screen0
            IMPORTING
              r_salv_table = alv
            CHANGING
              t_table 		 = t100_tab ).

        alv->display( ).

      CATCH cx_salv_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD pai_0100.

    DATA(save_ok_code) = okcode.

    CLEAR: okcode.

    CASE save_ok_code.
      WHEN 'BACK'
        OR 'EXIT'
        OR 'CANC'.

        SET SCREEN 0.

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
