*&---------------------------------------------------------------------*
*& Report z_test_2017_02_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_02_02.

CLASS test DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

  PRIVATE SECTION.
    DATA: pa0000_tab TYPE STANDARD TABLE OF pa0000.

    METHODS:
      _select,
      _processing,
      _display.

ENDCLASS.

CLASS test IMPLEMENTATION.

  METHOD start.

    _select( ).
    _processing( ).
    _display( ).

  ENDMETHOD.

  METHOD _display.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(alv)
          CHANGING
            t_table        = pa0000_tab ).

        alv->display( ).

      CATCH cx_salv_error INTO DATA(error).    "
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD _processing.

    LOOP AT pa0000_tab ASSIGNING FIELD-SYMBOL(<pa0000>).

    ENDLOOP.

  ENDMETHOD.

  METHOD _select.

    SELECT FROM pa0000
           FIELDS *
           INTO TABLE @pa0000_tab
           UP TO 100 ROWS.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test( )->start( ).
