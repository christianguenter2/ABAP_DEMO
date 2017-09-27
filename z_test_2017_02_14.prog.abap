*&---------------------------------------------------------------------*
*& Report z_test_2017_02_14
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_02_14.

CLASS test DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

  PRIVATE SECTION.
    DATA: t100_tab TYPE STANDARD TABLE OF t100.
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

  METHOD _select.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @t100_tab
           UP TO 100 ROWS.

  ENDMETHOD.

  METHOD _processing.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).

    ENDLOOP.

  ENDMETHOD.

  METHOD _display.

    TRY.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = DATA(alv)     " Basisklasse einfache ALV Tabellen
          CHANGING  t_table      = t100_tab ).

        alv->display( ).

      CATCH cx_salv_error INTO DATA(error).    "
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test( )->start( ).
