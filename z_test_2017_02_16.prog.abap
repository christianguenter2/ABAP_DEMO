*&---------------------------------------------------------------------*
*& Report z_test_2017_02_16
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_02_16.

CLASS test DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

  PRIVATE SECTION.
    DATA: t100_tab TYPE STANDARD TABLE OF t100.
    METHODS:
      _select,
      _process,
      _display.

ENDCLASS.

CLASS test IMPLEMENTATION.

  METHOD start.

    _select( ).
    _process( ).
    _display( ).

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

  METHOD _display.

    cl_demo_output=>display( t100_tab ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test( )->start( ).
