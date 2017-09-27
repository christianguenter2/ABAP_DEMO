*&---------------------------------------------------------------------*
*& Report z_test_2017_01_09
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_01_09.

CLASS test DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

  PRIVATE SECTION.
    DATA: t100_tab TYPE STANDARD TABLE OF t100.

    METHODS:
      _select,
      _display.

ENDCLASS.

CLASS test IMPLEMENTATION.

  METHOD start.

    _select( ).
    _display( ).

  ENDMETHOD.

  METHOD _display.

    cl_demo_output=>display( t100_tab ).

  ENDMETHOD.

  METHOD _select.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @t100_tab
           UP TO 100 ROWS.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test( )->start( ).
