*&---------------------------------------------------------------------*
*& Report z_test_2017_01_24
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_01_24.

CLASS test_t100 DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    	METHODS: start.

  PRIVATE SECTION.
    DATA: lt_t100 TYPE STANDARD TABLE OF t100.

    METHODS:
      _select,
      _display.

ENDCLASS.

CLASS test_t100 IMPLEMENTATION.

  METHOD start.

    _select( ).
    _display( ).

  ENDMETHOD.

  METHOD _display.

    cl_demo_output=>display( lt_t100 ).

  ENDMETHOD.

  METHOD _select.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @lt_t100
           UP TO 100 ROWS.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_t100( )->start( ).
