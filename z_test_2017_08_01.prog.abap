*&---------------------------------------------------------------------*
*& Report z_test_2017_08_01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_08_01.

CLASS test_file DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: run.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS _start.
    METHODS _stop.

ENDCLASS.

CLASS test_file IMPLEMENTATION.

  METHOD run.

    _start( ).
    _stop( ).

  ENDMETHOD.

  METHOD _start.

  ENDMETHOD.

  METHOD _stop.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_file( )->run( ).
