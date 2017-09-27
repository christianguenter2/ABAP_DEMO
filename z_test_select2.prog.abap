*&---------------------------------------------------------------------*
*& Report z_test_select2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_select2.

CLASS test_refactoring DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS: create.
    METHODS: start.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS test_refactoring IMPLEMENTATION.

  METHOD create.

  ENDMETHOD.

  METHOD start.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  test_refactoring=>create( )->start( ).
