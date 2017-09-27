*&---------------------------------------------------------------------*
*& Report z_test_750_4
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_750_4.

CLASS lcl_test DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_test IMPLEMENTATION.

  METHOD start.

    DATA: co_title TYPE string VALUE `Test`.

    ASSIGN ('CO_TITLE') TO FIELD-SYMBOL(<title>).
    <title> = |New title|. "RUNTIME ERROR

    cl_demo_output=>display( co_title ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_test( )->start( ).
