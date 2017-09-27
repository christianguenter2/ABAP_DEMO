*&---------------------------------------------------------------------*
*& Report z_test_reference_3
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_reference_3.

CLASS test_reference DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

  PRIVATE SECTION.

    METHODS: _do_something
      IMPORTING
        i_text TYPE REF TO string.

ENDCLASS.

CLASS test_reference IMPLEMENTATION.

  METHOD start.

    DATA: text TYPE string VALUE `Test` ##NO_TEXT.

    _do_something( REF #( text ) ).

    cl_demo_output=>write( text ).
    cl_demo_output=>display( ).

  ENDMETHOD.


  METHOD _do_something.

    i_text->* = |do something!!!|.
    cl_demo_output=>write( i_text->* ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_reference( )->start( ).
