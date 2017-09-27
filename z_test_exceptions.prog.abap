*&---------------------------------------------------------------------*
*& Report z_test_exceptions
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_exceptions.

CLASS test_exceptions DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

  PRIVATE SECTION.
    METHODS _test
      IMPORTING
        i_text          TYPE string
      RETURNING
        VALUE(r_result) TYPE string
      EXCEPTIONS
        error
        test_2.

ENDCLASS.

CLASS test_exceptions IMPLEMENTATION.

  METHOD start.

    _test( EXPORTING i_text = |Hallo Welt!|
           EXCEPTIONS OTHERS = 1 ).

  ENDMETHOD.


  METHOD _test.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_exceptions( )->start( ).
