REPORT z_test_adt_regex_dump.

DATA: *text TYPE string.

*----------------------------------------------------------------------*
*       CLASS test_regex DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS test_regex DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

  PRIVATE SECTION.
    METHODS:
      _do_something
        IMPORTING
          i_test TYPE csequence.

ENDCLASS.                    "test_regex DEFINITION

*----------------------------------------------------------------------*
*       CLASS test_regex IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS test_regex IMPLEMENTATION.

  METHOD start.

    _do_something( *text ).
    _do_something( *text ).
    _do_something( *text ).

  ENDMETHOD.                    "start

  METHOD _do_something.

  ENDMETHOD.                    "_do_something

ENDCLASS.                    "test_regex IMPLEMENTATION
