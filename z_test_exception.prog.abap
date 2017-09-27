*&---------------------------------------------------------------------*
*& Report z_test_exception
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_exception.


CLASS lcl_test DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      first.

  PRIVATE SECTION.
    CLASS-METHODS:
      second,

      third
        EXCEPTIONS
          error.

ENDCLASS.

CLASS lcl_test IMPLEMENTATION.

  METHOD first.

    second( ).

  ENDMETHOD.

  METHOD second.

    third( ).

  ENDMETHOD.

  METHOD third.

    RAISE error.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_test=>first( EXCEPTIONS OTHERS = 1 ).
