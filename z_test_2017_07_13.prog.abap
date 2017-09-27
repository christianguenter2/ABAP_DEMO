*&---------------------------------------------------------------------*
*& Report z_test_2017_07_13
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_07_13.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  new controller( )->start( ).
