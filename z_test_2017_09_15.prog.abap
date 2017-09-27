*&---------------------------------------------------------------------*
*& Report z_test_2017_09_15
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_09_15.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

ENDCLASS.


CLASS controller IMPLEMENTATION.

  METHOD start.

    NEW zcl_test_2017_09_15( )->start( ).

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.
  NEW controller( )->start( ).
