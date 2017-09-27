*&---------------------------------------------------------------------*
*& Report z_test_conway
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_conway.

CLASS test_conway DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      init_board FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS game_board DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          size TYPE i.

ENDCLASS.

CLASS test_conway IMPLEMENTATION.

  METHOD init_board.

    DATA(game_board) = NEW game_board( size = 3 ).

    cl_abap_unit_assert=>assert_bound( game_board ).

  ENDMETHOD.

ENDCLASS.

CLASS game_board IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.

ENDCLASS.
