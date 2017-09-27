*&---------------------------------------------------------------------*
*& Report  Z_TEST_ABAP_BOOL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_abap_bool.

*----------------------------------------------------------------------*
*       CLASS lcl_test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: start,
                   is_one_eq_two RETURNING value(r_one_eq_two) TYPE abap_bool.
ENDCLASS.                    "lcl_test DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test IMPLEMENTATION.
  METHOD start.
    IF is_one_eq_two( ) = abap_false.
      WRITE: `False`.
    ELSE.
      WRITE: `True`.
    ENDIF.
  ENDMETHOD.                    "start

  METHOD is_one_eq_two.
    r_one_eq_two = boolc( 1 = 2 ).
  ENDMETHOD.                    "is_one_eq_two
ENDCLASS.                    "lcl_test IMPLEMENTATION

START-OF-SELECTION.
  lcl_test=>start( ).
