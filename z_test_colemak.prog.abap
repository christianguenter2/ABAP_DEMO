*&---------------------------------------------------------------------*
*& Report  Z_TEST_COLEMAK
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_colemak.

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: start.
ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD start.
    text --__9|{ }|.
  ENDMETHOD.                    "start
ENDCLASS.                    "lcl_application IMPLEMENTATION

START-OF-SELECTION.
 lcl_application=>start( ).
