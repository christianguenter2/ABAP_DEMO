*&---------------------------------------------------------------------*
*& Report  Z_TEST_SY_SUBRC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_sy_subrc.

*&---------------------------------------------------------------------*
*&      Form  _3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM _3.

  sy-subrc = 4.

ENDFORM.                    "_3

*----------------------------------------------------------------------*
*       CLASS lcl_test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      get
        RETURNING value(r_instance) TYPE REF TO lcl_test.

    METHODS:
      start.

  PRIVATE SECTION.

    METHODS:
      _1,
      _2.

    CLASS-METHODS: _4.

ENDCLASS.                    "lcl_test DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test IMPLEMENTATION.

  METHOD get.

    CREATE OBJECT r_instance.

  ENDMETHOD.                    "get

  METHOD start.

    _1( ).
    _2( EXCEPTIONS OTHERS = 0 ).
    PERFORM _3.
    _4( ).
    CALL FUNCTION 'Z_TEST_SY_SUBRC'.

  ENDMETHOD.                    "start

  METHOD _1.

    sy-subrc = 4.

  ENDMETHOD.                    "_1

  METHOD _2.

    sy-subrc = 4.

  ENDMETHOD.                    "_2

  METHOD _4.

    sy-subrc = 4.

  ENDMETHOD.                    "_4

ENDCLASS.                    "lcl_test IMPLEMENTATION

START-OF-SELECTION.
  lcl_test=>get( )->start( ).
