*&---------------------------------------------------------------------*
*& Report  Z_TEST_SCAN_2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_scan_2.

*----------------------------------------------------------------------*
*       CLASS lcl_test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: start.

  PRIVATE SECTION.
    CLASS-METHODS:
        a RETURNING value(r_bool) TYPE abap_bool,
        b RETURNING value(r_bool) TYPE abap_bool,
        c RETURNING value(r_bool) TYPE abap_bool,
        d.

ENDCLASS.                    "lcl_test DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test IMPLEMENTATION.
  METHOD start.
    IF a( ) = abap_true AND
       b( ) = abap_true AND
       c( ) = abap_true AND
       a( ) = abap_true AND
       b( ) = abap_true AND
       c( ) = abap_true.

    ENDIF.

    d( ).
  ENDMETHOD.                    "start

  METHOD a.
    r_bool = abap_true.
  ENDMETHOD.                    "a

  METHOD b.
    r_bool = abap_true.
  ENDMETHOD.                    "b

  METHOD c.
    r_bool = abap_true.
  ENDMETHOD.                    "c

  METHOD d.
    DATA: source_info TYPE tpda_curr_source_pos,
          source      TYPE tpda_ast_src_it.

    zcl_bc_debugger_scripts=>get_source_info_extended(
      EXPORTING
        i_source_info  = source_info    " TPDA; Sttaische Source Position für Navigation
      CHANGING
        ct_source      = source ).
  ENDMETHOD.                    "d
ENDCLASS.                    "lcl_test IMPLEMENTATION

START-OF-SELECTION.
  lcl_test=>start( ).

  IF sy-subrc =  0.

  ENDIF.
