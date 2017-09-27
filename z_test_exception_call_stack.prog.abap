*&---------------------------------------------------------------------*
*& Report z_test_exception_call_stack
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_exception_call_stack.

CLASS lcx_error DEFINITION CREATE PUBLIC
                INHERITING FROM cx_no_check.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          textid   LIKE textid OPTIONAL
          previous LIKE previous OPTIONAL,

      display.

  PRIVATE SECTION.

    DATA:
      callstack    TYPE abap_callstack,
      lt_callstack TYPE sys_callst.

ENDCLASS.

CLASS lcl_application DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

  PRIVATE SECTION.
    METHODS:
      _do_something.

ENDCLASS.

CLASS lcx_error IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid = textid previous = previous ).

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack    = callstack        " ABAP-Aufrufstack
        et_callstack = lt_callstack.    " System Callstack Tabelle

  ENDMETHOD.

  METHOD display.

    cl_demo_output=>write( callstack ).
    cl_demo_output=>write( lt_callstack ).
    cl_demo_output=>display(  ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_application IMPLEMENTATION.

  METHOD start.

    TRY.
        _do_something( ).

      CATCH lcx_error INTO DATA(error).

        error->display( ).

    ENDTRY.

  ENDMETHOD.

  METHOD _do_something.

    RAISE EXCEPTION TYPE lcx_error.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_application( )->start( ).
