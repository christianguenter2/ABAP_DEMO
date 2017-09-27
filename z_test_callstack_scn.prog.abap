REPORT z_test_callstack_scn.

CLASS lcl_stack DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS start.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_stack IMPLEMENTATION.


  METHOD start.

*    DATA(callstack) = cl_abap_get_call_stack=>get_call_stack( ).
*    DATA(formatted_callstack) = cl_abap_get_call_stack=>format_call_stack( callstack  ).

    DATA:  callstack    TYPE abap_callstack,
           et_callstack TYPE sys_callst.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
*      EXPORTING
*        max_level    = 0
      IMPORTING
        callstack    = callstack
        et_callstack = et_callstack.    " System Callstack Table

    cl_demo_output=>write( callstack ).
    cl_demo_output=>write( et_callstack ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  lcl_stack=>start( ). lcl_stack=>start( ).

  cl_demo_output=>display(  ).
