*&---------------------------------------------------------------------*
*& Report  Z_TEST_CALL_STACK
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_call_stack.

DATA: formatted_stack TYPE cl_abap_get_call_stack=>formatted_stack.

DATA: formatted_entry_stack   TYPE cl_abap_get_call_stack=>formatted_entry_stack.

formatted_stack = cl_abap_get_call_stack=>format_call_stack(  cl_abap_get_call_stack=>get_call_stack( ) ).
formatted_entry_stack = cl_abap_get_call_stack=>format_call_stack_with_struct(  cl_abap_get_call_stack=>get_call_stack( ) ).

IF sy-subrc = 0.

ENDIF.
