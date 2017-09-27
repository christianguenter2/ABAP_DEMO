*&---------------------------------------------------------------------*
*& Report  Z_TEST_O2_CALL_SCREEN
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_o2_call_screen.

DATA: tool_ref TYPE REF TO cl_o2_page.

CREATE OBJECT tool_ref.

CALL FUNCTION 'O2_CALL_SCREEN'
  EXPORTING
    p_tool_ref = tool_ref
    p_is_page  = ' '.
