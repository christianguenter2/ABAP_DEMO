REPORT z_test_callstack_scn2.

*DATA(compiler) = cl_abap_compiler=>create( |Z_TEST_CALLSTACK_SCN| ).
*
*compiler->get_all_refs(
*  EXPORTING
*    p_local       = abap_true   " Local Classes also
**    p_testcode    = ' '    " Test Code
**    p_types       =     " Range of Tags
**    p_grades      =     " Range of Grades
**    p_no_includes =     " Reference Not per Include
**    p_extended    =     " Enhanced Breakdown of Where-Used List
**    p_internal    =     " Internal Objects Too
*  IMPORTING
*    p_result      = DATA(result)    " Table of References
**    p_error       =     " Syntax Error Occurred
**    p_errors      =     " Table with Messages
**    p_abort       =     " Analysis Cancelled
*).
*
*cl_demo_output=>display( result ).



cl_abap_parser=>get_tokens(
  EXPORTING
    source = |lcl_stack=>start( ). lcl_stack=>start( ).|
  IMPORTING
    tokens = DATA(tokens) ).

cl_demo_output=>display( tokens ).
