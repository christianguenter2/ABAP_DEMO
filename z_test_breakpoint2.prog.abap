*&---------------------------------------------------------------------*
*& Report z_test_breakpoint2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_breakpoint2.

SELECT FROM t100
       FIELDS *
       INTO TABLE @DATA(lt_t100)
       UP TO 100 ROWS.

MESSAGE 'Test' TYPE 'I'.

cl_demo_output=>display( lt_t100 ).
