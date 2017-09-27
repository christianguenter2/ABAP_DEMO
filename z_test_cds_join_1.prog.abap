*&---------------------------------------------------------------------*
*& Report z_test_cds_join_1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_cds_join_1.

SELECT *
       FROM z_demo_cds_join_1
       INTO TABLE @DATA(table).

SELECT *
       FROM z_demo_cds_join_2
       INTO TABLE @DATA(table2).

ASSERT table = table2.

cl_demo_output=>display( table ).
