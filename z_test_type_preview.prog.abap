*&---------------------------------------------------------------------*
*& Report z_test_type_preview
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_type_preview.

SELECT * FROM t100
         INTO TABLE @DATA(t100_tab)
         UP TO 100 ROWS.