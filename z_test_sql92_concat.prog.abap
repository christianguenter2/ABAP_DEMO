*&---------------------------------------------------------------------*
*& Report z_test_sql92_concat
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_sql92_concat.

SELECT FROM pa0002
       FIELDS CONCAT( CONCAT( pernr, CONCAT( vorna, nachn ) ), begda ) AS info
       INTO TABLE @DATA(table)
       UP TO 100 ROWS.

DATA(text) = 'test'.

SELECT FROM pa0002
       FIELDS vorna && @text && nachn AS info
       INTO TABLE @DATA(table2)
       UP TO 100 ROWS.

SELECT FROM pa0002
       FIELDS vorna && @text && nachn AS info
       INTO TABLE @DATA(table3)
       UP TO 100 ROWS.

TRY.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table   = DATA(alv)
      CHANGING
        t_table        = table2 ).

    alv->display( ).

  CATCH cx_salv_msg INTO DATA(error).    "
    MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
ENDTRY.
