REPORT z_test_cds_select.

SELECT *
        FROM z_test_cds_view
        INTO TABLE @DATA(lt_data).

cl_salv_table=>factory(
  IMPORTING
    r_salv_table   = DATA(alv)     " Basis Class Simple ALV Tables
  CHANGING
    t_table        = lt_data
).

alv->display( ).
