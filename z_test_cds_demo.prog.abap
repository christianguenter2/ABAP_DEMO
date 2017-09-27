REPORT z_test_cds_demo.

TYPE-POOLS: icon.

SELECT spras && '(Test)' AS spras,
       text AS text,
       CASE spras
         WHEN 'Deutsch' THEN '@09@'
         ELSE '@08@'
       END AS icon
       FROM z_test_cds_view
       INTO TABLE @DATA(lt_data)
       UP TO 100 ROWS.

cl_salv_table=>factory(
  IMPORTING
    r_salv_table   = DATA(alv)
  CHANGING
    t_table        = lt_data ).

alv->display( ).
