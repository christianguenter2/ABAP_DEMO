REPORT z_test_teched.

SELECT * INTO TABLE @DATA(lt_pa0002)
         FROM z_test_cds_view_teched
         UP TO 200 ROWS.

LOOP AT lt_pa0002 ASSIGNING FIELD-SYMBOL(<pa0002>).
  " do something
ENDLOOP.

cl_salv_table=>factory(
  IMPORTING
    r_salv_table   = DATA(alv)
  CHANGING
    t_table        = lt_pa0002 ).

alv->display( ).
