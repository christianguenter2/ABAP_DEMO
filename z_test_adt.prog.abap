REPORT z_test_adt.

SELECT pernr, vorna, nachn, inits,
       CASE pernr
         WHEN '00012667'
         THEN 'alpha geek'
         ELSE 'kein geek'
       END AS function
       FROM pa0002
       INTO TABLE @DATA(lt_data)
       UP TO 200 ROWS
       where pernr = '00012667'.

LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<data>).

ENDLOOP.

cl_salv_table=>factory(
  IMPORTING
    r_salv_table   = DATA(alv)    " Basisklasse einfache ALV Tabellen
  CHANGING
    t_table        = lt_data
).

alv->display( ).
