REPORT z_test_sapgui_2.

DATA: alv   TYPE REF TO cl_salv_table,
      table TYPE STANDARD TABLE OF t100
                 WITH NON-UNIQUE DEFAULT KEY.

SELECT *
       FROM t100
       INTO TABLE table
       UP TO 100 ROWS.

TRY.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table   = alv
      CHANGING
        t_table        = table ).

    alv->display( ).
  CATCH cx_salv_msg.    " ALV: Allg. Fehlerklasse  mit Meldung
ENDTRY.
