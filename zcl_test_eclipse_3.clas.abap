CLASS zcl_test_eclipse_3 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: start.

ENDCLASS.


CLASS zcl_test_eclipse_3 IMPLEMENTATION.

  METHOD start.

    SELECT FROM t100
           FIELDS sprsl
           INTO TABLE @DATA(table)
           UP TO 100 ROWS.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(alv)
          CHANGING
            t_table      = table ).

        alv->display( ).

      CATCH cx_salv_msg INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
