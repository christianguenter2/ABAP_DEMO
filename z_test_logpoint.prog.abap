*&---------------------------------------------------------------------*
*& Report z_test_logpoint
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_logpoint.

CLASS test_log_point DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS: start.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_data,
             i TYPE i,
             s TYPE string,
           END OF ty_data,
           tty_data TYPE STANDARD TABLE OF ty_data
                    WITH NON-UNIQUE DEFAULT KEY.

    DATA: table TYPE tty_data.

ENDCLASS.

CLASS test_log_point IMPLEMENTATION.

  METHOD start.

    DATA(x) = |Test|.

    DO 5 TIMES.
      INSERT VALUE #( i = 1 s = `test` ) INTO TABLE table.
    ENDDO.

    SELECT *
           FROM t100
           INTO TABLE @DATA(t100_tab)
           UP TO 100 ROWS.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(alv)
          CHANGING
            t_table        = t100_tab ).

        alv->display( ).

      CATCH cx_salv_msg INTO DATA(error).    "
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_log_point( )->start( ).
