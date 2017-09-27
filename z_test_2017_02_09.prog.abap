*&---------------------------------------------------------------------*
*& Report z_test_2017_02_09
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_02_09.

CLASS test DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

ENDCLASS.

CLASS test IMPLEMENTATION.

  METHOD start.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @DATA(t100_tab)
           UP TO 100 ROWS.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).

    ENDLOOP.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(alv)     " Basisklasse einfache ALV Tabellen
          CHANGING
            t_table        = t100_tab ).







        alv->display( ).

      CATCH cx_salv_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  NEW test( )->start( ).
