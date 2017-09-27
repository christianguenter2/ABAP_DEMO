*&---------------------------------------------------------------------*
*& Report Z_TEST_750_1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_750_1.

CLASS test DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS: run.

ENDCLASS.

CLASS test IMPLEMENTATION.

  METHOD run.

    SELECT FROM t100
           FIELDS arbgb, text
           WHERE arbgb LIKE 'B%'
        UNION
    SELECT FROM t100
           FIELDS arbgb, text
           WHERE arbgb LIKE 'A%'
           INTO TABLE @DATA(table).

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(alv)
          CHANGING
            t_table        = table ).
      CATCH cx_salv_msg INTO DATA(error).
        MESSAGE error TYPE 'I'.
    ENDTRY.

    alv->display( ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test( )->run( ).
