*&---------------------------------------------------------------------*
*& Report z_test_teched
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_teched.

CLASS lcl_main DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS create
      RETURNING
        VALUE(r_result) TYPE REF TO lcl_main.

    METHODS: run.

ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD create.

    CREATE OBJECT r_result.

  ENDMETHOD.

  METHOD run.

    DATA(lo_t100) = NEW zcl_t100_teched( ).

    DATA(t100_tab) = lo_t100->get_item_from_db( ).

    TRY.

        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(alv)
          CHANGING
            t_table        = t100_tab ).

        alv->display( ).
      CATCH cx_salv_msg INTO DATA(error).
        MESSAGE error  TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_main=>create( )->run( ).
