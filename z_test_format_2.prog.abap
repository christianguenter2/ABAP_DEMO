*&---------------------------------------------------------------------*
*& Report z_test_format_2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_format_2.

CLASS lcl_test DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      start.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_test IMPLEMENTATION.

  METHOD start.

    DATA: table TYPE stringtab.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(alv)    " Basisklasse einfache ALV Tabellen
          CHANGING
            t_table      = table ).

      CATCH cx_salv_error INTO DATA(error).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_test=>start( ).
