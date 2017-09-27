*&---------------------------------------------------------------------*
*& Report  Z_TEST_ENTER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_enter.

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor,
             start.
ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.                    "constructor

  METHOD start.
    TYPES: BEGIN OF ty_data,
             i TYPE i,
             s TYPE string,
           END OF ty_data,
           tty_data TYPE STANDARD TABLE OF ty_data.

    DATA: lo_alv  TYPE REF TO cl_salv_table,
          lt_data TYPE tty_data.

    cl_salv_table=>factory(
      IMPORTING
        r_salv_table   = lo_alv
      CHANGING
        t_table        = lt_data ).

    lo_alv->get_columns( )->get_column( `i` ).
    lo_alv->get_columns( )->get_column( `s` ).

    lo_alv->display( ).
  ENDMETHOD.                    "start
ENDCLASS.                    "lcl_application IMPLEMENTATION

START-OF-SELECTION.
  DATA: lo_application TYPE REF TO lcl_application.
  CREATE OBJECT lo_application.
  lo_application->start( ).
