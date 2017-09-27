*&---------------------------------------------------------------------*
*& Report z_test_2016_11_03
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2016_11_03.

CLASS test_ DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

ENDCLASS.

CLASS test_ IMPLEMENTATION.

  METHOD start.

    SELECT *
           FROM t100
           INTO TABLE @DATA(t100_tab)
           UP TO 100 ROWS.

    cl_demo_output=>display( t100_tab ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_( )->start( ).
