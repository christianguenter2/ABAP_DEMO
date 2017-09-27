*&---------------------------------------------------------------------*
*& Report z_test_2017_02_06
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_02_06.

CLASS test_2017_02_06 DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

ENDCLASS.

CLASS test_2017_02_06 IMPLEMENTATION.

  METHOD start.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @DATA(t100_tab)
           UP TO 100 ROWS.


  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_2017_02_06( )->start( ).
