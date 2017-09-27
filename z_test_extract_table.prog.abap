*&---------------------------------------------------------------------*
*& Report z_test_extract_table
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_extract_table.

CLASS test_extract_table DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS: start.

ENDCLASS.

CLASS test_extract_table IMPLEMENTATION.

  METHOD start.

    SELECT FROM t100
           FIELDS msgnr, sprsl
           INTO TABLE @DATA(t100_table)
           UP TO 100 ROWS.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_extract_table( )->start( ).
