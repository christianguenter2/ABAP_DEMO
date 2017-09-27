*&---------------------------------------------------------------------*
*& Report z_test_kata
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_kata.

CLASS lcl_test DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_test IMPLEMENTATION.

  METHOD start.

    SELECT *
           FROM t100
           INTO TABLE @DATA(t100_tab)
           UP TO 100 ROWS.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).
      "do some processing
    ENDLOOP.

    cl_demo_output=>display( t100_tab ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_test( )->start( ).
