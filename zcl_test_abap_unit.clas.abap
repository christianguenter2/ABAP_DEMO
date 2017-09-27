CLASS zcl_test_abap_unit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      start,
      start2.

  PRIVATE SECTION.
    METHODS _do_something.
    METHODS _test
      IMPORTING
        i_text        TYPE string
      EXPORTING
        e_text        TYPE string
      CHANGING
        c_text        TYPE string
      RETURNING
        VALUE(r_text) TYPE string.

ENDCLASS.



CLASS zcl_test_abap_unit IMPLEMENTATION.

  METHOD start.

    _do_something( ).

  ENDMETHOD.

  METHOD _do_something.

    DATA: t TYPE string.

    _test( EXPORTING i_text = | |
           IMPORTING e_text = DATA(z)
           CHANGING  c_text = t
           RECEIVING r_text = DATA(x) ).

    DATA(table) = VALUE stringtab( ( ) ).

  ENDMETHOD.

  METHOD start2.

    _do_something( ).

  ENDMETHOD.


  METHOD _test.

  ENDMETHOD.

ENDCLASS.
