*----------------------------------------------------------------------*
*       CLASS ZCL_TEST_ITERATOR DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_test_iterator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    types: ty_reftab TYPE STANDARD TABLE OF REF TO data.
    CLASS-METHODS: make_iter.
  PROTECTED SECTION.
    CLASS-METHODs: do_iter IMPORTING it_dref TYPE ty_reftab.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TEST_ITERATOR IMPLEMENTATION.


  METHOD do_iter.
    data: string TYPE value.
    FIELD-SYMBOLS: <ref> like line of it_dref,
                   <value> TYPE any.

    LOOP AT it_dref ASSIGNING <ref>.
      ASSIGN <ref>->* to <value>.
      WRITE: <value>.
    ENDLOOP.
  ENDMETHOD.                    "DO_ITER


  METHOD make_iter.
    DATA: itab TYPE ty_reftab,
          dref LIKE LINE OF itab,
          string TYPE string VALUE 'Dies ist ein Test',
          int TYPE int4 VALUE 123,
          char TYPE char40 VALUE 'Ein weiterer Test'.

    _append_to_itab: string,
                     int,
                     char.

    DO_ITER( itab ).
  ENDMETHOD.                    "make_ITER
ENDCLASS.
