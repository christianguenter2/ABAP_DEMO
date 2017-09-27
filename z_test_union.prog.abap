*&---------------------------------------------------------------------*
*& Report z_test_union
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_union.

CLASS test_union DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

ENDCLASS.

CLASS test_union IMPLEMENTATION.

  METHOD start.

    SELECT FROM toa01
           FIELDS archiv_id, arc_doc_id
           UNION
    SELECT FROM toa02
           FIELDS archiv_id, arc_doc_id
           UNION
    SELECT FROM toa03
           FIELDS archiv_id, arc_doc_id
           INTO TABLE @DATA(lt_toa).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_union( )->start( ).
