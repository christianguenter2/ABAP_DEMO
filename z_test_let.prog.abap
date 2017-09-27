*&---------------------------------------------------------------------*
*& Report z_test_let
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_let.

CLASS test DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS: get RETURNING VALUE(r_count) TYPE i.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA m_count TYPE i.

ENDCLASS.

CLASS test IMPLEMENTATION.

  METHOD get.

    m_count = m_count + 1.
    r_count = m_count.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA(table) = VALUE stringtab( ( `Test` ) ( `Test` ) ( `Test` ) ).

  DATA(table2) = VALUE stringtab( FOR line IN table
                                  LET x = test=>get( )
                                  IN ( line && '_' && x ) ).

  cl_demo_output=>display( table2 ).
