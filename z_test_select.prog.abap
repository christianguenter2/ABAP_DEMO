*&---------------------------------------------------------------------*
*& Report z_test_select
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_select.

CLASS test_select DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

  PRIVATE SECTION.
    METHODS:
      _get_msgnr
        RETURNING VALUE(r_msgnr) TYPE t100-msgnr.

ENDCLASS.

CLASS test_select IMPLEMENTATION.

  METHOD start.

    SELECT FROM t100
           FIELDS *
           WHERE msgnr = @( _get_msgnr( ) )
           INTO TABLE @DATA(t100_tab)
           UP TO 100 ROWS.

    cl_demo_output=>display( t100_tab ).

  ENDMETHOD.

  METHOD _get_msgnr.

    r_msgnr = 100.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_select( )->start( ).
