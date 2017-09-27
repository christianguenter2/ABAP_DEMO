*&---------------------------------------------------------------------*
*& Report z_test_numc
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_numc.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

    DATA(text) = REDUCE string( INIT result = ||
                                FOR wa IN VALUE stringtab( FOR	 x = 1
                                                           THEN  x + 1
                                                           WHILE x <= 20
                                                           ( |{ CONV numc2( x ) }| ) )
                                NEXT result = result
                                  && cl_abap_char_utilities=>cr_lf
                                  && wa ).

    cl_demo_output=>display( text ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW controller( )->start( ).
