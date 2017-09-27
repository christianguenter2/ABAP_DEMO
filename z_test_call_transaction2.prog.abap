*&---------------------------------------------------------------------*
*& Report z_test_call_transaction2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_call_transaction2.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

    cl_demo_output=>display( zcl_test_global=>flag ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW controller( )->start( ).
