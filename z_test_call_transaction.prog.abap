*&---------------------------------------------------------------------*
*& Report z_test_call_transaction
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_call_transaction.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

    DATA(test) = |Hallo Welt|.

    EXPORT test = test TO MEMORY ID 'Z_TEST'.

    zcl_test_global=>flag = abap_true.

    CALL TRANSACTION 'Z_TEST_CALL'.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW controller( )->start( ).
