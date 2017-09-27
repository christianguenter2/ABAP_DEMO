*&---------------------------------------------------------------------*
*& Report z_test_hash_table2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_hash_table2.

CLASS test_hash_table DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS test_hash_table IMPLEMENTATION.

  METHOD start.

    DATA: t100_tab TYPE HASHED TABLE OF t100
                   WITH UNIQUE KEY sprsl arbgb msgnr.

    SELECT *
           FROM t100
           INTO TABLE @t100_tab
           UP TO 1000 ROWS
           ORDER BY PRIMARY KEY.

    cl_demo_output=>display( t100_tab ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_hash_table( )->start( ).
