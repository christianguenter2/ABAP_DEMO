*&---------------------------------------------------------------------*
*& Report z_test_2017_06_16
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_06_16.

CLASS test_union DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

ENDCLASS.

CLASS test_union IMPLEMENTATION.

  METHOD start.

    TYPES: BEGIN OF ty_t100,
             arbgb TYPE t100-arbgb,
             msgnr TYPE t100-msgnr,
             sprsl TYPE t100-sprsl,
             text  TYPE t100-text,
           END OF ty_t100,
           tty_t100 TYPE STANDARD TABLE OF ty_t100
                         WITH NON-UNIQUE DEFAULT KEY.

    DATA: t100_tab  TYPE tty_t100,
          t100_tab2 TYPE tty_t100.

    SELECT FROM t100
           FIELDS arbgb, msgnr, sprsl, text
           WHERE arbgb = '00'
           ORDER BY PRIMARY KEY
           INTO TABLE @t100_tab.

    SELECT FROM t100
           FIELDS arbgb, msgnr, sprsl, text
           WHERE arbgb = '01'
           ORDER BY PRIMARY KEY
           APPENDING TABLE @t100_tab.

    SELECT FROM t100
           FIELDS  arbgb, msgnr, sprsl, text
           WHERE arbgb = '00'
    UNION
    SELECT FROM t100
           FIELDS arbgb, msgnr, sprsl, text
           WHERE arbgb = '01'
           INTO TABLE @t100_tab2.

    SORT t100_tab.
    SORT t100_tab2.
    ASSERT t100_tab = t100_tab2.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_union( )->start( ).
