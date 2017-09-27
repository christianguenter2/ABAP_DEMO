*&---------------------------------------------------------------------*
*& Report z_test_teched3
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_teched3.

CLASS test_refactoring DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS test_refactoring IMPLEMENTATION.

  METHOD start.

    DATA(test) = VALUE stringtab( ( ) ).

    LOOP AT test INTO DATA(y).

    ENDLOOP.

    LOOP AT test ASSIGNING FIELD-SYMBOL(<test>).

    ENDLOOP.

    DATA(x) = ||.
    DATA(y) = ||.


  IMPORTING
    VALUE(PI_KORRNUM) LIKE E070-TRKORR
    VALUE(WI_E071) LIKE E071
    VALUE(WI_SIMULATION) LIKE TRPARI-W_SIMULAT DEFAULT ' '
    VALUE(WI_SUPPRESS_KEY_CHECK) LIKE TRPARI-W_NO_CHECK DEFAULT ' '
  TABLES
    WT_E071K LIKE E071K OPTIONAL

    call function 'TR_APPEND_TO_COMM'
      EXPORTING
        pi_korrnum                     = pi_korrnum    " Order, to be appended
        wi_e071                        = wi_e071    " Object key
*        wi_simulation                  = wi_simulation    " Flag, 'X' - no database update
*        wi_suppress_key_check          = wi_suppress_key_check    " Flag, whether key syntax check suppressed
*      TABLES
*        wt_e071k                       = wt_e071k    " Input/output table E071K

*    _test( EXPORTING i_test = ''
*           IMPORTING e_test = DATA(x)
*           RECEIVING r_test = DATA(y) ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_refactoring( )->start( ).
