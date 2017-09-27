*&---------------------------------------------------------------------*
*& Report z_test_2017_06_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_06_02.

CLASS test DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

  PRIVATE SECTION.
    DATA: customers  TYPE RANGE OF kunnr,
          tvarvc_tab TYPE STANDARD TABLE OF tvarvc.

    METHODS:
      _select_customers,
      _select_tvarvc,
      _write_customers,
      _write_tvarvc,
      _display.

ENDCLASS.

CLASS test IMPLEMENTATION.

  METHOD start.

    _select_customers( ).
    _write_customers( ).
    _select_tvarvc( ).
    _write_tvarvc( ).
    _display( ).

  ENDMETHOD.

  METHOD _display.

    cl_demo_output=>display( ).

  ENDMETHOD.

  METHOD _write_tvarvc.

    cl_demo_output=>write( tvarvc_tab ).

  ENDMETHOD.

  METHOD _write_customers.

    cl_demo_output=>write( customers ).

  ENDMETHOD.

  METHOD _select_customers.

    SELECT FROM tvarvc
           FIELDS sign,
                  opti,
                  low,
                  high
           WHERE name = 'ZSD_CN_JD_NUMBER_REL_CUSTOMERS'
           INTO TABLE @customers.

  ENDMETHOD.

  METHOD _select_tvarvc.

    SELECT FROM tvarvc
           FIELDS *
           WHERE low IN @customers
           INTO TABLE @tvarvc_tab.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test( )->start( ).
