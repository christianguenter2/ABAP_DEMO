*&---------------------------------------------------------------------*
*& Report z_test_tvarvc_select
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_tvarvc_select.

PARAMETERS: customer TYPE kunnr OBLIGATORY.

CLASS test DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

  PRIVATE SECTION.
    DATA: customers_range TYPE RANGE OF kunnr,
          customers       TYPE STANDARD TABLE OF tvarvc.

    METHODS:
      _first_select,
      _second_select,
      _range_output,
      _customers_output,
      _customer_check_output,
      _display.

ENDCLASS.

CLASS test IMPLEMENTATION.

  METHOD start.

    _first_select( ).
    _range_output( ).
    _second_select( ).
    _customers_output( ).
    _customer_check_output( ).
    _display( ).

  ENDMETHOD.

  METHOD _first_select.

    SELECT FROM tvarvc
           FIELDS sign AS sign,
                  opti AS option,
                  low  AS low,
                  high AS high
           WHERE name = 'ZSD_CN_JD_NUMBER_REL_CUSTOMERS'
           INTO TABLE @customers_range.

  ENDMETHOD.

  METHOD _range_output.

    cl_demo_output=>write( customers_range ).

  ENDMETHOD.

  METHOD _second_select.

    SELECT FROM tvarvc
           FIELDS *
           WHERE low IN @customers_range
           INTO TABLE @customers.

  ENDMETHOD.

  METHOD _customers_output.

    cl_demo_output=>write( customers ).

  ENDMETHOD.

  METHOD _customer_check_output.

    cl_demo_output=>write( COND #( WHEN customer IN customers_range THEN |Customer { customer } is in range |
                                   ELSE |Customer { customer } is not in range!!!| ) ).

  ENDMETHOD.

  METHOD _display.

    cl_demo_output=>display( ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test( )->start( ).
