*&---------------------------------------------------------------------*
*& Report z_test_value_reference
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_value_reference.

CLASS test_class DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

  PRIVATE SECTION.
    METHODS: test_1
      IMPORTING
        VALUE(i_test) TYPE csequence
      EXPORTING
        VALUE(e_test) TYPE string
      CHANGING
        VALUE(c_test) TYPE string
      RETURNING
        VALUE(r_test) TYPE string.


ENDCLASS.

CLASS test_class IMPLEMENTATION.

  METHOD start.

    DATA: x TYPE string VALUE `Test`,
          y TYPE string VALUE `Test`,
          z TYPE string VALUE `changing`,
          r TYPE string.

    test_1( EXPORTING i_test = x
            IMPORTING e_test = y
            CHANGING  c_test = z
            RECEIVING r_test = r ).

    cl_demo_output=>write( x ).
    cl_demo_output=>write( y ).
    cl_demo_output=>write( z ).
    cl_demo_output=>write( r ).
    cl_demo_output=>display( ).

  ENDMETHOD.

  METHOD test_1.

    i_test = ''.
    cl_demo_output=>write( e_test ).
    CLEAR: c_test.
    cl_demo_output=>write( c_test ).
    e_test = '12345'.
    r_test = 'returning'.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_class( )->start( ).
