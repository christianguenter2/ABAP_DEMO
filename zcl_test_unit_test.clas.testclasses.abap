*"* use this source file for your ABAP unit test classes

CLASS test_class DEFINITION FINAL FOR TESTING
INHERITING FROM cl_aunit_assert
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      first_test FOR TESTING RAISING cx_static_check.
ENDCLASS.                    "test_class DEFINITION



*----------------------------------------------------------------------*
*       CLASS test_class IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS test_class IMPLEMENTATION.
  METHOD first_test.
    cl_abap_unit_assert=>fail( 'Implement your first test here' ).
  ENDMETHOD.                    "first_test
ENDCLASS.                    "test_class IMPLEMENTATION``
