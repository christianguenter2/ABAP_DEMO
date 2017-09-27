*"* use this source file for your ABAP unit test classes


CLASS test_class DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      first_test FOR TESTING RAISING cx_static_check,
      second_test FOR TESTING RAISING cx_static_check.
    METHODS _assert.

ENDCLASS.


CLASS test_class IMPLEMENTATION.

  METHOD first_test.

    _assert( ).

  ENDMETHOD.

  METHOD second_test.

    cl_abap_unit_assert=>assert_true( abap_true  ).

  ENDMETHOD.


  METHOD _assert.

    cl_abap_unit_assert=>assert_true( abap_true  ).

  ENDMETHOD.

ENDCLASS.
