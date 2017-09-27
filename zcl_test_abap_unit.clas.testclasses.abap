*"* use this source file for your ABAP unit test classes

CLASS test_abap_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: f_cut TYPE REF TO zcl_test_abap_unit.
    METHODS:
      setup,
      first_test FOR TESTING RAISING cx_static_check,
      second_test FOR TESTING RAISING cx_static_check,
      third_test FOR TESTING RAISING cx_static_check,
      fourth_test FOR TESTING RAISING cx_static_check,
      fifth FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS test_abap_unit IMPLEMENTATION.

  METHOD first_test.

    f_cut->start( ).

  ENDMETHOD.

  METHOD second_test.

    f_cut->start2( ).

  ENDMETHOD.

  METHOD setup.

    f_cut = NEW zcl_test_abap_unit( ).

  ENDMETHOD.

  METHOD third_test.

*    cl_abap_unit_assert=>fail( ).

  ENDMETHOD.

  METHOD fourth_test.

*    cl_abap_unit_assert=>assert_true( ).

  ENDMETHOD.

  METHOD fifth.

    cl_abap_unit_assert=>assert_equals( act = 1
                                        exp = 1
                                        msg = 'msg' ).

  ENDMETHOD.

ENDCLASS.
