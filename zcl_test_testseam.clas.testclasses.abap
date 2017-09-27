*"* use this source file for your ABAP unit test classes

CLASS test_testseam DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA m_cut TYPE REF TO zcl_test_testseam.
    METHODS:
      setup,
      test_start FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS test_testseam IMPLEMENTATION.

  METHOD setup.

    m_cut = NEW zcl_test_testseam( ).

  ENDMETHOD.

  METHOD test_start.

    TEST-INJECTION test_popup.
      r_answer-a = 'Y'.
    END-TEST-INJECTION.

    TEST-INJECTION test_popup2.
      r_answer-b = 'X'.
    END-TEST-INJECTION.

    cl_abap_unit_assert=>assert_equals( act = m_cut->start( )
                                        exp = VALUE zcl_test_testseam=>ty_answer( a = 'Y' b = 'X' ) ).

  ENDMETHOD.

ENDCLASS.
