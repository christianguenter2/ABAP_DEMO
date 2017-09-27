*"* use this source file for your ABAP unit test classes

CLASS test_template_method DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_test_case,
             type   TYPE string,
             result TYPE string,
           END OF ty_test_case,
           tty_test_case TYPE HASHED TABLE OF ty_test_case
                              WITH UNIQUE KEY type.
    METHODS:
      setup,
      generic FOR TESTING RAISING cx_static_check.

    DATA: f_cut      TYPE REF TO zcl_test_template_method,
          test_cases TYPE tty_test_case.

ENDCLASS.

CLASS test_template_method IMPLEMENTATION.

  METHOD setup.

    f_cut = NEW zcl_test_template_method( ).

  ENDMETHOD.

  DEFINE _test_cases_are.

    IF &2 = '->'.
      INSERT VALUE #( type   = zcl_test_template_method=>co_type-&1
                      result = &3 ) INTO TABLE test_cases.
    ENDIF.

  END-OF-DEFINITION.

  METHOD generic.

    _test_cases_are:
      one   '->' `one`,
      two   '->' `two`,
      three '->' `three`,
      four  '->' `four`,
      five  '->' `five`,
      six   '->' `six`,
      seven '->' `seven`.

    LOOP AT test_cases ASSIGNING FIELD-SYMBOL(<test_case>).

      DATA(result) = f_cut->run( <test_case>-type ).

      cl_abap_unit_assert=>assert_equals( act = result
                                          exp = <test_case>-result
                                          msg = |given { <test_case>-type } expected { <test_case>-result } but result is { result }| ).

    ENDLOOP.

  ENDMETHOD.


ENDCLASS.
