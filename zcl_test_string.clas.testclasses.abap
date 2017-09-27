*"* use this source file for your ABAP unit test classes

CLASS test_string DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA: act_result TYPE stringtab,
          _input     TYPE string.

    METHODS:
      construction FOR TESTING RAISING cx_static_check,
      empty_split FOR TESTING RAISING cx_static_check,
      non_empty_split FOR TESTING RAISING cx_static_check,

      _given_string
        IMPORTING
          i_input TYPE string,

      _when_string_is_splitted,

      _then_the_result_should_be
        IMPORTING
          exp_result TYPE stringtab.

ENDCLASS.


CLASS test_string IMPLEMENTATION.

  METHOD construction.

    TYPES: BEGIN OF ty_data,
             i TYPE i,
             s TYPE string,
           END OF ty_data,
           tty_data TYPE STANDARD TABLE OF ty_data
                         WITH NON-UNIQUE DEFAULT KEY.

    DATA(string) = NEW zcl_test_string( `Hallo Welt!` ).

  ENDMETHOD.

  METHOD non_empty_split.

    _given_string( `12345` ).

    _when_string_is_splitted( ).

    _then_the_result_should_be( VALUE stringtab( ( `1` ) ( `2` ) ( `3` ) ( `4` ) ( `5` ) ) ).

  ENDMETHOD.


  METHOD _given_string.

    _input = i_input.

  ENDMETHOD.


  METHOD _when_string_is_splitted.

    act_result = NEW zcl_test_string( _input )->split( ).

  ENDMETHOD.


  METHOD _then_the_result_should_be.

    cl_abap_unit_assert=>assert_equals( act = act_result
                                        exp = exp_result ).

  ENDMETHOD.

  METHOD empty_split.

    _given_string( || ).

    _when_string_is_splitted( ).

    _then_the_result_should_be( VALUE stringtab(  ) ).

  ENDMETHOD.

ENDCLASS.
