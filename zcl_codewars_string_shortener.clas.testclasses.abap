*"* use this source file for your ABAP unit test classes
CLASS lcl_test_string_shortener DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA input_string TYPE string.
    DATA length TYPE i.
    DATA output_string TYPE string.
    DATA glue TYPE string.

    METHODS:
      empty_string FOR TESTING RAISING cx_static_check,
      negative_length FOR TESTING RAISING cx_static_check,
      example_one_from_codewars FOR TESTING RAISING cx_static_check,
      example_with_odd_string_length FOR TESTING RAISING cx_static_check,
      glue_not_exactly_in_the_middle FOR TESTING RAISING cx_static_check,
      custom_glue FOR TESTING RAISING cx_static_check,
      custom_glue_with_diff_length FOR TESTING RAISING cx_static_check,
      glue_not_fit_in_short_string FOR TESTING RAISING cx_static_check,
      minimum_glue FOR TESTING RAISING cx_static_check,
      given_empty_string,
      when_string_is_shortened,
      then_shortened_string_is_empty,
      given_non_empty_string,
      given_negative_length,
      then_string_is_not_changed,
      input_string_is
        IMPORTING
          i_input_string TYPE string,
      length_is
        IMPORTING
          i_length TYPE i,
      then_output_should_be
        IMPORTING
          i_output_string TYPE string,
      glue_is
        IMPORTING
          i_glue TYPE string.
ENDCLASS.


CLASS lcl_test_string_shortener IMPLEMENTATION.

  METHOD empty_string.
    given_empty_string( ).
    when_string_is_shortened( ).
    then_shortened_string_is_empty( ).
  ENDMETHOD.


  METHOD given_empty_string.
    input_string = ``.
  ENDMETHOD.


  METHOD when_string_is_shortened.
    IF glue IS INITIAL.
      output_string = zcl_codewars_string_shortener=>shorten( i_text_to_shorten = input_string
                                                              i_length          = length ).
    ELSE.
      output_string = zcl_codewars_string_shortener=>shorten( i_text_to_shorten = input_string
                                                              i_length          = length
                                                              i_glue            = glue ).
    ENDIF.
  ENDMETHOD.


  METHOD then_shortened_string_is_empty.
    cl_abap_unit_assert=>assert_initial( output_string ).
  ENDMETHOD.

  METHOD negative_length.
    given_non_empty_string( ).
    given_negative_length( ).
    when_string_is_shortened( ).
    then_string_is_not_changed( ).
  ENDMETHOD.


  METHOD given_non_empty_string.
    input_string = `Test`.
  ENDMETHOD.


  METHOD given_negative_length.
    length = -10.
  ENDMETHOD.


  METHOD then_string_is_not_changed.
    cl_abap_unit_assert=>assert_equals( msg = `Input string mustn't change`
                                        exp = input_string
                                        act = output_string ).
  ENDMETHOD.

  METHOD example_one_from_codewars.
    input_string_is( `The quick brown fox jumps over the lazy dog` ).
    length_is( 27 ).
    when_string_is_shortened( ).
    then_output_should_be( `The quick br...the lazy dog` ).
  ENDMETHOD.


  METHOD input_string_is.
    input_string = i_input_string.
  ENDMETHOD.


  METHOD length_is.
    length = i_length.
  ENDMETHOD.

  METHOD glue_is.
    glue = i_glue.
  ENDMETHOD.

  METHOD then_output_should_be.
    cl_abap_unit_assert=>assert_equals( msg = 'Output string is wrong'
                                        exp = i_output_string
                                        act = output_string ).
  ENDMETHOD.

  METHOD example_with_odd_string_length.
    input_string_is( `The quick brown fox jumps over the lazy dog!` ).
    length_is( 27 ).
    when_string_is_shortened( ).
    then_output_should_be( `The quick br...the lazy dog!` ).
  ENDMETHOD.

  METHOD glue_not_exactly_in_the_middle.
    input_string_is( `Test` ).
    length_is( 30 ).
    when_string_is_shortened( ).
    then_output_should_be( `Test` ).
  ENDMETHOD.

  METHOD custom_glue.
    input_string_is( `The quick brown fox jumps over the lazy dog` ).
    length_is( 27 ).
    glue_is( '---' ).
    when_string_is_shortened( ).
    then_output_should_be( `The quick br---the lazy dog` ).
  ENDMETHOD.

  METHOD custom_glue_with_diff_length.
    input_string_is( `The quick brown fox jumps over the lazy dog` ).
    length_is( 27 ).
    glue_is( '-----' ).
    when_string_is_shortened( ).
    then_output_should_be( `The quick b-----he lazy dog` ).
  ENDMETHOD.

  METHOD glue_not_fit_in_short_string.
    input_string_is('hello world' ).
    length_is(  5 ).
    glue_is( '....').
    when_string_is_shortened( ).
    then_output_should_be( 'hello' ).
  ENDMETHOD.

  METHOD minimum_glue.
    input_string_is('hello world' ).
    length_is(  5 ).
    glue_is( '...').
    when_string_is_shortened( ).
    then_output_should_be( 'h...d' ).
  ENDMETHOD.
ENDCLASS.
