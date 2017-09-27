*"* use this source file for your ABAP unit test classes
CLASS lcl_test_lcm DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      teardown,
      given
        IMPORTING
          val1 TYPE string
          val2 TYPE string,
      when_gcd_is_calculated,
      then_gcd_must_be_right,
      first_test_lcm FOR TESTING RAISING cx_static_check.

    DATA: m_cut  TYPE REF TO zcl_rosetta_gcd,
          val1   TYPE i VALUE '18',
          val2   TYPE i VALUE '12',
          result TYPE i VALUE '36',
          gcd    TYPE i.
ENDCLASS.

CLASS lcl_test_gcd DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA val1 TYPE i.
    DATA result TYPE i.
    DATA m_cut TYPE REF TO zcl_rosetta_gcd.
    DATA val2 TYPE i.
    METHODS:
      setup,
      first_test_gcd FOR TESTING RAISING cx_static_check,
      given
        IMPORTING
          val1 TYPE string
          val2 TYPE string,
      when_lcd_is_calculated,
      then_lcd_must_be
        IMPORTING
          result TYPE string.
ENDCLASS.

CLASS lcl_test_lcm IMPLEMENTATION.
  METHOD setup.
    m_cut = NEW zcl_rosetta_gcd( ).
  ENDMETHOD.

  METHOD teardown.

  ENDMETHOD.

  METHOD first_test_lcm.
    given(  val1 = '18'
            val2 = '12' ).

    when_gcd_is_calculated( ).

    then_gcd_must_be_right( ).
  ENDMETHOD.

  METHOD given.
    me->val1 = val1.
    me->val2 = val2.
  ENDMETHOD.

  METHOD when_gcd_is_calculated.

    gcd  = m_cut->get_lcm( val1 = val1
                           val2 = val2 ).

  ENDMETHOD.


  METHOD then_gcd_must_be_right.
    cl_abap_unit_assert=>assert_equals( act = gcd
                                            exp = result ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_test_gcd IMPLEMENTATION.

  METHOD first_test_gcd.
    given( val1 = '13'
           val2 = '0' ).

    when_lcd_is_calculated( ).

    then_lcd_must_be( '0' ).
  ENDMETHOD.


  METHOD given.
    me->val1 = val1.
    me->val2 = val2.
  ENDMETHOD.


  METHOD when_lcd_is_calculated.
    result = m_cut->get_gcd(
             val1  = val1
             val2  = val2 ).
  ENDMETHOD.

  METHOD then_lcd_must_be.
    cl_abap_unit_assert=>assert_equals( act = me->result
                                        exp = result ).
  ENDMETHOD.

  METHOD setup.
    m_cut = NEW zcl_rosetta_gcd( ).
  ENDMETHOD.

ENDCLASS.
