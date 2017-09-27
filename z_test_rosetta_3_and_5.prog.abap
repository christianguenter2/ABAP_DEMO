REPORT z_test_rosetta_3_and_5.

CLASS lcl_sum_mult DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_mult_1 TYPE i
        i_mult_2 TYPE i.
    METHODS get_up_to
      IMPORTING
        i_limit         TYPE i
      RETURNING
        VALUE(r_result) TYPE i.

  PRIVATE SECTION.
    DATA: mult_1 TYPE i,
          mult_2 TYPE i.
ENDCLASS.

CLASS lcl_sum_mult IMPLEMENTATION.
  METHOD constructor.
    me->mult_1 = i_mult_1.
    me->mult_2 = i_mult_2.
  ENDMETHOD.

  METHOD get_up_to.

*    DATA(i) = mult_1.
*    DATA(values) = VALUE ty_int_hash_table( ).
*
*    WHILE i < i_limit.
*      INSERT i INTO TABLE values.
*      i = i + mult_1.
*    ENDWHILE.
*
*    i = mult_2.
*
*    WHILE i < i_limit.
*      INSERT i INTO TABLE values.
*      i = i + mult_2.
*    ENDWHILE.


    TYPES: ty_int_hash_table TYPE HASHED TABLE OF i
                                  WITH UNIQUE KEY table_line.

    DATA(vals) = REDUCE ty_int_hash_table( INIT values = VALUE ty_int_hash_table( )
                                                         FOR i = 0 THEN i + 3 WHILE i < 1000
                                                         NEXT values = VALUE #( BASE values
                                                                                 ( i ) ) ).

    r_result = REDUCE #( INIT result = 0
                         FOR value IN vals
                         NEXT result = result + value ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_sum_mult( i_mult_1 = 3
                    i_mult_2 = 5 )->get_up_to( 1000 ).


CLASS lcl_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      test_case FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.

  METHOD test_case.
    cl_aunit_assert=>assert_equals(
        exp                  = 233168
        act = NEW lcl_sum_mult( i_mult_1 = 3
                                i_mult_2 = 5 )->get_up_to( 1000 ) ).
  ENDMETHOD.

ENDCLASS.
