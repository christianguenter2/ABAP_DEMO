REPORT z_test_rosetta_happy_numbers.

CLASS lcl_happy_number DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_int,
             i TYPE i,
           END OF ty_int,
           tty_int TYPE STANDARD TABLE OF ty_int WITH NON-UNIQUE EMPTY KEY.
    METHODS is_happy_number
      IMPORTING
                i_number                 TYPE i
      RETURNING VALUE(r_is_happy_number) TYPE abap_bool.

    METHODS: get_happy_numbers_up_to IMPORTING i_up_to                 TYPE i
                                     RETURNING VALUE(rt_happy_numbers) TYPE tty_int.
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: seen_numbers TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.

    METHODS _get_new_number
      IMPORTING
        i_base_number       TYPE i
      RETURNING
        VALUE(r_new_number) TYPE i.

    TYPES: tty_chars TYPE STANDARD TABLE OF char01 WITH NON-UNIQUE DEFAULT KEY.

    METHODS _split_string_to_chars IMPORTING i_string       TYPE string
                                   RETURNING VALUE(r_chars) TYPE tty_chars.

    METHODS _is_number_already_seen
      IMPORTING
        i_number                     TYPE i
      RETURNING
        VALUE(r_number_already_seen) TYPE abap_bool.

    METHODS _set_number_seen
      IMPORTING
        i_number TYPE i.

ENDCLASS.

CLASS lcl_happy_number IMPLEMENTATION.

  METHOD is_happy_number.
    IF i_number = 1.
      r_is_happy_number = abap_true.
      RETURN.
    ENDIF.

    IF _is_number_already_seen( i_number ).
      RETURN.
    ENDIF.

    _set_number_seen( i_number ).

    r_is_happy_number = is_happy_number( _get_new_number( i_number ) ).
  ENDMETHOD.

  METHOD _get_new_number.
    DATA(digits) = _split_string_to_chars( |{ i_base_number }| ).

    r_new_number = REDUCE #( INIT result = 0
                             FOR digit IN digits
                             NEXT result = result + ( digit * digit ) ).
  ENDMETHOD.

  METHOD _split_string_to_chars.
    DO strlen( i_string ) TIMES.
      DATA(index) = sy-index - 1.
      DATA(char)  = CONV char01( i_string+index(1) ).
      INSERT char INTO TABLE r_chars.
    ENDDO.
  ENDMETHOD.

  METHOD _is_number_already_seen.
    r_number_already_seen = boolc( line_exists( seen_numbers[ table_line = i_number ] ) ).
  ENDMETHOD.

  METHOD _set_number_seen.
    INSERT i_number INTO TABLE seen_numbers.
  ENDMETHOD.

  METHOD get_happy_numbers_up_to.
    WHILE lines( rt_happy_numbers ) < i_up_to.
      DATA(index) = sy-index.
      IF NEW lcl_happy_number( )->is_happy_number( index ).
        INSERT VALUE #( i = index ) INTO TABLE rt_happy_numbers.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_happy_number_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS:
      is_happy_number FOR TESTING,
      happy_numbers_up_to FOR TESTING.

ENDCLASS.

CLASS lcl_happy_number_test IMPLEMENTATION.

  METHOD is_happy_number.
    cl_abap_unit_assert=>assert_true( NEW lcl_happy_number( )->is_happy_number( 1 ) ).
    cl_abap_unit_assert=>assert_true( NEW lcl_happy_number( )->is_happy_number( 7 ) ).
    cl_abap_unit_assert=>assert_true( NEW lcl_happy_number( )->is_happy_number( 13 ) ).
    cl_abap_unit_assert=>assert_true( NEW lcl_happy_number( )->is_happy_number( 28 ) ).
    cl_abap_unit_assert=>assert_true( NEW lcl_happy_number( )->is_happy_number( 79 ) ).

    cl_abap_unit_assert=>assert_false( NEW lcl_happy_number( )->is_happy_number( 2 ) ).
    cl_abap_unit_assert=>assert_false( NEW lcl_happy_number( )->is_happy_number( 78 ) ).
  ENDMETHOD.

  METHOD happy_numbers_up_to.
    cl_abap_unit_assert=>assert_equals(
         act = NEW lcl_happy_number( )->get_happy_numbers_up_to( 10 )
         exp = VALUE lcl_happy_number=>tty_int( ( i = 1 ) ( i = 7 ) ( i = 10 ) ( i = 13 ) ( i = 19 ) ( i = 23 ) ( i = 28 ) ( i = 31 ) ( i = 32 ) ( i = 44 ) ) ).
  ENDMETHOD.

ENDCLASS.
