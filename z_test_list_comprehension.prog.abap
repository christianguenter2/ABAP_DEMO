REPORT z_test_list_comprehension.

CLASS lcl_pythagorean_triplet DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_triplet,
             x TYPE i,
             y TYPE i,
             z TYPE i,
           END OF ty_triplet,
           tty_triplets TYPE STANDARD TABLE OF ty_triplet WITH NON-UNIQUE EMPTY KEY.

    CLASS-METHODS:
      get_triplets
        IMPORTING
          n                 TYPE i
        RETURNING
          VALUE(r_triplets) TYPE tty_triplets.

  PRIVATE SECTION.
    CLASS-METHODS:
      _is_pythagorean
        IMPORTING
          i_triplet               TYPE ty_triplet
        RETURNING
          VALUE(r_is_pythagorean) TYPE abap_bool.
ENDCLASS.

CLASS lcl_pythagorean_triplet IMPLEMENTATION.
  METHOD get_triplets.
    DATA(triplets) = VALUE tty_triplets( FOR x = 1 THEN x + 1 WHILE x <= n
                                         FOR y = x THEN y + 1 WHILE y <= n
                                         FOR z = y THEN z + 1 WHILE z <= n
                                            ( x = x y = y z = z ) ).

    LOOP AT triplets ASSIGNING FIELD-SYMBOL(<triplet>).
      IF _is_pythagorean( <triplet> ) = abap_true.
        INSERT <triplet> INTO TABLE r_triplets.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD _is_pythagorean.
    r_is_pythagorean = COND #( WHEN i_triplet-x * i_triplet-x + i_triplet-y * i_triplet-y = i_triplet-z * i_triplet-z THEN abap_true
                               ELSE abap_false ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>display( lcl_pythagorean_triplet=>get_triplets( n = 20 ) ).

CLASS lcl_test DEFINITION FINAL FOR TESTING
               DURATION SHORT
               RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA triplets TYPE lcl_pythagorean_triplet=>tty_triplets.

    METHODS:
      first_test FOR TESTING RAISING cx_static_check,

      _split_result
        IMPORTING
          i_text            TYPE string
        RETURNING
          VALUE(r_triplets) TYPE lcl_pythagorean_triplet=>tty_triplets,

      _raw_split_fields
        IMPORTING
          i_text          TYPE string
        RETURNING
          VALUE(r_fields) TYPE stringtab,

      _raw_split_record
        IMPORTING
          i_text                TYPE string
        RETURNING
          VALUE(r_raw_triplets) TYPE string_table.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  DEFINE _assert_triplet.
    triplets = _split_result( &3 ).

    assert &2 = '->'.
    cl_abap_unit_assert=>assert_equals(
            msg = |Error: { &1 } should be { &3 }|
            exp = triplets
            act = lcl_pythagorean_triplet=>get_triplets( &1 ) ).
  END-OF-DEFINITION.

  METHOD first_test.
    _assert_triplet:
       '10' '->' '3;4;5|6;8;10',
       '15' '->' '3;4;5|5;12;13|6;8;10|9;12;15',
       '20' '->' '3;4;5|5;12;13|6;8;10|8;15;17|9;12;15|12;16;20'.
  ENDMETHOD.

  METHOD _split_result.
    r_triplets = VALUE #( FOR t IN _raw_split_record( i_text )
                          LET fields = _raw_split_fields( t ) IN
                          ( x = fields[ 1 ]
                            y = fields[ 2 ]
                            z = fields[ 3 ] ) ).
  ENDMETHOD.

  METHOD _raw_split_fields.
    SPLIT i_text AT ';' INTO TABLE r_fields.
  ENDMETHOD.

  METHOD _raw_split_record.
    SPLIT i_text AT '|' INTO TABLE r_raw_triplets .
  ENDMETHOD.
ENDCLASS.
