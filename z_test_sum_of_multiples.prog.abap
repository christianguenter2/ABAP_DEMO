REPORT z_test_sum_of_multiples.

CLASS lcl_sum_of_multiples DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      sum_of_multiples_up_to
        IMPORTING i_multiple                TYPE i
                  i_limit                   TYPE i
        RETURNING VALUE(r_sum_of_multiples) TYPE i.
ENDCLASS.

CLASS lcl_sum_of_multiples IMPLEMENTATION.
  METHOD sum_of_multiples_up_to.
    TYPES: ty_int_hash_table TYPE HASHED TABLE OF i
                              WITH UNIQUE KEY table_line.

    DATA(multiples) = REDUCE ty_int_hash_table( INIT values = VALUE ty_int_hash_table( )
                                                              FOR i = 0 THEN i + i_multiple WHILE i < i_limit
                                                              NEXT values = VALUE #( BASE values
                                                                                      ( i ) ) ).

    r_sum_of_multiples = REDUCE #( INIT result = 0
                                   FOR value IN multiples
                                   NEXT result = result + value ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>display( NEW lcl_sum_of_multiples( )->sum_of_multiples_up_to( i_multiple = 3
                                                                                i_limit    = 1000 ) ).
