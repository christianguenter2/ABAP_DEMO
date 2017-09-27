REPORT z_test_collatz.

CLASS lcl_collatz DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS get_sequence
      IMPORTING
        starting_with   TYPE i
      RETURNING
        VALUE(r_result) TYPE int4_table.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_collatz IMPLEMENTATION.
  METHOD get_sequence.
    DATA: i  TYPE i.

    i = starting_with.

    INSERT i INTO TABLE r_result.

    WHILE i <> 1.
      i = COND #( WHEN i MOD 2 = 0 THEN i / 2
                  ELSE 3 * i + 1  ).
      INSERT i INTO TABLE r_result.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.



CLASS test_collatz DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      first_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS test_collatz IMPLEMENTATION.
  DEFINE _insert.
    insert &1 into table result.
  END-OF-DEFINITION.

  METHOD first_test.
    DATA: result TYPE int4_table.
    _insert: 19, 58, 29, 88, 44, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1.

    cl_abap_unit_assert=>assert_equals(
        exp = result
        act = lcl_collatz=>get_sequence( starting_with = 19 ) ).
  ENDMETHOD.
ENDCLASS.
