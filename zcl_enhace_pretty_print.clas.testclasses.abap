
*----------------------------------------------------------------------*
*       CLASS lcl_Test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test DEFINITION FOR TESTING
               DURATION SHORT
               RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: lt_source TYPE stringtab.
    METHODS: insert IMPORTING i_line TYPE string,
             call,
             assert_line IMPORTING line_num TYPE i
                                   line_value TYPE string,
             test_single FOR TESTING,
             test_single2 FOR TESTING,
             test_single_negativ FOR TESTING,
             test_multiple FOR TESTING,
             test_multiple2 FOR TESTING,
             test_multiple3 FOR TESTING,
             test_special_chars FOR TESTING,
             test_special_chars2 FOR TESTING,
             test_special_chars3 FOR TESTING,
             test_bounds FOR TESTING,
             test_bounds2 FOR TESTING,
             test_off_by_one FOR TESTING,
             test_with_function FOR TESTING.
ENDCLASS.       "lcl_Test


*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test IMPLEMENTATION.
  METHOD insert.
    INSERT i_line INTO TABLE lt_source.
  ENDMETHOD.                    "insert

  METHOD call.
    zcl_enhace_pretty_print=>pretty_print(
      CHANGING
        ct_source = lt_source ).
  ENDMETHOD.                    "call

  METHOD assert_line.
    FIELD-SYMBOLS: <source> LIKE LINE OF lt_source.

    READ TABLE lt_source ASSIGNING <source>
                         INDEX line_num.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp       = line_value    " Data Object with Expected Type
        act       = <source> ).
  ENDMETHOD.                    "assert_line

  METHOD test_single.
    insert( `a     = b` ).
    call( ).
    assert_line( line_num   = 1
                 line_value = `a     = b` ).
  ENDMETHOD.                    "test_single

  METHOD test_single2.
    insert( `a     = b` ).
    insert( ` ` ).
    insert( `1   = 2` ).
    call( ).
    assert_line( line_num   = 1
                 line_value = `a     = b` ).
    assert_line( line_num   = 3
                 line_value = `1   = 2` ).
  ENDMETHOD.                    "test_single2

  METHOD test_single_negativ.
    insert( `a     = b` ).
    insert( `1   = 2` ).
    call( ).
    assert_line( line_num   = 1
                 line_value = `a     = b` ).
    assert_line( line_num   = 2
                 line_value = `1   = 2` ).
  ENDMETHOD.                    "test_single_negativ

  METHOD test_multiple.
    insert(`zcl_bc_ddic_doma=>get_text_of_fixed_value(`).
    insert(`        EXPORTING`).
    insert(`          i_name       = i_name`).
    insert(`          i_langu      = sy-langu    " Language Key of Current Text Environment`).
    insert(`          i_domvalue_l = i_domvalue_l    " Values for Domains: Single Value / Upper Limit`).
    insert(`*        RECEIVING`).
    insert(`*          r_text      = r_text    " Explanatory short text`).
    insert(`      ).`).
    call( ).
    assert_line( line_num          = 1
                 line_value        = `zcl_bc_ddic_doma=>get_text_of_fixed_value(` ).
    assert_line( line_num          = 3
                 line_value        = `          i_name       = i_name` ).
    assert_line( line_num          = 4
                 line_value        = `          i_langu      = sy-langu    " Language Key of Current Text Environment` ).
    assert_line( line_num          = 5
                 line_value        = `          i_domvalue_l = i_domvalue_l    " Values for Domains: Single Value / Upper Limit` ).
    assert_line( line_num          = 7
                 line_value        = `*          r_text      = r_text    " Explanatory short text` ).
  ENDMETHOD.                    "test_multiple

  METHOD test_multiple2.
    insert(`Test = 1`).
    insert(`a = b`) .
    call( ).
    assert_line( line_num          = 1
                 line_value        = `Test = 1` ).
    assert_line( line_num          = 2
                 line_value        = `a = b` ).
  ENDMETHOD.                    "test_multiple2

  METHOD test_multiple3.
    insert(`zcl_bc_ddic_doma=>get_text_of_fixed_value(`).
    insert(`        EXPORTING`).
    insert(`          i_name       = i_name`).
    insert(`          i_langu      = sy-langu    " Language Key of Current Text Environment`).
    insert(`          i_domvalue_l = i_domvalue_l    " Values for Domains: Single Value / Upper Limit`).
    insert(`*        RECEIVING`).
    insert(`*          r_text      = r_text    " Explanatory short text`).
    insert(`      ).`).
    call( ).
    assert_line( line_num          = 1
                 line_value        = `zcl_bc_ddic_doma=>get_text_of_fixed_value(` ).
    assert_line( line_num          = 3
                 line_value        = `          i_name       = i_name` ).
    assert_line( line_num          = 4
                 line_value        = `          i_langu      = sy-langu    " Language Key of Current Text Environment` ).
    assert_line( line_num          = 5
                 line_value        = `          i_domvalue_l = i_domvalue_l    " Values for Domains: Single Value / Upper Limit` ).
    assert_line( line_num          = 7
                 line_value        = `*          r_text      = r_text    " Explanatory short text` ).
  ENDMETHOD.                    "test_multiple

  METHOD test_special_chars.
    insert(`'Test = 1'`).
    insert(`'a = b'`) .
    insert(`= 'a = b'`).
    insert(`  = 'c = d'`).
    call( ).
    assert_line( line_num          = 1
                 line_value        = `'Test = 1'` ).
    assert_line( line_num          = 2
                 line_value        = `'a = b'` ).
    assert_line( line_num          = 3
                 line_value        = `= 'a = b'` ).
    assert_line( line_num          = 4
                 line_value        = `  = 'c = d'` ).

  ENDMETHOD.                    "test_special_chars

  METHOD test_special_chars2.
    insert('`Test = 1`').
    insert('`a = b`') .
    call( ).
    assert_line( line_num          = 1
                 line_value        = '`Test = 1`' ).
    assert_line( line_num          = 2
                 line_value        = '`a = b`' ).
  ENDMETHOD.                    "test_special_chars2

  METHOD test_special_chars3.
   insert('insert( `a     = b` ).').
   insert('insert( `1   = 2` ).').
   call( ).
   assert_line( line_num   = 1
                line_value = 'insert( `a     = b` ).').
   assert_line( line_num   = 2
                line_value = 'insert( `1   = 2` ).').
  ENDMETHOD.                    "test_special_chars3

  METHOD test_bounds.
    insert(`a = 1`).
    insert(`abcd = 1`).
    call( ).
    assert_line( line_num          = 1
                 line_value        = `a = 1` ).
    assert_line( line_num          = 2
                 line_value        = `abcd = 1` ).
  ENDMETHOD.                    "test_bounds

  METHOD test_bounds2.
    insert('x = find( val = `Test`').
    insert('          sub     = `1234` ).').
    call( ).
    assert_line( line_num          = 1
                 line_value        = 'x = find( val = `Test`' ).
    assert_line( line_num          = 2
                 line_value        = '          sub     = `1234` ).').
  ENDMETHOD.                    "test_bounds2

  METHOD test_off_by_one.
    insert('zcl_bc_ddic_doma=>get_text_of_fixed_value(').
    insert('  EXPORTING').
    insert('    i_name             = `1 = 2`').
    insert('    i_langu            = sy-langu ').
    insert('    i_domvalue_l       = i_domvalue_l ).').
    call( ).
    assert_line( line_num          = 3
                 line_value        = '    i_name       = `1 = 2`' ).
    assert_line( line_num          = 4
                 line_value        = '    i_langu      = sy-langu' ).
    assert_line( line_num          = 5
                 line_value        = '    i_domvalue_l = i_domvalue_l ).').
  ENDMETHOD.                    "test_off_by_one

  METHOD test_with_function.
    insert(`delete_whitespace( EXPORTING i_count = nmax( val1 = lv_eq_pos - <data>-max_l_pos - 2`).
    insert(`                                             val2 = 0 )`).
    insert(`                   CHANGING  c_text  = <data>-s ).    `).
    call( ).
    assert_line( line_num    = 1
                 line_value  = `delete_whitespace( EXPORTING i_count = nmax( val1 = lv_eq_pos - <data>-max_l_pos - 2`).
    assert_line( line_num    = 2
                 line_value  = `                                             val2 = 0 )`).
    assert_line( line_num    = 3
                 line_value  = `                   CHANGING  c_text  = <data>-s ).    `).
  ENDMETHOD.                    "test_with_function
ENDCLASS.       "lcl_Teste
