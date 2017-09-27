*&---------------------------------------------------------------------*
*& Report z_test_itab_slicing
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_itab_slicing.

CLASS lcl_itab DEFINITION DEFERRED.

CLASS test_itab_slicing DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: ordinary_itab TYPE stringtab,
          itab          TYPE REF TO lcl_itab,
          slice         LIKE ordinary_itab.

    METHODS:
      test_constructor FOR TESTING RAISING cx_static_check,
      test_simple_slice FOR TESTING RAISING cx_static_check,
      test_slice_with_mult_lines FOR TESTING RAISING cx_static_check,
      test_slice_without_end FOR TESTING RAISING cx_static_check,
      test_slice_without_start FOR TESTING RAISING cx_static_check,
      test_negative_slice FOR TESTING RAISING cx_static_check,
      _given_stringtab,
      _when_itab_is_created,
      _then_itab_is_not_initial,
      _then_should_be_sliced,
      _when_simple_sliced,
      _when_multi_line_sliced,
      _then_should_be_sliced2,
      _when_sliced_without_end,
      _then_should_be_sliced3,
      _when_sliced_without_start,
      _when_negative_slice,
      _then_should_be_sliced4.

ENDCLASS.

CLASS lcl_itab DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          it_table TYPE INDEX TABLE,

      slice
        IMPORTING
          i_start  TYPE i OPTIONAL
          i_end    TYPE i OPTIONAL
        EXPORTING
          et_table TYPE INDEX TABLE.

  PRIVATE SECTION.
    DATA: mr_table TYPE REF TO data.
    METHODS _invert
      IMPORTING
        it_table  TYPE INDEX TABLE
      EXPORTING
        et_result TYPE INDEX TABLE.

ENDCLASS.

CLASS test_itab_slicing IMPLEMENTATION.

  METHOD test_constructor.

    _given_stringtab( ).
    _when_itab_is_created( ).
    _then_itab_is_not_initial( ).

  ENDMETHOD.

  METHOD _given_stringtab.

    ordinary_itab = VALUE stringtab( ( `Test` ) ( `Hallo Welt!` ) ).

  ENDMETHOD.

  METHOD _when_itab_is_created.

    itab = NEW lcl_itab( ordinary_itab ).

  ENDMETHOD.

  METHOD _then_itab_is_not_initial.

    cl_abap_unit_assert=>assert_not_initial( itab ).

  ENDMETHOD.

  METHOD test_simple_slice.

    _given_stringtab( ).
    _when_itab_is_created( ).
    _when_simple_sliced( ).
    _then_should_be_sliced( ).

  ENDMETHOD.

  METHOD _then_should_be_sliced.

    cl_abap_unit_assert=>assert_equals( act = lines( slice )
                                        exp = 1
                                        msg = 'Slice should be of size 1' ).

    cl_abap_unit_assert=>assert_equals( act = slice[ 1 ]
                                        exp = `Test`
                                        msg = 'First item of slice should be `Test`' ).

  ENDMETHOD.

  METHOD _when_simple_sliced.

    itab->slice(
      EXPORTING i_start    = 1
                i_end      = 1
      IMPORTING et_table = slice ).

  ENDMETHOD.

  METHOD test_slice_with_mult_lines.

    _given_stringtab( ).
    _when_itab_is_created( ).
    _when_multi_line_sliced( ).
    _then_should_be_sliced2( ).

  ENDMETHOD.

  METHOD _when_multi_line_sliced.

    itab->slice(
      EXPORTING i_start    = 1
                i_end      = 2
      IMPORTING et_table = slice ).

  ENDMETHOD.

  METHOD _then_should_be_sliced2.

    cl_abap_unit_assert=>assert_equals( act = lines( slice )
                                        exp = 2
                                        msg = 'Slice should be of size 1' ).

    cl_abap_unit_assert=>assert_equals( act = slice[ 1 ]
                                        exp = `Test`
                                        msg = 'First item of slice should be `Test`' ).

    cl_abap_unit_assert=>assert_equals( act = slice[ 2 ]
                                        exp = `Hallo Welt!`
                                        msg = 'First item of slice should be `Hello Welt!`' ).

  ENDMETHOD.

  METHOD test_slice_without_end.

    _given_stringtab( ).
    _when_itab_is_created( ).
    _when_sliced_without_end( ).
    _then_should_be_sliced3( ).

  ENDMETHOD.

  METHOD _when_sliced_without_end.

    itab->slice(
      EXPORTING
        i_start    = 1
      IMPORTING
        et_table = slice ).

  ENDMETHOD.

  METHOD _then_should_be_sliced3.

    cl_abap_unit_assert=>assert_equals( act = slice
                                        exp = me->ordinary_itab
                                        msg = 'msg' ).

  ENDMETHOD.

  METHOD test_slice_without_start.

    _given_stringtab( ).
    _when_itab_is_created( ).
    _when_sliced_without_start( ).
    _then_should_be_sliced3( ).

  ENDMETHOD.

  METHOD _when_sliced_without_start.

    itab->slice(
      EXPORTING
        i_end    = lines( ordinary_itab )
      IMPORTING
        et_table = slice ).

  ENDMETHOD.

  METHOD test_negative_slice.

    _given_stringtab( ).
    _when_itab_is_created( ).
    _when_negative_slice( ).
    _then_should_be_sliced4( ).

  ENDMETHOD.

  METHOD _when_negative_slice.

    itab->slice(
      EXPORTING
        i_start  = -1
        i_end    = 1
      IMPORTING
        et_table = slice ).

  ENDMETHOD.

  METHOD _then_should_be_sliced4.

    DATA: inverted_itab LIKE ordinary_itab.

    cl_abap_unit_assert=>assert_equals( act = lines( slice )
                                        exp = 2
                                        msg = 'negative slice should be 2' ).

    LOOP AT ordinary_itab ASSIGNING FIELD-SYMBOL(<line>).
      INSERT <line> INTO inverted_itab INDEX 1.
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals( act = slice
                                        exp = inverted_itab
                                        msg = 'slice should be inverted' ).


  ENDMETHOD.

ENDCLASS.

CLASS lcl_itab IMPLEMENTATION.

  METHOD constructor.

    mr_table = REF #( it_table ).

  ENDMETHOD.

  METHOD slice.

    DATA: end                TYPE i,
          inverted_table_ref TYPE REF TO data.

    FIELD-SYMBOLS: <table>          TYPE INDEX TABLE,
                   <inverted_table> TYPE INDEX TABLE.

    ASSIGN mr_table->* TO <table>.

    end = COND #( WHEN i_end IS SUPPLIED THEN i_end
                        ELSE lines( <table> ) ).

    IF i_start < 0.

      CREATE DATA inverted_table_ref LIKE <table>.
      ASSIGN inverted_table_ref->* TO <inverted_table>.
      DATA(inv_end) = i_start * -1.

      _invert(
        EXPORTING it_table  = <table>
        IMPORTING et_result = <inverted_table> ).

      LOOP AT <inverted_table> ASSIGNING FIELD-SYMBOL(<line>)
                               TO inv_end.
        INSERT <line> INTO TABLE et_table.
      ENDLOOP.

    ENDIF.

    LOOP AT <table> ASSIGNING <line>
                    FROM i_start TO end.

      INSERT <line> INTO TABLE et_table.
    ENDLOOP.

  ENDMETHOD.


  METHOD _invert.

    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<line>).
      INSERT <line> INTO et_result INDEX 1.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
