
CLASS ltc_source_repository DEFINITION INHERITING FROM cl_aunit_assert FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA repository TYPE REF TO zcl_art_source_repository.

    METHODS read_and_compare_includes      FOR TESTING RAISING cx_static_check.
    METHODS read_not_existing_includes     FOR TESTING RAISING cx_static_check.
    METHODS read_registered_source         FOR TESTING RAISING cx_static_check.
    METHODS creation_as_temp_singleton     FOR TESTING RAISING cx_static_check.
    METHODS fresh_registry                 FOR TESTING RAISING cx_static_check.
    METHODS explicite_include_registration FOR TESTING RAISING cx_static_check.
    METHODS implicite_include_registration FOR TESTING RAISING cx_static_check.
    METHODS is_dirty_outside_main_program FOR TESTING RAISING cx_static_check.

    METHODS read_and_compare
      IMPORTING
        i_include TYPE program
      RAISING
        cx_static_check.

    METHODS: setup.

ENDCLASS.

CLASS zcl_art_source_repository DEFINITION LOCAL FRIENDS ltc_source_repository.
CLASS ltc_source_repository IMPLEMENTATION.

  METHOD setup.
    repository = zcl_art_source_repository=>create( ).
  ENDMETHOD.

  METHOD read_and_compare_includes.

    read_and_compare( 'RFAC_RENAME_TESTDATA' ). " report

    read_and_compare( 'zCL_ART_SOURCE_REPOSITORY======CS' ). " class

    read_and_compare( 'LRFAC_TEST_FUGRU01' ). " function

    read_and_compare( 'LRFAC_TEST_FUGR$01' ). " function interface

  ENDMETHOD.


  METHOD read_not_existing_includes.

    cl_abap_unit_assert=>assert_initial( repository->read_source( 'RFAC_XXXX_XXXX_XXXX' ) ).

    TRY.
        repository->read_source( 'CL_XXXX_XXXX_XXXX=============CS' ).
        fail( 'expected exception not raised' ).
      CATCH cx_rfac_dynamic ##NO_HANDLER.
        " caught expected exception class not exist
    ENDTRY.


    cl_abap_unit_assert=>assert_initial( repository->read_source( 'LRFAC_XXXX_XXXXU01' ) ).

  ENDMETHOD.

  METHOD explicite_include_registration.


    DATA: source_to_register TYPE string_table.

    APPEND 'MY REGISTERED SOURCE' TO source_to_register.

    cl_abap_unit_assert=>assert_false( repository->was_include_registered( 'RFAC_RENAME_TESTDATA' ) ).
    repository->register_source( i_include = 'RFAC_RENAME_TESTDATA' i_source = source_to_register ).
    cl_abap_unit_assert=>assert_true( repository->was_include_registered( 'RFAC_RENAME_TESTDATA' ) ).

  ENDMETHOD.

  METHOD implicite_include_registration.

    cl_abap_unit_assert=>assert_false( repository->was_include_registered( 'RFAC_RENAME_TESTDATA' ) ).
    repository->read_source( 'RFAC_RENAME_TESTDATA' ).
    cl_abap_unit_assert=>assert_true( repository->was_include_registered( 'RFAC_RENAME_TESTDATA' ) ).

  ENDMETHOD.


  METHOD read_registered_source.

    CONSTANTS l_include TYPE program VALUE 'RFAC_RENAME_TESTDATA'.

    DATA l_registered_compare TYPE string_table.
    DATA l_registered TYPE string_table.
    DATA l_source_orig TYPE string_table.

    APPEND 'MY REGISTERED SOURCE' TO l_registered_compare.

    l_source_orig = repository->read_source( l_include ).
    assert_not_initial( act = l_source_orig ).

    repository->register_source( i_include = l_include i_source = l_registered_compare ).
    l_registered = repository->read_source( l_include ).
    assert_equals( exp = l_registered_compare act = l_registered ).
    cl_abap_unit_assert=>assert_false( boolc( l_source_orig = l_registered ) ). " assert_differs( ) not for tables

  ENDMETHOD.

  METHOD fresh_registry.

    CONSTANTS l_include TYPE program VALUE 'RFAC_RENAME_TESTDATA'.

    DATA l_registered_compare TYPE string_table.
    DATA l_source_orig TYPE string_table.

    APPEND 'MY REGISTERED SOURCE' TO l_registered_compare.

    l_source_orig = repository->read_source( l_include ).
    repository->register_source( i_include = l_include i_source = l_registered_compare ).

    assert_equals( exp = l_source_orig  act = zcl_art_source_repository=>create( )->read_source( l_include ) ).


  ENDMETHOD.

  METHOD read_and_compare.

    DATA l_source_compare TYPE string_table.
    DATA l_source TYPE string_table.

    l_source_compare = repository->read_source( i_include ).
    assert_not_initial( act = l_source_compare ).

    " read source again and compare
    l_source = repository->read_source( i_include ).
    assert_equals( exp = l_source_compare act = l_source ).

    " read source again in new repository and compare
    repository = zcl_art_source_repository=>create( ).
    l_source = repository->read_source( i_include ).
    assert_equals( exp = l_source_compare act = l_source ).

  ENDMETHOD.

  METHOD creation_as_temp_singleton.

    cl_abap_unit_assert=>assert_equals( exp = zcl_art_source_repository=>create( ) act = zcl_art_source_repository=>get_instance( ) ).

    IF zcl_art_source_repository=>create( ) = zcl_art_source_repository=>create( ) ##BOOL_OK.
      fail( 'create( ) should create always a fresh instance' ).
    ENDIF.

  ENDMETHOD.

  METHOD is_dirty_outside_main_program.

    DATA: source TYPE string_table.

    source = repository->read_source( i_include = 'CL_RIS_TEST_ACTION============CCIMP'
                                      i_mode    = zcl_art_source_repository=>mode_4_compiler-full_registered_source ).
    repository->register_source( i_include = 'CL_RIS_TEST_ACTION============CCIMP'
                                 i_source  = source ).
    source = repository->read_source( i_include = 'RIS_TEST_CLASS_USAGE'
                                      i_mode    = zcl_art_source_repository=>mode_4_compiler-full_registered_source ).
    repository->register_source( i_include  = 'RIS_TEST_CLASS_USAGE'
                                 i_source   = source
                                 i_is_dirty = abap_true ).

    cl_abap_unit_assert=>assert_true( repository->is_dirty_outside_main_program( i_main_program = 'CL_RIS_TEST_ACTION============CP' ) ).
    cl_abap_unit_assert=>assert_false( repository->is_dirty_outside_main_program( i_main_program = 'RIS_TEST_CLASS_USAGE' ) ).

  ENDMETHOD.

ENDCLASS.

DEFINE sample_code_source_corer.
  IF &1 = &1. ENDIF.
  APPEND &2 TO source_code.
  CASE &3.
  WHEN 'same'.
    APPEND &2 TO exp_code.
  WHEN 'empty'.
    APPEND '' TO exp_code.
  WHEN OTHERS.
    APPEND &3 TO exp_code.
  ENDCASE.


END-OF-DEFINITION.


CLASS ltc_source_corer DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: cut         TYPE REF TO if_art_source_corer,
          source_code TYPE string_table,
          exp_code    TYPE string_table.
    METHODS:
      setup,
      verify_grained_source
        IMPORTING
          i_cursor_position TYPE REF TO cl_pst_source_position OPTIONAL,
      creation FOR TESTING RAISING cx_static_check,
      grain_methods FOR TESTING RAISING cx_static_check,
      grain_methods_in_one_line FOR TESTING RAISING cx_static_check,
      grain_methods_with_cursor FOR TESTING RAISING cx_static_check,
      dont_grain_constructors FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_source_corer IMPLEMENTATION.

  METHOD creation.

    cl_abap_unit_assert=>assert_bound( lcl_source_corer=>create( ) ).

  ENDMETHOD.

  METHOD setup.
    cut = lcl_source_corer=>create( ).
  ENDMETHOD.

  METHOD grain_methods.

    sample_code_source_corer:
    1 `CLASS lcl DEFINITION.`        `same`,
    2 `  PRIVATE SECTION.`           `same`,
    3 `    METHODS: one, two.`       `same`,
    4 `ENDCLASS.`                    `same`,
    5 `CLASS lcl IMPLEMENTATION.`    `same`,
    6 ``                             `same`,
    7 `  METHOD one.`                `same`,
    8 `    DATA text TYPE string.`   `empty`,
    9 `    IF text IS INITIAL.`      `empty`,
   10 `      text = 'hello!'.`       `empty`,
   11 `    ENDIF.`                   `empty`,
   12 `  ENDMETHOD.`                 `same`,
   13 ``                             `same`,
   14 `  METHOD two.DATA: n TYPE i.` `  METHOD two.`,
   15 `  n = 5.ENDMETHOD.`           `        ENDMETHOD.`,
   16 ``                             `same`,
   17 `ENDCLASS.`                    `same`.

    verify_grained_source( ).

  ENDMETHOD.

  METHOD dont_grain_constructors.

    sample_code_source_corer:
    1 `CLASS lcl_super DEFINITION.`                      `same`,
    2 `  PUBLIC SECTION.`                                `same`,
    3 `    METHODS: constructor.`                        `same`,
    4 `ENDCLASS.`                                        `same`,
    5 `CLASS lcl_super IMPLEMENTATION.`                  `same`,
    6 ` METHOD constructor.`                             `same`,
    7 `   DATA: n TYPE i.`                               `same`,
    8 ` ENDMETHOD.`                                      `same`,
    9 `ENDCLASS.`                                        `same`,
   10 `CLASS lcl DEFINITION INHERITING FROM lcl_super.`  `same`,
   11 `  PUBLIC SECTION.`                                `same`,
   12 `    METHODS: constructor.`                        `same`,
   13 `  PRIVATE SECTION.`                               `same`,
   14 `    METHODS: one.`                                `same`,
   15 `ENDCLASS.`                                        `same`,
   16 `CLASS lcl IMPLEMENTATION.`                        `same`,
   17 `  METHOD constructor.`                            `same`,
   18 `    super->constructor( ).`                       `same`,
   19 `  ENDMETHOD.`                                     `same`,
   20 `  METHOD one.`                                    `same`,
   21 `    DATA: n TYPE i.`                              `same`,
   22 `  ENDMETHOD.`                                     `same`,
   23 ``                                                 `same`,
   24 `ENDCLASS.`                                        `same`.

    verify_grained_source( i_cursor_position = cl_pst_source_position=>create_from_string( '21,0' )  ).

  ENDMETHOD.

  METHOD grain_methods_in_one_line.

    sample_code_source_corer:
    1 `CLASS lcl DEFINITION.`        `same`,
    2 `  PRIVATE SECTION.`           `same`,
    3 `    METHODS: one, two.`       `same`,
    4 `ENDCLASS.`                    `same`,
    5 `CLASS lcl IMPLEMENTATION.`    `same`,
    6 ``                             `same`,
    7 `  METHOD one.DATA text TYPE string.ENDMETHOD.METHOD` `  METHOD one.                      ENDMETHOD.METHOD`,
    8 `  two.DATA: n TYPE i.n = 5.ENDMETHOD.`               `  two.                     ENDMETHOD.`,
    9 ``                             `same`,
   10 `ENDCLASS.`                    `same`.

    verify_grained_source( ).

  ENDMETHOD.

  METHOD grain_methods_with_cursor.

    sample_code_source_corer:
    1 `CLASS lcl DEFINITION.`        `same`,
    2 `  PRIVATE SECTION.`           `same`,
    3 `    METHODS: one, two.`       `same`,
    4 `ENDCLASS.`                    `same`,
    5 `CLASS lcl IMPLEMENTATION.`    `same`,
    6 ``                             `same`,
    7 `  METHOD one.`                `same`,
    8 `    DATA text TYPE string.`   `same`,
    9 `    IF text IS INITIAL.`      `same`,
   10 `      text = 'hello!'.`       `same`,
   11 `    ENDIF.`                   `same`,
   12 `  ENDMETHOD.`                 `same`,
   13 ``                             `same`,
   14 `  METHOD two.DATA: n TYPE i.` `  METHOD two.`,
   15 `  n = 5.ENDMETHOD.`           `        ENDMETHOD.`,
   16 ``                             `same`,
   17 `ENDCLASS.`                    `same`.

    verify_grained_source( i_cursor_position = cl_pst_source_position=>create_from_string( '10,8' ) ).

  ENDMETHOD.


  METHOD verify_grained_source.

    DATA act_code TYPE string_table.

    act_code = cut->grain_procedures( i_source_code = source_code i_cursor_position = i_cursor_position ).

    cl_abap_unit_assert=>assert_equals( exp = exp_code act = act_code ).

  ENDMETHOD.

ENDCLASS.

DEFINE sample_code.
  add_sample_code( i_line_number = &1 i_code_before = &2 ).
END-OF-DEFINITION.

CLASS ltc_get_compiler_instance DEFINITION FINAL INHERITING FROM th_art_contributor FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: compiler               TYPE REF TO cl_abap_compiler,
          fullname               TYPE string,
          include_pos_4_compiler TYPE cl_oo_source_pos_converter=>type_include_position.
    METHODS:
      sample_source_ccau_include FOR TESTING RAISING cx_static_check,
      sample_source_cs_include FOR TESTING RAISING cx_static_check,
      sample_report FOR TESTING RAISING cx_static_check,
      sample_global_interface FOR TESTING RAISING cx_static_check,
      sample_function_group FOR TESTING RAISING cx_static_check,
      buffering_compiler_instances FOR TESTING RAISING cx_static_check,
      register_fake_source4compiler FOR TESTING RAISING cx_static_check,
      calc_compiler_pos_for_clif
        IMPORTING
          clif_name                       TYPE string
          i_cursor_position               TYPE csequence
        RETURNING
          VALUE(r_include_pos_4_compiler) TYPE cl_oo_source_pos_converter=>type_include_position.
ENDCLASS.


CLASS ltc_get_compiler_instance IMPLEMENTATION.

  METHOD sample_source_ccau_include.

    sample_code:
    1 `CLASS ltc DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.`,
    2 `  PRIVATE SECTION.`,
    3 `     METHODS: test FOR TESTING.`,
    4 `ENDCLASS.`,
    5 `CLASS ltc IMPLEMENTATION.`,
    6 `  METHOD test.`,
    7 `    DATA bla TYPE string.`,
    8 `  ENDMETHOD.`,
    9 `ENDCLASS.`.


    source_repository->register_source( i_include = 'zCL_ART_SOURCE_REPOSITORY======CCAU' i_source = source_code
                                        i_cursor_position = cl_pst_source_position=>create_from_string( '7,10' ) ).

    compiler = source_repository->get_compiler_for_main_program( 'zCL_ART_SOURCE_REPOSITORY======CP' ).

    cl_abap_unit_assert=>assert_bound( compiler ).

    compiler->get_full_name_for_position( EXPORTING p_include = 'zCL_ART_SOURCE_REPOSITORY======CCAU'
                                                    p_line    = 7
                                                    p_column  = 10
                                          IMPORTING p_full_name = fullname ).

    cl_abap_unit_assert=>assert_equals( exp = '\PR:zCL_ART_SOURCE_REPOSITORY======CP\TY:LTC\ME:TEST\DA:BLA' act = fullname ).

  ENDMETHOD.

  METHOD sample_source_cs_include.

    sample_code:
    1 `CLASS zCL_ART_SOURCE_REPOSITORY DEFINITION PUBLIC.`,
    2 `  PRIVATE SECTION.`,
    3 `     METHODS: one.`,
    4 `ENDCLASS.`,
    5 `CLASS zCL_ART_SOURCE_REPOSITORY IMPLEMENTATION.`,
    6 `  METHOD one.`,
    7 `    DATA bla TYPE string.`,
    8 `  ENDMETHOD.`,
    9 `ENDCLASS.`.


    source_repository->register_source( i_include = 'zCL_ART_SOURCE_REPOSITORY======CS' i_source = source_code
                                        i_cursor_position = cl_pst_source_position=>create_from_string( '7,10' ) ).

    include_pos_4_compiler = calc_compiler_pos_for_clif( clif_name = 'zCL_ART_SOURCE_REPOSITORY' i_cursor_position = '7,10' ).

    compiler = source_repository->get_compiler_for_main_program( 'zCL_ART_SOURCE_REPOSITORY======CP' ).

    compiler->get_full_name_for_position( EXPORTING p_include = include_pos_4_compiler-include
                                                    p_line    = include_pos_4_compiler-source_position-line
                                                    p_column  = include_pos_4_compiler-source_position-column
                                          IMPORTING p_full_name = fullname ).

    cl_abap_unit_assert=>assert_equals( exp = '\TY:zCL_ART_SOURCE_REPOSITORY\ME:ONE\DA:BLA' act = fullname ).

  ENDMETHOD.

  METHOD register_fake_source4compiler.

    sample_code:
    1 `CLASS zCL_ART_SOURCE_REPOSITORY DEFINITION PUBLIC.`,
    2 `  PRIVATE SECTION.`,
    3 `     METHODS: one.`,
    4 `ENDCLASS.`,
    5 `CLASS zCL_ART_SOURCE_REPOSITORY IMPLEMENTATION.`,
    6 `  METHOD one.`,
    7 `    DATA bla TYPE string.`,
    8 `  ENDMETHOD.`,
    9 `ENDCLASS.`.


    source_repository->register_source( i_include = 'zCL_ART_SOURCE_REPOSITORY======CS' i_source = source_code
                                        i_cursor_position = cl_pst_source_position=>create_from_string( '7,10' ) ).

    CLEAR source_code.

    sample_code:
    1 `CLASS zCL_ART_SOURCE_REPOSITORY DEFINITION PUBLIC.`,
    2 `  PRIVATE SECTION.`,
    3 `     METHODS: one.`,
    4 `ENDCLASS.`,
    5 `CLASS zCL_ART_SOURCE_REPOSITORY IMPLEMENTATION.`,
    6 `  METHOD one.`,
    7 `    DATA dirty_variable TYPE string.`,
    8 `  ENDMETHOD.`,
    9 `ENDCLASS.`.


    source_repository->register_fake_source( i_include = 'zCL_ART_SOURCE_REPOSITORY======CS' i_fake_source = source_code ).

    include_pos_4_compiler = calc_compiler_pos_for_clif( clif_name = 'zCL_ART_SOURCE_REPOSITORY' i_cursor_position = '7,10' ).

    compiler = source_repository->get_compiler_for_main_program( i_main_program =  'zCL_ART_SOURCE_REPOSITORY======CP' i_mode = zcl_art_source_repository=>mode_4_compiler-grained_registered_source ).

    compiler->get_full_name_for_position( EXPORTING p_include = include_pos_4_compiler-include
                                                    p_line    = include_pos_4_compiler-source_position-line
                                                    p_column  = include_pos_4_compiler-source_position-column
                                          IMPORTING p_full_name = fullname ).

    cl_abap_unit_assert=>assert_equals( exp = '\TY:zCL_ART_SOURCE_REPOSITORY\ME:ONE\DA:BLA' act = fullname ).

    compiler = source_repository->get_compiler_for_main_program( i_main_program = 'zCL_ART_SOURCE_REPOSITORY======CP' i_mode = zcl_art_source_repository=>mode_4_compiler-fake_source ).

    compiler->get_full_name_for_position( EXPORTING p_include = include_pos_4_compiler-include
                                                    p_line    = include_pos_4_compiler-source_position-line
                                                    p_column  = include_pos_4_compiler-source_position-column
                                          IMPORTING p_full_name = fullname ).

    cl_abap_unit_assert=>assert_equals( exp = '\TY:zCL_ART_SOURCE_REPOSITORY\ME:ONE\DA:DIRTY_VARIABLE' act = fullname ).

  ENDMETHOD.

  METHOD sample_report.

    sample_code:
    1 `REPORT dummy. CLASS ltc DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.`,
    2 `  PRIVATE SECTION.`,
    3 `     METHODS: test FOR TESTING.`,
    4 `ENDCLASS.`,
    5 `CLASS ltc IMPLEMENTATION.`,
    6 `  METHOD test.`,
    7 `    DATA bla TYPE string.`,
    8 `  ENDMETHOD.`,
    9 `ENDCLASS.`.


    source_repository->register_source( i_include = 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE' i_source = source_code
                                        i_cursor_position = cl_pst_source_position=>create_from_string( '7,10' ) ).

    compiler = source_repository->get_compiler_for_main_program( 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE' ).

    cl_abap_unit_assert=>assert_bound( compiler ).

    compiler->get_full_name_for_position( EXPORTING p_include = 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE'
                                                    p_line    = 7
                                                    p_column  = 10
                                          IMPORTING p_full_name = fullname ).

    cl_abap_unit_assert=>assert_equals( exp = '\PR:DUMMY_REPORT4SOURCE_OBJ_DOUBLE\TY:LTC\ME:TEST\DA:BLA' act = fullname ).

  ENDMETHOD.

  METHOD sample_global_interface.

    sample_code:
      1 `INTERFACE if_art_blackboard_contributor PUBLIC .`,
      2 `  METHODS one.`,
      3 `ENDINTERFACE.`.

    source_repository->register_source( i_include = 'IF_ART_BLACKBOARD_CONTRIBUTOR=IU' i_source = source_code
                                        i_cursor_position = cl_pst_source_position=>create_from_string( '2,12' ) ).

    compiler = source_repository->get_compiler_for_main_program( 'IF_ART_BLACKBOARD_CONTRIBUTOR=IP' ).
    cl_abap_unit_assert=>assert_bound( compiler ).

    include_pos_4_compiler = calc_compiler_pos_for_clif( clif_name = 'IF_ART_BLACKBOARD_CONTRIBUTOR' i_cursor_position = '2,12' ).

    compiler->get_full_name_for_position( EXPORTING p_include = include_pos_4_compiler-include
                                                    p_line    = include_pos_4_compiler-source_position-line
                                                    p_column  = include_pos_4_compiler-source_position-column
                                          IMPORTING p_full_name = fullname ).

    cl_abap_unit_assert=>assert_equals( exp = '\TY:IF_ART_BLACKBOARD_CONTRIBUTOR\ME:ONE' act = fullname ).

  ENDMETHOD.

  METHOD sample_function_group.

    sample_code:
      1 `CLASS lcl DEFINITION.`,
      2 `  PUBLIC SECTION.`,
      3 `    METHODS one.`,
      4 `ENDCLASS.`.

    source_repository->register_source( i_include = 'LTEST_FUGR_FOR_SCAN_RESULTD01' i_source = source_code
                                        i_cursor_position = cl_pst_source_position=>create_from_string( '3,12' ) ).

    CLEAR: source_code.

    sample_code:
      1 `CLASS lcl IMPLEMENTATION.`,
      2 `  METHOD one.`,
      3 `    DATA bla TYPE string.`,
      4 `  ENDMETHOD.`,
      5 `ENDCLASS.`.

    source_repository->register_source( i_include = 'LTEST_FUGR_FOR_SCAN_RESULTP01' i_source = source_code
                                        i_cursor_position = cl_pst_source_position=>create_from_string( '3,10' ) ).

    compiler = source_repository->get_compiler_for_main_program( 'SAPLTEST_FUGR_FOR_SCAN_RESULT' ).
    cl_abap_unit_assert=>assert_bound( compiler ).

    compiler->get_full_name_for_position( EXPORTING p_include = 'LTEST_FUGR_FOR_SCAN_RESULTP01'
                                                    p_line    = 3
                                                    p_column  = 10
                                          IMPORTING p_full_name = fullname ).

    cl_abap_unit_assert=>assert_equals( exp = '\PR:SAPLTEST_FUGR_FOR_SCAN_RESULT\TY:LCL\ME:ONE\DA:BLA' act = fullname ).

  ENDMETHOD.

  METHOD buffering_compiler_instances.

    DATA: second_compiler TYPE REF TO cl_abap_compiler.

    sample_code:
    1 `REPORT dummy.`.

    source_repository->register_source( i_include = 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE' i_source = source_code ).

    compiler = source_repository->get_compiler_for_main_program( 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE' ).
    second_compiler = source_repository->get_compiler_for_main_program( 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE' ).

    cl_abap_unit_assert=>assert_equals( exp = compiler act = second_compiler ).

    source_repository->register_source( i_include = 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE' i_source = source_code ).
    second_compiler = source_repository->get_compiler_for_main_program( 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE' ).

    IF compiler = second_compiler.
      fail( 'Buffered compiler instance should be cleared on call of register_source( ) for an include of to the same main program.' ).
    ENDIF.

  ENDMETHOD.

  METHOD calc_compiler_pos_for_clif.

    DATA: clif_source            TYPE REF TO cl_oo_clif_source,
          cls_key                TYPE seoclskey,
          oo_pos_converter       TYPE REF TO cl_oo_source_pos_converter,
          editor_source_position TYPE cl_source_scanner=>type_source_position,
          include                TYPE programm.

    clif_source ?= cl_oo_factory=>create_instance( )->create_clif_source( clif_name = clif_name ).

    IF clif_name(2) = 'CL'.
      include = cl_oo_classname_service=>get_cs_name( clif_name  && '' ).
    ELSEIF clif_name(2) = 'IF'.
      include = cl_oo_classname_service=>get_intfsec_name( clif_name  && '' ).
    ENDIF.

    clif_source->set_source( source_repository->read_source( include ) ).

    cls_key-clsname = clif_name.
    oo_pos_converter = cl_oo_source_pos_converter=>create( clif_key = cls_key source = clif_source ).

    editor_source_position-line   = segment( val = i_cursor_position index = 1 sep = ',' ).
    editor_source_position-column = segment( val = i_cursor_position index = 2 sep = ',' ).

    r_include_pos_4_compiler = oo_pos_converter->get_syntax_check_include_pos( editor_source_position ).

  ENDMETHOD.

ENDCLASS.



CLASS ltc_get_syntax_check_includes DEFINITION INHERITING FROM th_art_contributor FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA dirty_syntax_check_includes TYPE sreptab.
    METHODS:
      for_all_registered_sources FOR TESTING RAISING cx_static_check,
      all_dirty_standalone_includes FOR TESTING RAISING cx_static_check,
      all_dirty_fugr_includes FOR TESTING RAISING cx_static_check,
      setup,
      teardown.

ENDCLASS.


CLASS ltc_get_syntax_check_includes IMPLEMENTATION.

  METHOD setup.

    set_include_to_register( 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE' ).
    sample_code: 1 `REPORT dummy.`,
                 2 `include SRFAC_DUMMY_ART_INCLUDE_1.`,
                 3 `include SRFAC_DUMMY_ART_INCLUDE_2.`.

    set_include_to_register( 'SRFAC_DUMMY_ART_INCLUDE_1' ).
    sample_code: 1 `CLASS lcl DEFINITION.`,
                 2 `ENDCLASS.`.

    set_include_to_register( 'SRFAC_DUMMY_ART_INCLUDE_2' ).
    sample_code: 1 `CLASS lcl IMPLEMENTATION.`,
                 2 `ENDCLASS.`.

    set_include_to_register( 'IF_ART_BLACKBOARD_CONTRIBUTOR=IU' ).
    sample_code: 1 `INTERFACE if_art_blackboard_contributor PUBLIC .`,
                 2 `  METHODS one.`,
                 3 `ENDINTERFACE.`.

    set_include_to_register( 'zCL_ART_SOURCE_REPOSITORY======CS' ).
    sample_code: 1 `CLASS zCL_ART_SOURCE_REPOSITORY DEFINITION PUBLIC.`,
                 2 `  PRIVATE SECTION.`,
                 3 `     METHODS: one.`,
                 4 `ENDCLASS.`,
                 5 `CLASS zCL_ART_SOURCE_REPOSITORY IMPLEMENTATION.`,
                 6 `  METHOD one.`,
                 7 `    DATA bla TYPE string.`,
                 8 `  ENDMETHOD.`,
                 9 `ENDCLASS.`.

    set_include_to_register( 'zCL_ART_SOURCE_REPOSITORY======CCIMP' ).
    sample_code: 1 `CLASS lcl DEFINITION.`,
                 2 `ENDCLASS.`,
                 3 `CLASS lcl IMPLEMENTATION.`,
                 4 `ENDCLASS.`.

    set_include_to_register( 'LTEST_FUGR_FOR_SCAN_RESULTU01' ).
    sample_code: 1 `FUNCTION TEST_FUBA_SINGLE_INPUT_PARAM IMPORTING I_INPUT TYPE STRING.`,
                 2 `ENDFUNCTION.`.

    set_include_to_register( 'LTEST_FUGR_FOR_SCAN_RESULTD01' ).
    sample_code:
      1 `CLASS lcl DEFINITION.`,
      2 `  PUBLIC SECTION.`,
      3 `    METHODS one.`,
      4 `ENDCLASS.`.

    set_include_to_register( 'LTEST_FUGR_FOR_SCAN_RESULTP01' ).
    sample_code:
      1 `CLASS lcl IMPLEMENTATION.`,
      2 `  METHOD one.`,
      3 `    DATA bla TYPE string.`,
      4 `  ENDMETHOD.`,
      5 `ENDCLASS.`.

    "register source of last include explicitly
    register_source( ).

  ENDMETHOD.

  METHOD teardown.

    source_repository->clear_register( ).

  ENDMETHOD.

  METHOD for_all_registered_sources.

    dirty_syntax_check_includes = source_repository->get_syntax_check_includes4all( i_mode = zcl_art_source_repository=>mode_4_compiler-grained_registered_source ).

    "9 includes are registered, CS include is replaced by CP, CI, CO, CU includes, FUBA include is replaced by two includes
    cl_abap_unit_assert=>assert_equals( exp = 13 act = lines( dirty_syntax_check_includes ) ).

    dirty_syntax_check_includes = source_repository->get_syntax_check_includes4all( i_mode = zcl_art_source_repository=>mode_4_compiler-grained_registered_source
                                                                                    i_focused_incude_for_exclusion = 'zCL_ART_SOURCE_REPOSITORY======CS' ).
    cl_abap_unit_assert=>assert_equals( exp = 9 act = lines( dirty_syntax_check_includes ) ).

    dirty_syntax_check_includes = source_repository->get_syntax_check_includes4all( i_mode = zcl_art_source_repository=>mode_4_compiler-grained_registered_source
                                                                                    i_focused_incude_for_exclusion = 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE' ).
    cl_abap_unit_assert=>assert_equals( exp = 12 act = lines( dirty_syntax_check_includes ) ).

  ENDMETHOD.

  METHOD all_dirty_standalone_includes.

    dirty_syntax_check_includes = source_repository->get_registered_standalone_incs( ).

    "only standalone includes DUMMY_REPORT4SOURCE_OBJ_DOUBLE, SRFAC_DUMMY_ART_INCLUDE_1 and SRFAC_DUMMY_ART_INCLUDE_2 should be considered
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( dirty_syntax_check_includes ) ).

    dirty_syntax_check_includes = source_repository->get_registered_standalone_incs( i_focused_incude_for_exclusion = 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE' ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( dirty_syntax_check_includes ) ).

  ENDMETHOD.

  METHOD all_dirty_fugr_includes.

    dirty_syntax_check_includes = source_repository->get_registered_fugr_includes( i_function_pool = 'SAPLTEST_FUGR_FOR_SCAN_RESULT' ).

    " expected includes LTEST_FUGR_FOR_SCAN_RESULTD01 LTEST_FUGR_FOR_SCAN_RESULTP01 and two for function module
    cl_abap_unit_assert=>assert_equals( exp = 4 act = lines( dirty_syntax_check_includes ) ).


  ENDMETHOD.

ENDCLASS.

CLASS ltc_include_belongs_to_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: cut TYPE REF TO zcl_art_source_repository.
    METHODS:
      class_pool FOR TESTING RAISING cx_static_check,
      interface_pool FOR TESTING RAISING cx_static_check,
      function_group FOR TESTING RAISING cx_static_check,
      setup.
ENDCLASS.

CLASS zcl_art_source_repository DEFINITION LOCAL FRIENDS ltc_include_belongs_to_main.
CLASS ltc_include_belongs_to_main IMPLEMENTATION.

  METHOD setup.
    cut = zcl_art_source_repository=>get_instance( ).
  ENDMETHOD.

  METHOD class_pool.

    cl_abap_unit_assert=>assert_true( cut->include_belongs_to_main_prog( i_main_program = 'zCL_ART_SOURCE_REPOSITORY======CP' i_include = 'zCL_ART_SOURCE_REPOSITORY======CS' ) ).
    cl_abap_unit_assert=>assert_true( cut->include_belongs_to_main_prog( i_main_program = 'zCL_ART_SOURCE_REPOSITORY======CP' i_include = 'zCL_ART_SOURCE_REPOSITORY======CCDEF' ) ).
    cl_abap_unit_assert=>assert_true( cut->include_belongs_to_main_prog( i_main_program = 'zCL_ART_SOURCE_REPOSITORY======CP' i_include = 'zCL_ART_SOURCE_REPOSITORY======CCIMP' ) ).
    cl_abap_unit_assert=>assert_true( cut->include_belongs_to_main_prog( i_main_program = 'zCL_ART_SOURCE_REPOSITORY======CP' i_include = 'zCL_ART_SOURCE_REPOSITORY======CCAU' ) ).
    cl_abap_unit_assert=>assert_true( cut->include_belongs_to_main_prog( i_main_program = 'zCL_ART_SOURCE_REPOSITORY======CP' i_include = 'zCL_ART_SOURCE_REPOSITORY======CCMAC' ) ).
    cl_abap_unit_assert=>assert_true( cut->include_belongs_to_main_prog( i_main_program = 'zCL_ART_SOURCE_REPOSITORY======CP' i_include = 'zCL_ART_SOURCE_REPOSITORY======CM01' ) ).
    cl_abap_unit_assert=>assert_false( cut->include_belongs_to_main_prog( i_main_program = 'zCL_ART_SOURCE_REPOSITORY======CP' i_include = 'CL_ART_SOURCE_UTILITY=========CS' ) ).
    cl_abap_unit_assert=>assert_false( cut->include_belongs_to_main_prog( i_main_program = 'zCL_ART_SOURCE_REPOSITORY======CP' i_include = 'LTEST_FUGR_FOR_SCAN_RESULTT99' ) ).

  ENDMETHOD.

  METHOD interface_pool.

    cl_abap_unit_assert=>assert_true( cut->include_belongs_to_main_prog( i_main_program = 'IF_ART_BLACKBOARD_CONTRIBUTOR=IP' i_include = 'IF_ART_BLACKBOARD_CONTRIBUTOR=IU' ) ).
    cl_abap_unit_assert=>assert_false( cut->include_belongs_to_main_prog( i_main_program = 'IF_ART_BLACKBOARD_CONTRIBUTOR=IP' i_include = 'IF_ART_WHITEBOARD_CONTRIBUTOR=IU' ) ).
    cl_abap_unit_assert=>assert_false( cut->include_belongs_to_main_prog( i_main_program = 'IF_ART_BLACKBOARD_CONTRIBUTOR=IP' i_include = 'IF_ART_BLACKBOARD_CONTRIBUTOR=CS' ) ).
    cl_abap_unit_assert=>assert_false( cut->include_belongs_to_main_prog( i_main_program = 'IF_ART_BLACKBOARD_CONTRIBUTOR=IP' i_include = 'LTEST_FUGR_FOR_SCAN_RESULTT99' ) ).

  ENDMETHOD.

  METHOD function_group.

    cl_abap_unit_assert=>assert_true( cut->include_belongs_to_main_prog( i_main_program = 'SAPLTEST_FUGR_FOR_SCAN_RESULT' i_include = 'LTEST_FUGR_FOR_SCAN_RESULTT99' ) ).
    cl_abap_unit_assert=>assert_true( cut->include_belongs_to_main_prog( i_main_program = 'SAPLTEST_FUGR_FOR_SCAN_RESULT' i_include = 'LTEST_FUGR_FOR_SCAN_RESULTU01' ) ).
    cl_abap_unit_assert=>assert_true( cut->include_belongs_to_main_prog( i_main_program = 'SAPLTEST_FUGR_FOR_SCAN_RESULT' i_include = 'LTEST_FUGR_FOR_SCAN_RESULT$01' ) ).
    cl_abap_unit_assert=>assert_true( cut->include_belongs_to_main_prog( i_main_program = 'SAPLTEST_FUGR_FOR_SCAN_RESULT' i_include = 'LTEST_FUGR_FOR_SCAN_RESULTTOP' ) ).
    cl_abap_unit_assert=>assert_true( cut->include_belongs_to_main_prog( i_main_program = 'SAPLTEST_FUGR_FOR_SCAN_RESULT' i_include = 'LTEST_FUGR_FOR_SCAN_RESULTP01' ) ).
    cl_abap_unit_assert=>assert_true( cut->include_belongs_to_main_prog( i_main_program = 'SAPLTEST_FUGR_FOR_SCAN_RESULT' i_include = 'LTEST_FUGR_FOR_SCAN_RESULTD01' ) ).
    cl_abap_unit_assert=>assert_true( cut->include_belongs_to_main_prog( i_main_program = 'SAPLTEST_FUGR_FOR_SCAN_RESULT' i_include = 'LTEST_FUGR_FOR_SCAN_RESULTF01' ) ).
    cl_abap_unit_assert=>assert_false( cut->include_belongs_to_main_prog( i_main_program = 'SAPLTEST_FUGR_FOR_SCAN_RESULT' i_include = '/TEST/LFUGR_FOR_SCAN_RESULTF01' ) ).

    cl_abap_unit_assert=>assert_false( cut->include_belongs_to_main_prog( i_main_program = 'SAPLTEST_FUGR_FOR_SCAN_RESULT' i_include = 'CL_ART_SOURCE_UTILITY=========CS' ) ).
    cl_abap_unit_assert=>assert_false( cut->include_belongs_to_main_prog( i_main_program = 'SAPLTEST_FUGR_FOR_SCAN_RESULT' i_include = 'IF_ART_BLACKBOARD_CONTRIBUTOR=IU' ) ).


  ENDMETHOD.


ENDCLASS.
