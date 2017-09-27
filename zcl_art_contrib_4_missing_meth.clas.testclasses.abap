*"* use this source file for your ABAP unit test classes
DEFINE source.
  APPEND &1 TO source_code.
END-OF-DEFINITION.

DEFINE sample_code.
  add_sample_code( i_line_number = &1 i_code_before = &2 i_code_change = &3 ).
END-OF-DEFINITION.

DEFINE sample_code_no_changes.
  add_sample_code( i_line_number = &1 i_code_before = &2 ).
END-OF-DEFINITION.

CLASS ltc_create_methods_abstract DEFINITION ABSTRACT INHERITING FROM zth_art_contributor FOR TESTING DURATION MEDIUM RISK LEVEL HARMLESS.

  PROTECTED SECTION.
    METHODS:
      exercise_qfix_contributor,
      assert_clif_and_method
        IMPORTING
                  i_class         TYPE string
                  i_method        TYPE string
                  i_interface     TYPE string OPTIONAL
                  i_parameters    TYPE if_rfac_adt_res_definitions=>tt_method_parameters OPTIONAL
                  i_adt_selection TYPE string OPTIONAL
                  i_is_static     TYPE abap_bool DEFAULT abap_false
                  i_qfix          TYPE REF TO ce_art_qfix DEFAULT ce_art_qfix=>create_method_def_dialog
                  i_visibility    TYPE string OPTIONAL
        RAISING   cx_static_check.

    DATA: cut TYPE REF TO zcl_art_contrib_4_missing_meth.

ENDCLASS.

CLASS ltc_create_methods_abstract IMPLEMENTATION.

  METHOD exercise_qfix_contributor.
    cut = zcl_art_contrib_4_missing_meth=>create( blackboard ).
    cut->if_art_blackboard_contributor~contribute( ).
  ENDMETHOD.

  METHOD assert_clif_and_method.
    DATA:
      qfix_id_matched    TYPE abap_bool,
      user_content_xml   TYPE string,
      x_user_content_xml TYPE xstring,
      adt_method         TYPE if_rfac_adt_res_definitions=>ts_extract_method_refactoring,
      method_description TYPE REF TO cl_rfac_method_description,
      method_definition  TYPE if_rfac_impl_types=>ts_method,
      proposals          TYPE cl_art_proposal=>tab,
      proposal           TYPE REF TO cl_art_proposal.

    FIELD-SYMBOLS <parameter> LIKE LINE OF adt_method-parameters.

    proposals = blackboard->get_proposals( ).

    LOOP AT proposals INTO proposal.
      CHECK proposal->qfix->id = i_qfix->id.
      qfix_id_matched = abap_true.

      user_content_xml = proposal->user_content.
      x_user_content_xml = cl_abap_codepage=>convert_to( user_content_xml ).

      CALL TRANSFORMATION rfac_st_adt_extractmethod
        SOURCE XML x_user_content_xml
        RESULT root = adt_method.

      cl_abap_unit_assert=>assert_equals( act = adt_method-class_name exp = i_class  msg = |Class { i_class } not found | ).
      cl_abap_unit_assert=>assert_not_initial( act = adt_method-visibility msg = |Visibilty is not set| ).

      method_description = cl_rfac_method_description=>create_from_adt( adt_method ).

      method_definition = method_description->data.

      IF i_interface IS NOT INITIAL.
        cl_abap_unit_assert=>assert_equals( exp = i_interface act = method_definition-interface_name ).
      ENDIF.

      IF i_visibility IS NOT INITIAL.
        cl_abap_unit_assert=>assert_equals( act = adt_method-visibility exp = i_visibility msg = |exp visibilty { i_visibility } but is { adt_method-visibility } | ).
      ENDIF.

      IF i_parameters IS SUPPLIED.
        LOOP AT adt_method-parameters ASSIGNING <parameter>.
          CLEAR <parameter>-user_content.
        ENDLOOP.
        cl_abap_unit_assert=>assert_equals( act = adt_method-parameters exp = i_parameters ).
      ELSE.
        cl_abap_unit_assert=>assert_initial( adt_method-parameters ).
      ENDIF.

      IF i_adt_selection IS SUPPLIED.
        cl_abap_unit_assert=>assert_equals( act = adt_method-generic-adt_object_uri exp = i_adt_selection ).
      ENDIF.

      cl_abap_unit_assert=>assert_equals( act = adt_method-is_static exp = i_is_static msg = 'static method expected' ).
      cl_abap_unit_assert=>assert_equals( act = to_lower( adt_method-name ) exp = to_lower( i_method ) ).

    ENDLOOP.

    IF qfix_id_matched = abap_false.
      cl_abap_unit_assert=>fail( msg = |No evaluation result found with expected quickfix id: { i_qfix->id }| ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.


CLASS ltc_create_method_from_call DEFINITION INHERITING FROM ltc_create_methods_abstract FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.

    DATA: parameters TYPE if_rfac_adt_res_definitions=>tt_method_parameters.

    METHODS setup.

    METHODS add_returning
      IMPORTING
        i_id        TYPE string
        i_name      TYPE string DEFAULT lcl_method_call=>co_returning_default
        i_type      TYPE string DEFAULT 'STRING'
        i_type_type TYPE string DEFAULT 'type'.

    METHODS add_importing
      IMPORTING
        i_id         TYPE string
        i_name       TYPE string
        i_type       TYPE string DEFAULT 'STRING'
        i_type_type  TYPE string DEFAULT 'type'
        i_with_upper TYPE abap_bool DEFAULT abap_true.

    METHODS add_exporting
      IMPORTING
        i_id        TYPE string
        i_name      TYPE string
        i_type      TYPE string DEFAULT 'STRING'
        i_type_type TYPE string DEFAULT 'type'.

    METHODS add_changing
      IMPORTING
        i_id        TYPE string
        i_name      TYPE string
        i_type      TYPE string DEFAULT 'STRING'
        i_type_type TYPE string DEFAULT 'type'.

    METHODS add_parameter_type
      IMPORTING
        i_id         TYPE string
        i_name       TYPE string
        i_type       TYPE string DEFAULT 'STRING'
        i_type_type  TYPE string DEFAULT 'type'
        i_direction  TYPE string
        i_with_upper TYPE abap_bool DEFAULT abap_true.


    METHODS:
      static_method_no_param         FOR TESTING RAISING cx_static_check,
      instance_method_no_param       FOR TESTING RAISING cx_static_check,
      traditionel_call_method_01     FOR TESTING RAISING cx_static_check, "with call method uppercase
      traditionel_call_method_02     FOR TESTING RAISING cx_static_check, "with call method lowercase and no parenthesis
      returning_abap_bool            FOR TESTING RAISING cx_static_check,
      signature_receiving            FOR TESTING RAISING cx_static_check,
      signature_import_string_var    FOR TESTING RAISING cx_static_check,
      signature_import_int_val       FOR TESTING RAISING cx_static_check,
      signature_import_string_val    FOR TESTING RAISING cx_static_check,
      signature_import_3_params      FOR TESTING RAISING cx_static_check,
      signature_export_long_form     FOR TESTING RAISING cx_static_check,
      signature_export_medium_form   FOR TESTING RAISING cx_static_check,
      signature_export_short_form    FOR TESTING RAISING cx_static_check,
      signature_changing_param       FOR TESTING RAISING cx_static_check,
      signature_mixed_each_param_typ FOR TESTING RAISING cx_static_check,
      signature_mixed_with_receiving FOR TESTING RAISING cx_static_check,
      mixed_with_inline_data_dec     FOR TESTING RAISING cx_static_check,
      signature_short_string_pattern FOR TESTING RAISING cx_static_check,
      signature_export_field_symbol  FOR TESTING RAISING cx_static_check,
      signature_receive_field_symbol FOR TESTING RAISING cx_static_check,
      signature_me                   FOR TESTING RAISING cx_static_check,
      short_signature_with_expr      FOR TESTING RAISING cx_static_check,
      short_signature_string_templ   FOR TESTING RAISING cx_static_check,
      medium_signature_with_expr     FOR TESTING RAISING cx_static_check,
      long_signature_with_expr       FOR TESTING RAISING cx_static_check,
      signature_inlines              FOR TESTING RAISING cx_static_check, "method inline and string pattern
      component_importing            FOR TESTING RAISING cx_static_check,
      static_call_multi_lines        FOR TESTING RAISING cx_static_check,
      static_call_multi_lines_old    FOR TESTING RAISING cx_static_check,
      inline_01                      FOR TESTING RAISING cx_static_check,
      inline_string_concat_01        FOR TESTING RAISING cx_static_check,
      inline_string_concat_02        FOR TESTING RAISING cx_static_check,
      inline_exists                  FOR TESTING RAISING cx_static_check,
      inline_static_local            FOR TESTING RAISING cx_static_check,
      inline_static_global           FOR TESTING RAISING cx_static_check,
      inline_static_glob_exists      FOR TESTING RAISING cx_static_check,
      global_interface_method        FOR TESTING RAISING cx_static_check,
      local_interface_method         FOR TESTING RAISING cx_static_check,
      local_intf_method_no_decl      FOR TESTING RAISING cx_static_check,

      no_proposal_4_super_meth_call FOR TESTING RAISING cx_static_check,
      chained_call                   FOR TESTING RAISING cx_static_check,
      params_by_nested_methods FOR TESTING RAISING cx_static_check,
      params_by_nested_chained_meth FOR TESTING RAISING cx_static_check,
      params_by_deeply_nest_chain FOR TESTING RAISING cx_static_check,
      deep_struc_component_importing FOR TESTING RAISING cx_static_check,
      static_interface_method FOR TESTING RAISING cx_static_check,
      local_interface_method_ref FOR TESTING RAISING cx_static_check,
      no_proposal_alias_in_interface FOR TESTING RAISING cx_static_check,
      no_proposal_if_name_too_long FOR TESTING RAISING cx_static_check,
      no_proposal_after_super FOR TESTING RAISING cx_static_check,
      no_wizard_4_redefined FOR TESTING RAISING cx_static_check,
      no_wizard_4_redef_aliased FOR TESTING RAISING cx_static_check,
      param_field_access_after_meth FOR TESTING RAISING cx_static_check,
      param_comp_access_after_meth FOR TESTING RAISING cx_static_check,
      call_after_if FOR TESTING RAISING cx_static_check,
      call_after_case FOR TESTING RAISING cx_static_check,
      call_after_when FOR TESTING RAISING cx_static_check,
      call_after_loop FOR TESTING RAISING cx_static_check,
      call_after_if_exporting FOR TESTING RAISING cx_static_check,
      call_after_if_with_brackets FOR TESTING RAISING cx_static_check,
      call_after_if_right_side FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS zcl_art_contrib_4_missing_meth DEFINITION LOCAL FRIENDS ltc_create_method_from_call.
CLASS ltc_create_method_from_call IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT cut.
    CLEAR parameters.
  ENDMETHOD.

  METHOD add_returning.
    add_parameter_type( i_direction = 'returning' i_id = i_id i_name = i_name i_type = i_type i_type_type = i_type_type ).
  ENDMETHOD.

  METHOD add_importing.
    add_parameter_type( i_direction = 'importing' i_id = i_id i_name = i_name i_type = i_type i_type_type = i_type_type i_with_upper = i_with_upper ).
  ENDMETHOD.

  METHOD add_exporting.
    add_parameter_type( i_direction = 'exporting' i_id = i_id i_name = i_name i_type = i_type i_type_type = i_type_type ).
  ENDMETHOD.

  METHOD add_changing.
    add_parameter_type( i_direction = 'changing' i_id = i_id i_name = i_name i_type = i_type i_type_type = i_type_type ).
  ENDMETHOD.

  METHOD add_parameter_type.

    DATA parameter TYPE if_rfac_adt_res_definitions=>ts_method_parameter.

    IF i_with_upper = abap_true.
      parameter-id        = to_upper( i_id ).
      parameter-name      = to_upper( i_name ).
    ELSE.
      parameter-id        = i_id.
      parameter-name      = i_name.
    ENDIF.
    parameter-typetype  = i_type_type.
    parameter-type      = i_type.
    parameter-direction = i_direction.
    parameter-byvalue = boolc( parameter-direction = 'returning' ).
    APPEND parameter TO parameters.

  ENDMETHOD.

  METHOD static_method_no_param.

    sample_code_no_changes:

      1 `REPORT dummy.`,
      2 `CLASS lcl_test DEFINITION. ENDCLASS.`,
      3 `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      4 `START-OF-SELECTION.`,
      5 `  lcl_test=>new_static_method( ).`.

    prepare_blackboard( '5,20' ).
    exercise_qfix_contributor( ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'NEW_STATIC_METHOD' i_is_static = abap_true ).
  ENDMETHOD.

  METHOD static_interface_method.

    sample_code_no_changes:

     1 `REPORT dummy.`,
     2 `INTERFACE lif_test. ENDINTERFACE.`,
     3 `CLASS lcl_test DEFINITION. PUBLIC SECTION. INTERFACES lif_test. ENDCLASS.`,
     4 `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
     5 `START-OF-SELECTION.`,
     6 `  lcl_test=>lif_test~new_static_method( ).`.

    prepare_blackboard( '6,25' ).
    exercise_qfix_contributor( ).
    assert_clif_and_method( i_class = 'LIF_TEST' i_interface = 'LIF_TEST' i_method = 'NEW_STATIC_METHOD' i_is_static = abap_true ).
  ENDMETHOD.

  METHOD instance_method_no_param.

    sample_code_no_changes:

     1 `REPORT dummy.`,
     2 `CLASS lcl_test DEFINITION. ENDCLASS.`,
     3 `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
     4 `START-OF-SELECTION.`,
     5 `  DATA obj_test  TYPE REF TO lcl_test.`,
     6 `  obj_test->add_method( ).`.

    prepare_blackboard( '6,20' ).
    exercise_qfix_contributor( ).

    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' ).

  ENDMETHOD.

  METHOD traditionel_call_method_01.

    sample_code_no_changes:

     1 `REPORT dummy.`,
     2 `CLASS lcl_test DEFINITION. ENDCLASS.`,
     3 `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
     4 `START-OF-SELECTION.`,
     5 `  DATA obj_test  TYPE REF TO lcl_test.`,
     6 `  CALL METHOD obj_test->add_method( ).`.

    prepare_blackboard( '6,30' ).
    exercise_qfix_contributor( ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' ).

  ENDMETHOD.

  METHOD traditionel_call_method_02.

    sample_code_no_changes:

     1 `REPORT dummy.`,
     2 `CLASS lcl_test DEFINITION. ENDCLASS.`,
     3 `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
     4 `START-OF-SELECTION.`,
     5 `  DATA obj_test  TYPE REF TO lcl_test.`,
     6 `  call method obj_test->add_method.`.

    prepare_blackboard( '6,30' ).
    exercise_qfix_contributor( ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' ).

  ENDMETHOD.

  METHOD returning_abap_bool.

    sample_code_no_changes:

     1 `REPORT dummy.`,
     2 `CLASS lcl_test DEFINITION. ENDCLASS.`,
     3 `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
     4 `START-OF-SELECTION.`,
     5 `  DATA: obj_test  TYPE REF TO lcl_test, boolean type abap_bool.`,
     6 `  boolean = obj_test->add_method_returning( ).`.

    prepare_blackboard( '6,30' ).
    exercise_qfix_contributor( ).
    add_returning( i_id = 'boolean' i_type = 'ABAP_BOOL' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD_RETURNING' i_parameters = parameters ).

  ENDMETHOD.


  METHOD call_after_if.

    sample_code_no_changes:

     1 `REPORT dummy.`,

     2 `CLASS lcl_car DEFINITION.`,
     3 `  PUBLIC SECTION.`,
     4 `    METHODS: drive.`,
     5 `ENDCLASS.`,

     6 `CLASS lcl_car IMPLEMENTATION.`,
     7 `  METHOD drive.`,
     8 `    IF check_speed( ) = abap_true.`,
     9 `    ENDIF.`,
    10 `  ENDMETHOD.`,
    11 `ENDCLASS.`.

    prepare_blackboard( '8,12' ).
    exercise_qfix_contributor( ).
    add_returning( i_id = 'ABAP_TRUE' i_type = 'ABAP_BOOL' ).
    assert_clif_and_method( i_class = 'LCL_CAR' i_method = 'CHECK_SPEED' i_parameters = parameters ).

  ENDMETHOD.

  METHOD call_after_if_right_side.

    sample_code_no_changes:

     1 `REPORT dummy.`,

     2 `CLASS lcl_car DEFINITION.`,
     3 `  PUBLIC SECTION.`,
     4 `    METHODS: drive.`,
     5 `ENDCLASS.`,

     6 `CLASS lcl_car IMPLEMENTATION.`,
     7 `  METHOD drive.`,
     8 `    IF abap_true = check_speed( ).`,
     9 `    ENDIF.`,
    10 `  ENDMETHOD.`,
    11 `ENDCLASS.`.

    prepare_blackboard( '8,24' ).
    exercise_qfix_contributor( ).
    add_returning( i_id = 'ABAP_TRUE' i_type = 'ABAP_BOOL' ).
    assert_clif_and_method( i_class = 'LCL_CAR' i_method = 'CHECK_SPEED' i_parameters = parameters ).

  ENDMETHOD.

  METHOD call_after_if_with_brackets.

    sample_code_no_changes:

     1 `REPORT dummy.`,

     2 `CLASS lcl_car DEFINITION.`,
     3 `  PUBLIC SECTION.`,
     4 `    METHODS: drive.`,
     5 `ENDCLASS.`,

     6 `CLASS lcl_car IMPLEMENTATION.`,
     7 `  METHOD drive.`,
     8 `    IF ( check_speed( ) = abap_true ).`,
     9 `    ENDIF.`,
    10 `  ENDMETHOD.`,
    11 `ENDCLASS.`.

    prepare_blackboard( '8,14' ).
    exercise_qfix_contributor( ).
    add_returning( i_id = 'ABAP_TRUE' i_type = 'ABAP_BOOL' ).
    assert_clif_and_method( i_class = 'LCL_CAR' i_method = 'CHECK_SPEED' i_parameters = parameters ).

  ENDMETHOD.

  METHOD call_after_if_exporting.

    sample_code_no_changes:

     1 `REPORT dummy.`,

     2 `CLASS lcl_car DEFINITION.`,
     3 `  PUBLIC SECTION.`,
     4 `    METHODS: drive.`,
     5 `ENDCLASS.`,

     6 `CLASS lcl_car IMPLEMENTATION.`,
     7 `  METHOD drive.`,
     8 `    IF check_speed( EXPORTING i_speed = 50 ) = abap_true.`,
     9 `    ENDIF.`,
    10 `  ENDMETHOD.`,
    11 `ENDCLASS.`.

    prepare_blackboard( '8,12' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = '50' i_name = 'i_speed' i_type = 'I' ).
    add_returning( i_id = 'ABAP_TRUE' i_type = 'ABAP_BOOL' ).
    assert_clif_and_method( i_class = 'LCL_CAR' i_method = 'CHECK_SPEED' i_parameters = parameters ).

  ENDMETHOD.


  METHOD call_after_case.

    sample_code_no_changes:

     1 `REPORT dummy.`,

     2 `CLASS lcl_car DEFINITION.`,
     3 `  PUBLIC SECTION.`,
     4 `    METHODS: drive.`,
     5 `ENDCLASS.`,

     6 `CLASS lcl_car IMPLEMENTATION.`,
     7 `  METHOD drive.`,
     8 `    CASE check_speed( ).`,
     9 `        WHEN abap_true.`,
    10 `    ENDCASE.`,
    11 `  ENDMETHOD.`,
    12 `ENDCLASS.`.

    prepare_blackboard( '8,12' ).
    exercise_qfix_contributor( ).
    add_returning( i_id = '' i_type = 'ANY' ).
    assert_clif_and_method( i_class = 'LCL_CAR' i_method = 'CHECK_SPEED' i_parameters = parameters ).

  ENDMETHOD.

  METHOD call_after_when.

    sample_code_no_changes:

     1 `REPORT dummy.`,

     2 `CLASS lcl_car DEFINITION.`,
     3 `  PUBLIC SECTION.`,
     4 `    METHODS: drive.`,
     5 `ENDCLASS.`,

     6 `CLASS lcl_car IMPLEMENTATION.`,
     7 `  METHOD drive.`,
     8 `    CASE abap_true.`,
     9 `        WHEN check_speed( ).`,
    10 `    ENDCASE.`,
    11 `  ENDMETHOD.`,
    12 `ENDCLASS.`.

    prepare_blackboard( '9,18' ).
    exercise_qfix_contributor( ).
    add_returning( i_id = '' i_type = 'ANY' ).
    assert_clif_and_method( i_class = 'LCL_CAR' i_method = 'CHECK_SPEED' i_parameters = parameters ).

  ENDMETHOD.

  METHOD call_after_loop.

    sample_code_no_changes:

     1 `REPORT dummy.`,

     2 `CLASS lcl_car DEFINITION.`,
     3 `  PUBLIC SECTION.`,
     4 `    METHODS: drive.`,
     5 `ENDCLASS.`,

     6 `CLASS lcl_car IMPLEMENTATION.`,
     7 `  METHOD drive.`,
     8 `    DATA speeds TYPE STANDARD TABLE OF i.`,
     9 `    LOOP AT get_speeds( ) INTO speeds.`,
    10 `    ENDLOOP.`,
    11 `  ENDMETHOD.`,
    12 `ENDCLASS.`.

    prepare_blackboard( '9,18' ).
    exercise_qfix_contributor( ).
    add_returning( i_id = '' i_type = 'ANY' ).
    assert_clif_and_method( i_class = 'LCL_CAR' i_method = 'GET_SPEEDS' i_parameters = parameters ).

  ENDMETHOD.

  METHOD signature_receiving.

    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, boolean type abap_bool.`,
      `  obj_test->add_method_returning( Receiving r_value = boolean ).`.

    prepare_blackboard( '6,30' ).
    exercise_qfix_contributor( ).
    add_returning( i_id = 'boolean' i_type = 'ABAP_BOOL' i_name = 'r_value' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD_RETURNING' i_parameters = parameters ).
  ENDMETHOD.

  METHOD chained_call.

    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. PUBLIC SECTION.`,
      `METHODS test. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. METHOD test. ENDMETHOD. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, boolean type abap_bool.`,
      `  obj_test->add_method_returning( )->test( ).`.

    prepare_blackboard( '7,20' ).
    exercise_qfix_contributor( ).
    add_returning( i_id = '' i_name = 'r_result' i_type = 'OBJECT' i_type_type = if_rfac_adt_res_definitions=>co_parameter_type_type-type_ref_to ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD_RETURNING' i_parameters = parameters ).
  ENDMETHOD.

  METHOD params_by_nested_methods.

    abort_if_wrong_release( 'Type determination for returning parameters via inline declarations is not possible below 7.40 release.' ).

    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. PUBLIC SECTION.`,
      `METHODS one returning value(r) type ref to LCL_TEST.`,
      `METHODS two returning value(r) type CHAR10. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. METHOD one. ENDMETHOD. METHOD two. ENDMETHOD. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, boolean type abap_bool.`,
      `  obj_test->new_method( exporting iv1 = obj_test->one( ) iv2 = obj_test->two( ) importing ev1 = boolean ).`.

    prepare_blackboard( '8,20' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = 'obj_test->one( )' i_name = 'iv1' i_type = 'LCL_TEST' i_type_type = if_rfac_adt_res_definitions=>co_parameter_type_type-type_ref_to ).
    add_importing( i_id = 'obj_test->two( )' i_name = 'iv2' i_type = 'CHAR10' ).
    add_exporting( i_id = 'boolean' i_name = 'ev1' i_type = 'ABAP_BOOL' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'NEW_METHOD' i_parameters = parameters ).

  ENDMETHOD.

  METHOD params_by_nested_chained_meth.

    abort_if_wrong_release( 'Type determination for returning parameters via inline declarations is not possible below 7.40 release.' ).

    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. PUBLIC SECTION.`,
      `METHODS get_obj returning value(r) type ref to lcl_test.`,
      `METHODS one returning value(r) type I.`,
      `METHODS two returning value(r) type CHAR10. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. METHOD one. ENDMETHOD. METHOD two. ENDMETHOD. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, boolean type abap_bool.`,
      `  obj_test->new_method( iv1 = obj_test->get_obj( )->one( )`,
      `                        iv2 = obj_test->get_obj( )->two( ) ).`.

    prepare_blackboard( '9,20' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = 'obj_test->get_obj( )->one( )' i_name = 'iv1' i_type = 'I' ).
    add_importing( i_id = 'obj_test->get_obj( )->two( )' i_name = 'iv2' i_type = 'CHAR10' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'NEW_METHOD' i_parameters = parameters ).

  ENDMETHOD.

  METHOD params_by_deeply_nest_chain.

    abort_if_wrong_release( 'Type determination for returning parameters via inline declarations is not possible below 7.40 release.' ).

    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. PUBLIC SECTION.`,
      `METHODS get_obj importing iv type string returning value(r) type ref to lcl_test.`,
      `METHODS one importing iv type string returning value(r) type I.`,
      `METHODS two importing iv type string returning value(r) type CHAR10.`,
      `METHODS three returning value(r) type STRING. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. METHOD one. ENDMETHOD. METHOD two. ENDMETHOD. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, boolean type abap_bool.`,
      `  obj_test->new_method( iv1 = obj_test->get_obj( obj_test->three( ) )->one( obj_test->three( ) )`,
      `                        iv2 = obj_test->get_obj( obj_test->three( ) )->two( obj_test->three( ) ) ).`.

    prepare_blackboard( '10,20' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = 'obj_test->get_obj( obj_test->three( ) )->one( obj_test->three( ) )' i_name = 'iv1' i_type = 'I' ).
    add_importing( i_id = 'obj_test->get_obj( obj_test->three( ) )->two( obj_test->three( ) )' i_name = 'iv2' i_type = 'CHAR10' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'NEW_METHOD' i_parameters = parameters ).

  ENDMETHOD.

  METHOD param_field_access_after_meth.

    abort_if_wrong_release( 'Type determination for returning parameters via inline declarations is not possible below 7.40 release.' ).

    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. PUBLIC SECTION.`,
      `METHODS one returning value(r) type SYST.`,
      `METHODS two returning value(r) type CHAR10. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. METHOD one. ENDMETHOD. METHOD two. ENDMETHOD. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, boolean type abap_bool.`,
      `  obj_test->new_method( iv1 = obj_test->one( )-uname iv2 = obj_test->one( )-datum ).`.

    prepare_blackboard( '8,20' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = 'obj_test->one( )-uname' i_name = 'iv1' i_type = 'SYST-UNAME'  ).
    add_importing( i_id = 'obj_test->one( )-datum' i_name = 'iv2' i_type = 'D' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'NEW_METHOD' i_parameters = parameters ).

  ENDMETHOD.

  METHOD param_comp_access_after_meth.

    abort_if_wrong_release( 'Type determination for returning parameters via inline declarations is not possible below 7.40 release.' ).

    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. PUBLIC SECTION.`,
      `DATA attribute type i.`,
      `METHODS one returning value(r) type ref to lcl_test.`,
      `METHODS two returning value(r) type CHAR10. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. METHOD one. ENDMETHOD. METHOD two. ENDMETHOD. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, boolean type abap_bool.`,
      `  obj_test->new_method( iv1 = obj_test->one( )->attribute ).`.

    prepare_blackboard( '9,20' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = 'obj_test->one( )->attribute' i_name = 'iv1' i_type = 'I'  ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'NEW_METHOD' i_parameters = parameters ).

  ENDMETHOD.

  METHOD signature_import_string_var."import_var_string
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, var type string.`,
      `  obj_test->add_method( IMPORTING e_value = var ).`.

    prepare_blackboard( '6,20' ).
    exercise_qfix_contributor( ).
    add_exporting( i_id = 'var' i_name = 'e_value' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).
  ENDMETHOD.

  METHOD signature_import_int_val.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test.`,
      `  obj_test->add_method( EXPORTING e_value = 5 ).`.

    prepare_blackboard( '6,20' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = '5' i_name = 'e_value'  i_type = 'I' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).
  ENDMETHOD.

  METHOD signature_import_string_val.

    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test.`,
      `  obj_test->add_method( IMPORTING e_value = 'HELLO' ).`.

    prepare_blackboard( '6,20' ).
    exercise_qfix_contributor( ).
    add_exporting( i_id = `'HELLO'` i_name = 'e_value' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).
  ENDMETHOD.

  METHOD signature_import_3_params.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, var type string.`,
      `  obj_test->add_method( EXPORTING e_v1 = 5 e_v2 = 'HELLO' e_v3 = var ).`.

    prepare_blackboard( '6,20' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = '5' i_name = 'e_v1'  i_type = 'I' ).
    add_importing( i_id = `'HELLO'` i_name = 'e_v2' ).
    add_importing( i_id = 'var' i_name = 'e_v3' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).
  ENDMETHOD.

  METHOD signature_export_long_form.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, var type string.`,
      `  obj_test->add_method( EXPORTING i_value = var ).`.

    prepare_blackboard( '6,20' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = 'var' i_name = 'i_value' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).
  ENDMETHOD.

  METHOD signature_export_medium_form.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, var type i.`,
      `  obj_test->add_method( i_value = var ).`.

    prepare_blackboard( '6,20' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = 'var' i_name = 'i_value' i_type = 'I' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).
  ENDMETHOD.

  METHOD signature_export_short_form.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, var type i.`,
      `  obj_test->add_method( var ).`.

    prepare_blackboard( '6,20' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = 'var' i_name = 'i_var' i_type = 'I' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).
  ENDMETHOD.

  METHOD signature_changing_param.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, var type i.`,
      `  obj_test->add_method( changing c_value = var ).`.

    prepare_blackboard( '6,20' ).
    exercise_qfix_contributor( ).
    add_changing( i_id = 'var' i_name = 'c_value' i_type = 'I' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).
  ENDMETHOD.

  METHOD signature_mixed_each_param_typ.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, importing type string, returning type string, exporting type string.`,
      `  obj_test->add_method( exporting i_value = importing importing e_value =  exporting changing c_value = returning EXCEPTIONS OTHERS = 1 ).`.


    prepare_blackboard( '6,20' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = 'importing' i_name = 'i_value' ).
    add_exporting( i_id = 'exporting' i_name = 'e_value' ).
    add_changing( i_id = 'returning' i_name = 'c_value' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).
  ENDMETHOD.

  METHOD mixed_with_inline_data_dec.

    abort_if_wrong_release( 'Inline declarations are not supported below 7.40 release.' ).

    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, text type string.`,
      `  obj_test->add_method( exporting i_value = text importing e_value =  data(val1) changing c_value = data(val2) EXCEPTIONS OTHERS = 1 ).`.

    prepare_blackboard( '6,20' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = 'text' i_name = 'i_value' ).
    add_exporting( i_id = 'data(val1)' i_name = 'e_value' i_type = 'ANY' ).
    add_changing(  i_id = 'data(val2)' i_name = 'c_value' i_type = 'ANY' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).

  ENDMETHOD.

  METHOD signature_mixed_with_receiving.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, importing type string, returning type string, exporting type string.`,
      `  obj_test->add_method(`,
      `   exporting i_value = importing `,
      `   RECEIVING r_value = returning EXCEPTIONS OTHERS = 1 ).`.

    prepare_blackboard( '6,20' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = 'importing' i_name = 'i_value' ).
    add_returning( i_id = 'returning' i_name = 'r_value' ).

    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).
  ENDMETHOD.

  METHOD signature_short_string_pattern.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test.`,
      `  obj_test->add_method( |STR_PATTERN| ).`.

    prepare_blackboard( '6,20' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = '|STR_PATTERN|' i_name = 'i_str_pattern' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).
  ENDMETHOD.


  METHOD signature_export_field_symbol.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test.`,
      `  FIELD-SYMBOLS <field_symbol> TYPE string.`,
      `  obj_test->add_method( field_symbol = <field_symbol> ).`.

    prepare_blackboard( '7,20' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = '<field_symbol>' i_name = 'field_symbol' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).
  ENDMETHOD.

  METHOD signature_receive_field_symbol.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test.`,
      `  FIELD-SYMBOLS <field_symbol> TYPE string.`,
      `  <field_symbol> = obj_test->add_method( ).`.

    prepare_blackboard( '7,35' ).
    exercise_qfix_contributor( ).
    add_returning( i_id = '<field_symbol>' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).
  ENDMETHOD.


  METHOD signature_me.
    source:
       `REPORT dummy.`,
       `class lcl_test definition.`,
       `  public section.`,
       `  methods test.`,
       `endclass.`,
       `class lcl_test implementation.`,
       `  method test.`,
       `    data obj_test type ref to lcl_test.`,
       `    obj_test->add_method( me ).`,
       `  endmethod.`,
       `endclass.`.

    prepare_blackboard( '9,20' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = 'me' i_name = 'i_me' i_type = 'LCL_TEST' i_type_type = if_rfac_adt_res_definitions=>co_parameter_type_type-type_ref_to ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).
  ENDMETHOD.

  METHOD short_signature_with_expr.

    abort_if_wrong_release( 'Type determination for expressions via inline declarations is not possible below 7.40 release.' ).

    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, result type string.`,
      `  result = obj_test->add_method( 2 + 3 ).`.

    prepare_blackboard( '6,30' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = '2 + 3' i_name = 'i_2' i_type = 'I' i_type_type = if_rfac_adt_res_definitions=>co_parameter_type_type-type ).
    add_returning( i_id = 'RESULT' i_name = 'r_result' i_type = 'STRING' i_type_type = if_rfac_adt_res_definitions=>co_parameter_type_type-type ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).
  ENDMETHOD.

  METHOD short_signature_string_templ.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, result type string.`,
      `  result = obj_test->add_method( |bla| && | fasel | && 'soundso' ).`.

    prepare_blackboard( '6,30' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = `|bla| && | fasel | && 'soundso'` i_name = 'I_BLA' i_type = 'STRING' i_type_type = if_rfac_adt_res_definitions=>co_parameter_type_type-type i_with_upper = abap_false ).
    add_returning( i_id = 'RESULT' i_name = 'r_result' i_type = 'STRING' i_type_type = if_rfac_adt_res_definitions=>co_parameter_type_type-type ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).

  ENDMETHOD.

  METHOD medium_signature_with_expr.

    abort_if_wrong_release( 'Type determination for expressions via inline declarations is not possible below 7.40 release.' ).

    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, result type string.`,
      `  result = obj_test->add_method( i_1 = 2 + 3 i_2 = 'bla' && '' ).`.

    prepare_blackboard( '6,30' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = '2 + 3' i_name = 'I_1' i_type = 'I' i_type_type = if_rfac_adt_res_definitions=>co_parameter_type_type-type i_with_upper = abap_false ).
    add_importing( i_id = `'bla' && ''` i_name = 'I_2' i_type = 'STRING' i_type_type = if_rfac_adt_res_definitions=>co_parameter_type_type-type i_with_upper = abap_false ).
    add_returning( i_id = 'result' i_name = 'r_result' i_type = 'STRING' i_type_type = if_rfac_adt_res_definitions=>co_parameter_type_type-type ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).

  ENDMETHOD.

  METHOD long_signature_with_expr.

    abort_if_wrong_release( 'Type determination for expressions via inline declarations is not possible below 7.40 release.' ).

    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, result type string.`,
      `  obj_test->add_method( exporting i_1 = 2 + 3 i_2 = 'bla' && '' importing e_result = result ).`.

    prepare_blackboard( '6,20' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = '2 + 3' i_name = 'I_1' i_type = 'I' i_type_type = if_rfac_adt_res_definitions=>co_parameter_type_type-type i_with_upper = abap_false ).
    add_importing( i_id = `'bla' && ''` i_name = 'I_2' i_type = 'STRING' i_type_type = if_rfac_adt_res_definitions=>co_parameter_type_type-type i_with_upper = abap_false ).
    add_exporting( i_id = `result` i_name = 'e_result' i_type = 'STRING' i_type_type = if_rfac_adt_res_definitions=>co_parameter_type_type-type ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).

  ENDMETHOD.

  METHOD no_proposal_4_super_meth_call.

    sample_code_no_changes:

     1 `REPORT dummy.`,
     2 `class lcl_super definition. public section. methods super. endclass.`,
     3 `class lcl_super implementation. method super. endmethod. endclass.`,
     4 `CLASS lcl_test DEFINITION inheriting from lcl_super. ENDCLASS.`,
     5 `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
     6 `START-OF-SELECTION.`,
     7 `  DATA: obj_test  TYPE REF TO lcl_test, result type string.`,
     8 `  obj_test->super( ).`.

    prepare_blackboard( '8,15' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 0 ).

  ENDMETHOD.

  METHOD no_proposal_after_super.

    sample_code_no_changes:
     1 `REPORT dummy.`,
     2 `CLASS lcl_super DEFINITION. PUBLIC SECTION. METHODS super. ENDCLASS.`,
     3 `CLASS lcl_super IMPLEMENTATION. METHOD super. ENDMETHOD. ENDCLASS.`,
     4 `CLASS lcl_test DEFINITION INHERITING FROM lcl_super. PUBLIC SECTION. METHODS super REDEFINITION.  ENDCLASS.`,
     5 `CLASS lcl_test IMPLEMENTATION.`,
     6  `METHOD super.`,
     7  ` super->cool( ).`,
     8  `ENDMETHOD.`,
     9  `ENDCLASS.`.

    prepare_blackboard( '7,10' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 0 ).

  ENDMETHOD.


  METHOD no_wizard_4_redefined.

    sample_code_no_changes:

     1 `REPORT dummy.`,

     2 `CLASS lcl_super DEFINITION. PUBLIC SECTION. METHODS move. ENDCLASS.`,
     3 `CLASS lcl_super IMPLEMENTATION. METHOD move. ENDMETHOD. ENDCLASS.`,

     4 `CLASS lcl_test DEFINITION INHERITING FROM lcl_super. PUBLIC SECTION. ENDCLASS.`,

     5 `CLASS lcl_test IMPLEMENTATION.`,
     6  `METHOD move.`,
     7  `  `,
     8  `ENDMETHOD.`,
     9  `ENDCLASS.`.

    prepare_blackboard( '6,8' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 1 ).
    assert_proposed( ce_art_qfix=>create_method_def ).

  ENDMETHOD.


  METHOD no_wizard_4_redef_aliased.

    sample_code_no_changes:

     1 `REPORT dummy.`,

     2 `INTERFACE lif. METHODS: move. ENDINTERFACE.`,

     3 `CLASS lcl_super DEFINITION. PUBLIC SECTION. INTERFACES: lif. ALIASES: move FOR lif~move. ENDCLASS.`,
     4 `CLASS lcl_super IMPLEMENTATION. METHOD move. ENDMETHOD. ENDCLASS.`,

     5 `CLASS lcl_test DEFINITION INHERITING FROM lcl_super. PUBLIC SECTION. ENDCLASS.`,

     6 `CLASS lcl_test IMPLEMENTATION.`,
     7  `METHOD move.`,
     8  `  `,
     9  `ENDMETHOD.`,
    10  `ENDCLASS.`.

    prepare_blackboard( '7,8' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 1 ).
    assert_proposed( ce_art_qfix=>create_method_def ).

  ENDMETHOD.


  METHOD signature_inlines.

    abort_if_wrong_release( 'Type determination for returning parameters via inline declarations is not possible below 7.40 release.' ).

    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. public section. methods do returning value(r_v) type string. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. method do. endmethod. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, result type string.`,
      `  cl_aunit_assert=>assert_initial1( act = obj_test->do( ) msg = |{ to_upper( 'hello' ) }| ).`.

    prepare_blackboard( '6,30' ).
    exercise_qfix_contributor( ).
    add_importing( i_name = 'ACT' i_id = 'obj_test->do( )' i_type = 'STRING' ).
    add_importing( i_name = 'MSG' i_id = `|{ to_upper( 'hello' ) }|` i_type = 'STRING' i_with_upper = abap_false ).
    assert_clif_and_method( i_class = 'CL_AUNIT_ASSERT' i_method = 'ASSERT_INITIAL1' i_is_static = abap_true i_parameters = parameters ).
  ENDMETHOD.

  METHOD component_importing.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, token type stokesx.`,
      `  obj_test->add( string = token-str ).`.

    prepare_blackboard( '6,14' ).
    exercise_qfix_contributor( ).
    add_importing( i_name = 'STRING' i_id = 'TOKEN-STR' i_type = 'STRING' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD' i_parameters = parameters ).
  ENDMETHOD.

  METHOD deep_struc_component_importing.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION. ENDCLASS.`,
      `START-OF-SELECTION.`,
      `  TYPES: begin of struc,`,
      `         field type string,`,
      `         token type stokesx,`,
      `         end of struc.`,
      `  DATA: obj_test  TYPE REF TO lcl_test, data type struc.`,
      `  obj_test->add( string = data-token-str ).`.

    prepare_blackboard( '10,14' ).
    exercise_qfix_contributor( ).
    add_importing( i_name = 'STRING' i_id = 'DATA-TOKEN-STR' i_type = 'STRING' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD' i_parameters = parameters ).
  ENDMETHOD.

  METHOD static_call_multi_lines.
    source:
      `REPORT dummy.`,
      `START-OF-SELECTION.`,
      `  cl_aunit_assert=>add(`,
      `    EXPORTING`,
      `      msg = ')' ).`.


    prepare_blackboard( '3,21' ).
    exercise_qfix_contributor( ).
    add_importing( i_name = 'MSG' i_id = `')'` i_type = 'STRING' ).
    assert_clif_and_method( i_class = 'CL_AUNIT_ASSERT' i_method = 'ADD' i_parameters = parameters i_is_static = abap_true ).

  ENDMETHOD.

  METHOD static_call_multi_lines_old.
    source:
      `REPORT dummy.`,
      `START-OF-SELECTION.`,
      `  CALL METHOD cl_aunit_assert=>add`,
      `    EXPORTING`,
      `*TEST comment.`,
      `      msg = ')'.`.

    prepare_blackboard( '3,32' ).
    exercise_qfix_contributor( ).
    add_importing( i_name = 'MSG' i_id = `')'` i_type = 'STRING' ).
    assert_clif_and_method( i_class = 'CL_AUNIT_ASSERT' i_method = 'ADD' i_parameters = parameters i_is_static = abap_true ).

  ENDMETHOD.

  METHOD inline_01.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. public section. methods one. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION.`,
      `method one.`,
      ` add_method( ).`,
      `endmethod.`,
      `ENDCLASS.`.

    prepare_blackboard( '5,5' ).
    exercise_qfix_contributor( ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).
  ENDMETHOD.

  METHOD no_proposal_if_name_too_long.

    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. public section. methods one. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION.`,
      `method one.`,
      ` me->five_five_five_five_five_five_1( ).`,
      `endmethod.`,
      `ENDCLASS.`.

    prepare_blackboard( '5,10' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 0 ).

  ENDMETHOD.

  METHOD inline_string_concat_01.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. public section. methods one. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION.`,
      `method one.`,
      ` data: char5 TYPE c LENGTH 5.`,
      ` add_method( char5 && 'test' && '' ).`,
      `endmethod.`,
      `ENDCLASS.`.

    prepare_blackboard( '6,5' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = `CHAR5 && 'test' && ''` i_name = 'I_CHAR5' i_type = 'STRING' i_with_upper = abap_false ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).
  ENDMETHOD.

  METHOD inline_string_concat_02.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. public section. methods one. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION.`,
      `method one.`,
      ` data: char5 TYPE c LENGTH 5.`,
      ` add_method( test1 = char5 && '' test2 = <any> ).`,
      `endmethod.`,
      `ENDCLASS.`.

    prepare_blackboard( '6,5' ).
    exercise_qfix_contributor( ).
    add_importing( i_id = `CHAR5 && ''` i_name = 'test1' i_type = 'STRING' ).
    add_importing( i_id = '<any>' i_name = 'test2' i_type = 'ANY' ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_method = 'ADD_METHOD' i_parameters = parameters ).
  ENDMETHOD.

  METHOD inline_exists.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. public section. methods one. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION.`,
      `method one.`,
      ` data: char5 TYPE c LENGTH 5.`,
      ` one( ).`,
      `endmethod.`,
      `ENDCLASS.`.

    prepare_blackboard( '6,5' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 0 ).
  ENDMETHOD.

  METHOD inline_static_local.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. public section. class-methods one. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION.`,
      `method one.`,
      ` lcl_test=>add_static_method( ).`,
      `endmethod.`,
      `ENDCLASS.`.

    prepare_blackboard( '5,20' ).
    exercise_qfix_contributor( ).
    assert_clif_and_method( i_class      = 'LCL_TEST' i_method = 'ADD_STATIC_METHOD' i_parameters = parameters i_is_static  = abap_true ).
  ENDMETHOD.

  METHOD inline_static_global.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. public section. class-methods one. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION.`,
      `method one.`,
      ` cl_aunit_assert=>assert_initial2( )..`,
      `endmethod.`,
      `ENDCLASS.`.
    prepare_blackboard( '5,30' ).
    exercise_qfix_contributor( ).
    assert_clif_and_method( i_class = 'CL_AUNIT_ASSERT' i_method = 'ASSERT_INITIAL2' i_is_static = abap_true ).
  ENDMETHOD.

  METHOD inline_static_glob_exists.
    source:
      `REPORT dummy.`,
      `CLASS lcl_test DEFINITION. methods one. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION.`,
      `method one.`,
      ` cl_aunit_assert=>assert_initial( ).`,
      `endmethod.`,
      `ENDCLASS.`.

    prepare_blackboard( '5,30' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 0 ).
  ENDMETHOD.

  METHOD global_interface_method.
    source:
      `CLASS cl_rfac_class_dummy DEFINITION PUBLIC.`,
      `PUBLIC SECTION. `,
      ` INTERFACES IF_RFAC_TEST_UNIMPL_METHODS all methods abstract.`,
      ` methods one.`,
      `endclass.`,
      `CLASS cl_rfac_class_dummy IMPLEMENTATION.`,
      `method one.`,
      ` if_rfac_test_unimpl_methods~wugga_hugga( ).`,
      ` if_rfac_test_unimpl_methods~do_something( ).`,
      `endmethod.`,
      `endclass.`.

    prepare_blackboard( '8,38' ).
    exercise_qfix_contributor( ).
    assert_clif_and_method( i_class = 'CL_RFAC_CLASS_DUMMY' i_interface = 'IF_RFAC_TEST_UNIMPL_METHODS' i_method = 'WUGGA_HUGGA' ).

    "don't react on existing methods
    prepare_blackboard( '9,38' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 0 ).

  ENDMETHOD.

  METHOD local_interface_method_ref.
    source:
      `REPORT dummy.`,
      `INTERFACE if_interface. METHODS if_test. ENDINTERFACE.`,
      `CLASS lcl_test DEFINITION.`,
      `PUBLIC SECTION. INTERFACES if_interface.`,
      `CLASS-METHODS create RETURNING VALUE(r_result) TYPE REF TO lcl_test.`,
      `DATA ref  TYPE REF to if_interface. methods one. ENDCLASS.`,
      `CLASS lcl_test IMPLEMENTATION.`,
      `METHOD create. CREATE OBJECT r_result. ENDMETHOD.`,
      `method one.`,
      `ref = create( ). ref->if_test(  ).`,
      `ref->if_test_not_exist(  ).`,
      `endmethod.`,
      `ENDCLASS.`.

    prepare_blackboard( '11,10' ).
    exercise_qfix_contributor( ).
    assert_clif_and_method( i_class = 'LCL_TEST' i_interface = 'IF_INTERFACE' i_method = 'IF_TEST_NOT_EXIST' ).

    "don't react on existing methods
    prepare_blackboard( '10,25' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 0 ).

  ENDMETHOD.

  METHOD no_proposal_alias_in_interface.

    source:

      `REPORT dummy.`,
      `interface lif_super.                          `,
      `  methods lifsuper01.                         `,
      `endinterface.                                 `,

      `interface lif_test.                           `,
      `  interfaces lif_super.                       `,
      `  aliases lifalias01 for lif_super~lifsuper01.`,
      `endinterface.                                 `,

      `CLASS lcl_test DEFINITION CREATE PRIVATE.     `,
      `  PUBLIC SECTION.                             `,
      `    METHODS m1.                               `,
      `ENDCLASS.                                     `,

      `CLASS lcl_test IMPLEMENTATION.                `,
      `  METHOD m1.                                  `,
      `    data lif type ref to lif_test.            `,
      `    lif->lifalias01( ).                       `,
      `  ENDMETHOD.                                  `,
      `ENDCLASS.                                     `.

    prepare_blackboard( '16,12' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 0 ).

  ENDMETHOD.

  METHOD local_interface_method.

    source:

      `report dummy.`,
      `Interface lif. endinterface.`,

      `class lcl definition.`,
      ` public section. interfaces lif.`,
      `   methods one.`,
      `endclass.`,

      `class lcl implementation.`,
      `method one.`,
      ` lif~wugga_hugga( ).`,
      `endmethod.`,
      `endclass.`.

    prepare_blackboard( '9,10' ).
    exercise_qfix_contributor( ).
    assert_clif_and_method( i_class = 'LCL' i_interface = 'LIF' i_method = 'WUGGA_HUGGA' ).

  ENDMETHOD.

  METHOD local_intf_method_no_decl.

    source:
      `report dummy.`,

      `Interface lif. endinterface.`,

      `class lcl definition.`,
      ` public section.`," !!! interface not declared !!!
      `   methods one.`,
      `endclass.`,

      `class lcl implementation.`,
      `method one.`,
      ` lif~wugga_hugga( ).`,
      `endmethod.`,
      `endclass.`.

    prepare_blackboard( '9,20' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 0 ).

  ENDMETHOD.

ENDCLASS.


CLASS ltc_method_def_imp DEFINITION INHERITING FROM ltc_create_methods_abstract FOR TESTING DURATION MEDIUM RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.

    METHODS:
      method_def_exists             FOR TESTING RAISING cx_static_check,
      interface_method_def_exists   FOR TESTING RAISING cx_static_check,
      missing_method_def            FOR TESTING RAISING cx_static_check,
      missing_interface_method      FOR TESTING RAISING cx_static_check,
      not_responsible_4_missing_imp FOR TESTING RAISING cx_static_check,
      missing_method_in_test_class FOR TESTING RAISING cx_static_check,
      missing_setup_method_in_tc FOR TESTING RAISING cx_static_check,
      check_invalid_testmethod_names FOR TESTING RAISING cx_static_check,
      missing_class_constructor_def FOR TESTING RAISING cx_static_check,
      missing_class_setup_in_tc FOR TESTING RAISING cx_static_check,
      method_name_too_long FOR TESTING RAISING cx_static_check,
      method_exactly_30_chars_long FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS zcl_art_contrib_4_missing_meth DEFINITION LOCAL FRIENDS ltc_method_def_imp.
CLASS ltc_method_def_imp IMPLEMENTATION.

  METHOD method_def_exists.
    source:
      `Report dummy.`,
      `class lcl definition create public.`,
      ` public section.`,
      `   methods test importing a type i.`,
      `endclass.`,
      `class lcl implementation.`,
      `method test.`,
      `endmethod.`,
      `endclass.`.

    prepare_blackboard( '7,10' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 0 ).
  ENDMETHOD.

  METHOD interface_method_def_exists.
    source:
      `Report dummy.`,
      `class lcl definition create public.`,
      ` public section. interfaces if_quickfix_command.`,
      `endclass.`,
      `class lcl implementation.`,
      `method if_quickfix_command~apply.`,
      `endmethod.`,
      `endclass.`.

    prepare_blackboard( '7,20' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 0 ).
  ENDMETHOD.

  METHOD not_responsible_4_missing_imp.
    source:
      `Report dummy.`,
      `class lcl definition.`,
      ` private section.`,
      ` METHODS missing_method.`,
      `endclass.`,
      `class lcl implementation.`,
      `endclass.`.

    prepare_blackboard( '4,20' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 0 ).
  ENDMETHOD.

  METHOD missing_method_def.
    source:
      `Report dummy.`,
      `class lcl definition create public.`,
      `endclass.`,
      `class lcl implementation.`,
      `method test.`,
      `endmethod.`,
      `endclass.`.

    prepare_blackboard( '5,10' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 2 ).
    assert_clif_and_method( i_class = 'LCL' i_method = 'TEST' i_qfix = ce_art_qfix=>create_method_def_dialog  i_visibility = '' && if_rfac_impl_types=>co_method_visibility-private ).
    assert_clif_and_method( i_class = 'LCL' i_method = 'TEST' i_qfix = ce_art_qfix=>create_method_def         i_visibility = '' && if_rfac_impl_types=>co_method_visibility-private ).
  ENDMETHOD.

  METHOD method_name_too_long.
    source:
      `Report dummy.`,
      `class lcl definition create public.`,
      `endclass.`,
      `class lcl implementation.`,
      `method five_five_five_five_five_five_1.`,
      `endmethod.`,
      `endclass.`.

    prepare_blackboard( '5,10' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 0 ).

  ENDMETHOD.

  METHOD method_exactly_30_chars_long.
    source:
      `Report dummy.`,
      `class lcl definition create public.`,
      `endclass.`,
      `class lcl implementation.`,
      `method five_five_five_five_five_five_.`,
      `endmethod.`,
      `endclass.`.

    prepare_blackboard( '5,10' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 2 ).

  ENDMETHOD.



  METHOD missing_method_in_test_class.
    source:
      `Report dummy.`,
      `class lcl definition for testing risk level harmless duration short.`,
      `endclass.`,
      `class lcl implementation.`,
      `method test.`,
      `endmethod.`,
      `endclass.`.

    prepare_blackboard( '5,10' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 3 ).
    assert_clif_and_method( i_class = 'LCL' i_method = 'TEST' i_qfix = ce_art_qfix=>create_method_def_testing  i_visibility = '' && if_rfac_impl_types=>co_method_visibility-private ).
    assert_clif_and_method( i_class = 'LCL' i_method = 'TEST' i_qfix = ce_art_qfix=>create_method_def_dialog  i_visibility = '' && if_rfac_impl_types=>co_method_visibility-private ).
    assert_clif_and_method( i_class = 'LCL' i_method = 'TEST' i_qfix = ce_art_qfix=>create_method_def         i_visibility = '' && if_rfac_impl_types=>co_method_visibility-private ).

  ENDMETHOD.

  METHOD missing_setup_method_in_tc.
    source:
      `Report dummy.`,
      `class lcl definition for testing risk level harmless duration short.`,
      `endclass.`,
      `class lcl implementation.`,
      `method setup.`,
      `endmethod.`,
      `endclass.`.

    prepare_blackboard( '5,10' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 1 ).
    assert_clif_and_method( i_class = 'LCL' i_method = 'SETUP' i_qfix = ce_art_qfix=>create_method_def         i_visibility = '' && if_rfac_impl_types=>co_method_visibility-private ).

  ENDMETHOD.

  METHOD missing_class_setup_in_tc.
    source:
      `Report dummy.`,
      `class lcl definition for testing risk level harmless duration short.`,
      `endclass.`,
      `class lcl implementation.`,
      `method class_setup.`,
      `endmethod.`,
      `endclass.`.

    prepare_blackboard( '5,10' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 1 ).
    assert_clif_and_method( i_class = 'LCL' i_method = 'CLASS_SETUP' i_is_static = abap_true i_qfix = ce_art_qfix=>create_method_def         i_visibility = '' && if_rfac_impl_types=>co_method_visibility-private ).

  ENDMETHOD.

  METHOD check_invalid_testmethod_names.

    cl_abap_unit_assert=>assert_equals( exp = 6 act = lines( zcl_art_contrib_4_missing_meth=>invalid_test_method_names ) ).
    cl_abap_unit_assert=>assert_true( boolc( 'setup'             IN zcl_art_contrib_4_missing_meth=>invalid_test_method_names ) ).
    cl_abap_unit_assert=>assert_true( boolc( 'class_setup'       IN zcl_art_contrib_4_missing_meth=>invalid_test_method_names ) ).
    cl_abap_unit_assert=>assert_true( boolc( 'teardown'          IN zcl_art_contrib_4_missing_meth=>invalid_test_method_names ) ).
    cl_abap_unit_assert=>assert_true( boolc( 'class_teardown'    IN zcl_art_contrib_4_missing_meth=>invalid_test_method_names ) ).
    cl_abap_unit_assert=>assert_true( boolc( 'constructor'       IN zcl_art_contrib_4_missing_meth=>invalid_test_method_names ) ).
    cl_abap_unit_assert=>assert_true( boolc( 'class_constructor' IN zcl_art_contrib_4_missing_meth=>invalid_test_method_names ) ).

  ENDMETHOD.

  METHOD missing_interface_method.
    source:
      `Report dummy.`,
      `class lcl definition create public.`,
      ` public section. interfaces if_quickfix_evaluation.`,
      `endclass.`,
      `class lcl implementation.`,
      `method if_quickfix_evaluation~missing_method.`,
      `endmethod.`,
      `method if_quickfix_evaluation~evaluate.`,
      `endmethod.`,
      `endclass.`.

    prepare_blackboard( '6,40' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 2 ).
    assert_clif_and_method( i_class = 'LCL' i_interface = 'IF_QUICKFIX_EVALUATION' i_method = 'MISSING_METHOD' i_qfix = ce_art_qfix=>create_method_def_dialog  i_visibility = '' && if_rfac_impl_types=>co_method_visibility-public ).
    assert_clif_and_method( i_class = 'LCL' i_interface = 'IF_QUICKFIX_EVALUATION' i_method = 'MISSING_METHOD' i_qfix = ce_art_qfix=>create_method_def         i_visibility = '' && if_rfac_impl_types=>co_method_visibility-public ).
  ENDMETHOD.

  METHOD missing_class_constructor_def.
    source:
      `Report dummy.`,
      `class lcl definition create public.`,
      ` public section.`,
      `endclass.`,
      `class lcl implementation.`,
      `method class_constructor.`,
      `endmethod.`,
      `endclass.`.

    prepare_blackboard( '6,20' ).
    exercise_qfix_contributor( ).
    assert_number_of_proposals( 1 ).
    assert_clif_and_method( i_class = 'LCL' i_method = 'CLASS_CONSTRUCTOR' i_is_static = abap_true i_qfix = ce_art_qfix=>create_method_def         i_visibility = '' && if_rfac_impl_types=>co_method_visibility-public ).

  ENDMETHOD.

ENDCLASS.


CLASS ltc_apply_create_method DEFINITION INHERITING FROM ltc_create_methods_abstract FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      create_instance_method_def FOR TESTING RAISING cx_static_check,
      create_method_from_call    FOR TESTING RAISING cx_static_check,
      create_method_in_chained_call FOR TESTING RAISING cx_static_check,
      create_method_def_wizard FOR TESTING RAISING cx_static_check,
      create_method_returning_any FOR TESTING RAISING cx_static_check,
      create_constructor_method_def FOR TESTING RAISING cx_static_check,

      prepare_evaluation_result
        IMPORTING qfix            TYPE REF TO ce_art_qfix
        RETURNING VALUE(r_result) TYPE REF TO cl_art_proposal,

      exercise_qfix
        IMPORTING
          type TYPE REF TO ce_art_qfix
          pos  TYPE csequence
          old  TYPE csequence OPTIONAL
          new  TYPE csequence OPTIONAL
        RAISING
          cx_quickfix_error.


    DATA: source_object_double TYPE REF TO if_quickfix_source_object,
          proposal             TYPE REF TO cl_art_proposal,
          act_result           TYPE REF TO if_quickfix_result.

ENDCLASS.


CLASS ltc_apply_create_method IMPLEMENTATION.

  METHOD create_instance_method_def.

    sample_code:

     1 `report dummy.`                    ``,
     2 `class lcl definition.`            ``,
     3 `private section.`                 `append:#    METHODS one.`,
     4 `endclass.`                        ``,
     5 `class lcl implementation.`        ``,
     6 `method one. endmethod.`           ``,
     7 `endclass.`                        ``.

    exercise_qfix( type = ce_art_qfix=>create_method_def pos = '6,8' ).

  ENDMETHOD.

  METHOD create_constructor_method_def.

    sample_code:

     1 `report dummy.`                       ``,
     2 `class lcl definition create public.` `append:#  PUBLIC SECTION.##    METHODS constructor.`,
     3 `private section.`                    ``,
     4 `endclass.`                           ``,
     5 `class lcl implementation.`           ``,
     6 `method constructor. endmethod.`      ``,
     7 `endclass.`                           ``.

    exercise_qfix( type = ce_art_qfix=>create_method_def pos = '6,8' ).

  ENDMETHOD.

  METHOD create_method_def_wizard.

    sample_code:

     1 `report dummy.`                    ``,
     2 `class lcl definition.`            ``,
     3 `private section.`                 `append:#    METHODS two.`,
     4 `endclass.`                        ``,
     5 `class lcl implementation.`        ``,
     6 `method one. endmethod.`           `replace:one ~> two`,
     7 `endclass.`                        ``.

    exercise_qfix( type = ce_art_qfix=>create_method_def pos = '6,8' old = 'one' new = 'two' ).

  ENDMETHOD.

  METHOD create_method_from_call.

    sample_code:

     1 `report dummy.`                     ``,
     2 `class lcl definition.`             `append:#  public section.##    class-methods new_static_method.`,
     3 `endclass.`                         ``,
     4 `class lcl implementation.`         ``,
     5 `endclass.`                         `prepend:#  method new_static_method.##  endmethod.##`,
     6 `START-OF-SELECTION.`               ``,
     7 `  lcl=>new_static_method( ).`      ``.

    exercise_qfix( type = ce_art_qfix=>create_method_from_call pos = '7,20' ).

    RETURN.

  ENDMETHOD.

  METHOD create_method_in_chained_call.

    sample_code:

      1  `report dummy.` ``,
      2  `class lcl definition.` ``,
      3  ` public section.` ``,
      4  `  methods existing.` `append:#    methods new_method#      returning#        value(r_result) type ref to object.`,
      5  `endclass.` ``,
      6  `class lcl implementation.` ``,
      7  `endclass.` `prepend:#  method new_method.##  endmethod.##`,
      8  `START-OF-SELECTION.` ``,
      9  `  data obj type ref to lcl.` ``,
      10 `  create object lcl.` ``,
      11 `  obj->new( )->existing( ).` `replace:obj->new( ) ~> obj->new_method( )`.

    exercise_qfix( type = ce_art_qfix=>create_method_from_call pos = '11,8' old = 'new' new = 'new_method' ).

  ENDMETHOD.

  METHOD create_method_returning_any.

    sample_code:

      1  `report dummy.` ``,
      2  `class lcl definition.` `append:#  public section.##    methods new_method#      returning#        value(r_result) type any.`,
      3  `endclass.` ``,
      4  `class lcl implementation.` ``,
      5  `endclass.` `prepend:#  method new_method.##  endmethod.##`,
      6  `START-OF-SELECTION.` ``,
      7  `  DATA(obj) = NEW lcl( ).` ``,
      8 `  DATA(field) = obj->new_method( ).` ``.

    exercise_qfix( type = ce_art_qfix=>create_method_from_call pos = '8,24' old = 'new_method' new = 'new_method' ).

  ENDMETHOD.

  METHOD prepare_evaluation_result.

    prep_blackboard_by_source_obj( source_object_double ).
    exercise_qfix_contributor( ).

    DATA: proposal TYPE REF TO cl_art_proposal.

    LOOP AT me->blackboard->get_proposals( ) INTO proposal.
      IF proposal->qfix->id = qfix->id.
        r_result = proposal.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD exercise_qfix.

    DATA: user_content TYPE string.


    source_object_double = create_source_object_double( i_source_code = source_code i_selection = pos ).
    proposal    = prepare_evaluation_result( type ).

    user_content = proposal->if_quickfix_evaluation_result~get_user_content( ).

    "simulate rename of the new method in wizard
    IF old IS SUPPLIED AND new IS SUPPLIED.
      REPLACE old IN user_content WITH new IGNORING CASE.
    ENDIF.

    proposal->set_user_content( user_content ).

    cut ?= create_quickfix( i_proposal = proposal i_source_object = source_object_double ).
    act_result = cut->if_qfix_quickfix~apply( input = source_object_double
                                                evaluation_result = proposal ).

    act_deltas = act_result->get_source_deltas( ).

    verify_deltas( ).


  ENDMETHOD.

ENDCLASS.

CLASS lth_4_lcl_method_call DEFINITION ABSTRACT INHERITING FROM zth_art_contributor FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PROTECTED SECTION.

    METHODS:
      prepare_class_under_test
        IMPORTING i_code_of_statment TYPE string
        RETURNING VALUE(r_result)    TYPE REF TO lcl_method_call,
      inject_method_end_index
        IMPORTING i_index TYPE i.

    DATA: cut TYPE REF TO lcl_method_call.
  PRIVATE SECTION.


ENDCLASS.

CLASS lth_4_lcl_method_call IMPLEMENTATION.

  METHOD prepare_class_under_test.

    DATA: blackboard               TYPE REF TO cl_art_blackboard,
          sample_code              TYPE string_table,
          dummy_wbobjtype          TYPE wbobjtype,
          scan_result              TYPE REF TO if_rfac_scan_result,
          unqualified_tokens_empty TYPE if_ris_adt_source_handler=>ty_t_token,
          qualified_tokens         TYPE if_ris_adt_source_handler=>ty_t_token,
          source_position          TYPE REF TO cl_pst_source_position.

    APPEND i_code_of_statment TO sample_code.

    source_repository->register_source( i_include = 'DUMMY' i_source = sample_code ).

    scan_result = cl_rfac_scan_result=>create_for_single_include( 'DUMMY' ).

    blackboard = cl_art_blackboard=>create( ).

    source_position = cl_pst_source_position=>create_from_string(
                            i_main_prog       = 'DUMMY'
                            i_include         = 'DUMMY'
                            i_position_string = '1,0' ).

    blackboard->set_source_selection_info(
        i_transport_obj_name = 'DUMMY'
        i_wb_object_type     = dummy_wbobjtype
        i_source_position = source_position ).

    blackboard->set_scan_result( scan_result ).

    scan_result->get_qualified_scan_for_stmnt( EXPORTING i_include = 'DUMMY' i_statement_index = 1
                                               IMPORTING e_tokens = qualified_tokens ).

    CREATE OBJECT r_result
      EXPORTING
        i_blackboard         = blackboard
        i_tokens_qualified   = qualified_tokens
        i_tokens_unqualified = unqualified_tokens_empty.

  ENDMETHOD.

  METHOD inject_method_end_index.
    cut->last_token_of_method = i_index.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_find_expression DEFINITION FINAL INHERITING FROM lth_4_lcl_method_call FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      verify_expression IMPORTING exp           TYPE string
                                  i_token_tabix TYPE i,

      short_signature FOR TESTING,
      short_signature_string_expr FOR TESTING,
      short_signature_mixed_string FOR TESTING,
      short_signature_with_meth FOR TESTING,
      medium_signature_one_parameter FOR TESTING,
      medium_signature_two_parameter FOR TESTING,
      long_signature_many_parameter FOR TESTING,
      with_nested_and_chained_calls FOR TESTING,
      nested_call_with_string_expr FOR TESTING.

ENDCLASS.


CLASS ltc_find_expression IMPLEMENTATION.

  METHOD short_signature.

    cut = prepare_class_under_test( `result = new_method( 3 * 3 ).` ).
    inject_method_end_index( 7 ).
    verify_expression( exp = '3 * 3' i_token_tabix = 4 ).

  ENDMETHOD.

  METHOD short_signature_string_expr.

    cut = prepare_class_under_test( `result = new_method( |{ var1 } bla fasel so { 1 + 2 } und so| ).` ).
    inject_method_end_index( 19 ).
    verify_expression( exp = '|{ var1 } bla fasel so { 1 + 2 } und so|' i_token_tabix = 4 ).

  ENDMETHOD.

  METHOD short_signature_mixed_string.

    cut = prepare_class_under_test( `result = new_method( |{ var1 } bla| && ' fasel so ' && |{ 1 + 2 } und so| ).` ).
    inject_method_end_index( 25 ).
    verify_expression( exp = `|{ var1 } bla| && ' fasel so ' && |{ 1 + 2 } und so|` i_token_tabix = 4 ).

  ENDMETHOD.

  METHOD short_signature_with_meth.

    cut = prepare_class_under_test( `result = new_method( other_method( 'abc' ) + 3 ).` ).
    inject_method_end_index( 9 ).
    verify_expression( exp = `OTHER_METHOD( 'abc' ) + 3` i_token_tabix = 4 ).

  ENDMETHOD.

  METHOD medium_signature_one_parameter.

    cut = prepare_class_under_test( `result = new_method( i_input = 3 * 3 ).` ).
    inject_method_end_index( 9 ).
    verify_expression( exp = '3 * 3' i_token_tabix = 6 ).

  ENDMETHOD.

  METHOD medium_signature_two_parameter.

    cut = prepare_class_under_test( `result = new_method( i_1 = 3 * 3 i_2 = 'bla' && 'fasel' ).` ).
    inject_method_end_index( 14 ).
    verify_expression( exp = '3 * 3' i_token_tabix = 6 ).
    verify_expression( exp = `'bla' && 'fasel'` i_token_tabix = 11 ).

  ENDMETHOD.

  METHOD long_signature_many_parameter.

    cut = prepare_class_under_test( `result = new_method( exporting i_1 = 3 * 3 importing e_bla = variable changing c_2 = onkel_hugo ).` ).
    inject_method_end_index( 24 ).
    verify_expression( exp = '3 * 3' i_token_tabix = 7 ).
    verify_expression( exp = 'VARIABLE' i_token_tabix = 13 ).
    verify_expression( exp = `ONKEL_HUGO` i_token_tabix = 17 ).

  ENDMETHOD.

  METHOD with_nested_and_chained_calls.

    cut = prepare_class_under_test( `result = new_method( i_1 = 3 * 3 i_2 = cl_bla=>create( 1 )->do( i_1 = 'abc' i_2 = me->get_x( 2 ) ) ).` ).
    inject_method_end_index( 23 ).
    verify_expression( exp = '3 * 3' i_token_tabix = 6 ).
    verify_expression( exp = `CL_BLA=>CREATE( 1 )->DO( I_1 = 'abc' I_2 = ME->GET_X( 2 ) )` i_token_tabix = 11 ).

  ENDMETHOD.

  METHOD nested_call_with_string_expr.

    cut = prepare_class_under_test( `result = new_method( obj->do( i_1 = |abc { var }| && 'bla' ) ).` ).
    inject_method_end_index( 16 ).
    verify_expression( exp = `OBJ->DO( I_1 = |abc { var }| && 'bla' )` i_token_tabix = 4 ).

  ENDMETHOD.

  METHOD verify_expression.

    DATA act TYPE string.

    cut->extract_actual_parameter( EXPORTING i_start_token_index = i_token_tabix IMPORTING e_actual = act ).
    cl_abap_unit_assert=>assert_equals( exp = exp act = act ).

  ENDMETHOD.


ENDCLASS.

CLASS ltc_find_closing_token DEFINITION DEFERRED.
CLASS zcl_art_contrib_4_missing_meth DEFINITION LOCAL FRIENDS ltc_find_closing_token.

CLASS ltc_find_closing_token DEFINITION FINAL INHERITING FROM lth_4_lcl_method_call FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      classic_call_method_no_params  FOR TESTING,
      simple_call_no_params          FOR TESTING,
      simple_call_short_single_param FOR TESTING,
      call_large_signature           FOR TESTING,
      chained_calls                  FOR TESTING,
      nested_calls                   FOR TESTING,
      nested_chained_calls           FOR TESTING.

ENDCLASS.


CLASS ltc_find_closing_token IMPLEMENTATION.

  METHOD classic_call_method_no_params.

    cut = prepare_class_under_test( `CALL METHOD new_method.` ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = cut->find_closing_token_of_method( i_token_index_method_name = 3 ) ).

  ENDMETHOD.

  METHOD simple_call_no_params.

    cut = prepare_class_under_test( `new_method( ).` ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = cut->find_closing_token_of_method( i_token_index_method_name = 1 ) ).

  ENDMETHOD.

  METHOD simple_call_short_single_param.

    cut = prepare_class_under_test( `new_method( variable ).` ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = cut->find_closing_token_of_method( i_token_index_method_name = 1 ) ).

  ENDMETHOD.

  METHOD call_large_signature.

    cut = prepare_class_under_test( `new_method( exporting iv1 = var1 iv2 = ')bla' importing ev1 = var2 changing cv1 = var3 ).` ).
    cl_abap_unit_assert=>assert_equals( exp = 17 act = cut->find_closing_token_of_method( i_token_index_method_name = 1 ) ).

  ENDMETHOD.

  METHOD nested_chained_calls.

    cut = prepare_class_under_test( `object->run( iv1 = lcl=>create( )->do( 123 ) iv2 = me->find_service( 'bla' )->get_settings( )->is_ok( ) ).` ).
    cl_abap_unit_assert=>assert_equals( exp = 15 act = cut->find_closing_token_of_method( i_token_index_method_name = 1 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 5  act = cut->find_closing_token_of_method( i_token_index_method_name = 4 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 7 act = cut->find_closing_token_of_method( i_token_index_method_name = 5 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 12 act = cut->find_closing_token_of_method( i_token_index_method_name = 10 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 13 act = cut->find_closing_token_of_method( i_token_index_method_name = 12 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 14 act = cut->find_closing_token_of_method( i_token_index_method_name = 13 ) ).

  ENDMETHOD.

  METHOD nested_calls.

    cut = prepare_class_under_test( `result = object->new_method( iv1 = me->do( 123 ) iv2 = me->dodo( 'bla' ) iv3 = 'fasel' ).` ).
    cl_abap_unit_assert=>assert_equals( exp = 17 act = cut->find_closing_token_of_method( i_token_index_method_name = 3 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 8  act = cut->find_closing_token_of_method( i_token_index_method_name = 6 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 13 act = cut->find_closing_token_of_method( i_token_index_method_name = 11 ) ).

  ENDMETHOD.

  METHOD chained_calls.

    cut = prepare_class_under_test( `result = class=>create( ')bla(' )->get_handler( i_id = 123 )->do( 'bla' ).` ).
    cl_abap_unit_assert=>assert_equals( exp = 5  act = cut->find_closing_token_of_method( i_token_index_method_name = 3 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 9  act = cut->find_closing_token_of_method( i_token_index_method_name = 5 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 11 act = cut->find_closing_token_of_method( i_token_index_method_name = 9 ) ).

  ENDMETHOD.

ENDCLASS.
