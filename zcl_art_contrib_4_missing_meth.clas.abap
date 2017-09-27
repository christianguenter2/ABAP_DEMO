class ZCL_ART_CONTRIB_4_MISSING_METH definition
  public
  final
  create public .

public section.

  interfaces IF_ART_BLACKBOARD_CONTRIBUTOR .
  interfaces IF_QFIX_QUICKFIX .

  class-methods CREATE
    importing
      !I_BLACKBOARD type ref to CL_ART_BLACKBOARD
    returning
      value(R_INSTANCE) type ref to ZCL_ART_CONTRIB_4_MISSING_METH .
  class-methods CLASS_CONSTRUCTOR .
private section.

  types:
    ty_range_of_seocpdname TYPE RANGE OF seocpdname .

  data METHOD_DESCRIPTION type ref to CL_RFAC_METHOD_DESCRIPTION .
  data BLACKBOARD type ref to CL_ART_BLACKBOARD .
  data CLASS_NAME type STRING .
  data AFFECTED_INCLUDES type IF_RIS_INCLUDE=>TAB .
  class-data INVALID_TEST_METHOD_NAMES type TY_RANGE_OF_SEOCPDNAME .
  class-data STATIC_TEST_FIXTURE_METHODS type TY_RANGE_OF_SEOCPDNAME .

  methods ADD_EVAL_FOR_CREATE_METHOD_DEF
    importing
      !I_PST_CLASS type ref to CL_PST_CLASS
      !I_PST_METHOD_IMP type ref to CL_PST_METHOD_IMPLEMENTATION .
  methods ADD_EVAL_FOR_CREATE_FROM_CALL
    importing
      !I_METHOD_NAME type STRING
      !I_CLASS4NEW_METHOD type ref to CL_PST_CLASS
      !I_CREATE_METHOD_CALL_OBJECT type ref to LCL_METHOD_CALL .
  methods ADD_EVAL_CREATE_DEF_INTF_METH
    importing
      !I_METHOD_NAME type STRING
      !I_INTFS4NEW_METHOD type ref to CL_PST_INTERFACE
      !I_IS_STATIC type ABAP_BOOL .
  methods CREATE_RIS_INCLUDE
    importing
      !I_INCLUDE type PROGRAMM
    returning
      value(R_RESULT) type ref to IF_RIS_INCLUDE
    raising
      CX_QUICKFIX_ERROR .
  methods GET_METHOD_FROM_USER_CONTENT
    importing
      !I_EVALUATION_RESULT type ref to IF_QUICKFIX_EVALUATION_RESULT
    returning
      value(R_METHOD_DESCRIPTION) type ref to CL_RFAC_METHOD_DESCRIPTION
    raising
      CX_RFAC_ERROR
      CX_QUICKFIX_ERROR .
  methods GET_METHOD_DESCRIPTION
    importing
      !I_EVALUATION_RESULT type ref to IF_QUICKFIX_EVALUATION_RESULT
    returning
      value(R_METHOD_DESCRIPTION) type ref to CL_RFAC_METHOD_DESCRIPTION
    raising
      CX_RFAC_ERROR
      CX_QUICKFIX_ERROR .
  class-methods ADD_EQUAL_ENTRY_TO_RANGE
    importing
      !I_METHOD_NAME type SEOCMPNAME
    changing
      !C_RANGE_TAB type zCL_ART_CONTRIB_4_MISSING_METH=>TY_RANGE_OF_SEOCPDNAME .
  methods BUILD_QUICKFIX_RESULT
    returning
      value(R_RESULT) type ref to IF_QUICKFIX_RESULT
    raising
      CX_ADT_URI_MAPPING .
  methods BUILD_CURSOR_SELECTION
    importing
      !I_RIS_DEF_INCLUDE type ref to IF_RIS_INCLUDE
      !I_RIS_IMPL_INCLUDE type ref to IF_RIS_INCLUDE
    returning
      value(R_RESULT) type ref to CL_QUICKFIX_SOURCE_OBJECT .
  methods GET_LINES_COUNT_OF_DEFINITION
    importing
      !I_RIS_INCLUDE type ref to IF_RIS_INCLUDE
    returning
      value(R_ROW) type I .
  methods GET_START_ROW_OF_IMPLEMETATION
    importing
      !I_RIS_INCLUDE type ref to IF_RIS_INCLUDE
    returning
      value(R_ROW) type I .
  methods ADD_QF_CREATE_METHOD
    importing
      !I_QFIX type ref to CE_ART_QFIX
      !I_METHOD_DATA type IF_RFAC_IMPL_TYPES=>TS_METHOD .
  methods DERIVE_RANGE_FROM_METHOD_IMP
    importing
      !I_NODE type ref to CL_PST_NODE
      !I_OLD_METHOD_NAME type CSEQUENCE
      !I_NEW_METHOD_NAME type CSEQUENCE
    returning
      value(R_RESULT) type IF_RIS_INCLUDE=>TS_RANGE .
  methods BUILD_METHOD_DESCRIPTION
    importing
      !I_METHOD_DATA type IF_RFAC_IMPL_TYPES=>TS_METHOD
    returning
      value(R_RESULT) type ref to CL_RFAC_METHOD_DESCRIPTION .
  methods BUILD_CREATE_METHOD_CALL
    returning
      value(R_RESULT) type ref to LCL_METHOD_CALL .
  methods TRANSF_METHOD_DESC_TO_USR_CONT
    importing
      !I_METHOD_DESCRIPTION type ref to CL_RFAC_METHOD_DESCRIPTION
    returning
      value(R_RESULT) type STRING .
  methods GET_PST_NODE4METHOD
    importing
      !I_CLIF_NAME type STRING
      !I_METHOD_NAME type STRING
      !I_ROOT_NODE type ref to CL_PST_NODE
    returning
      value(R_RESULT) type ref to CL_PST_NODE .
ENDCLASS.



CLASS ZCL_ART_CONTRIB_4_MISSING_METH IMPLEMENTATION.


  METHOD ADD_EQUAL_ENTRY_TO_RANGE.

    DATA: entry LIKE LINE OF c_range_tab.

    entry-sign = 'I'.
    entry-option = 'EQ'.
    entry-low = i_method_name.

    APPEND entry TO c_range_tab.

  ENDMETHOD.


  METHOD ADD_EVAL_CREATE_DEF_INTF_METH.
    DATA: method_data                TYPE if_rfac_impl_types=>ts_method.

    method_data-interface_name     = i_intfs4new_method->name.
    method_data-classname          = i_intfs4new_method->name.
    method_data-name               = i_method_name.
    method_data-visibility         = if_rfac_impl_types=>co_method_visibility-public.
    method_data-is_static          = i_is_static.
    method_data-definition_include = i_intfs4new_method->get_definition_include( ).

    me->method_description = build_method_description( method_data ).

    add_qf_create_method(
      i_qfix             = ce_art_qfix=>create_method_from_call
      i_method_data      = method_data ).


  ENDMETHOD.


  METHOD ADD_EVAL_FOR_CREATE_FROM_CALL.

    DATA: method_data           TYPE if_rfac_impl_types=>ts_method,
          implemented_interface TYPE REF TO cl_pst_interface.

    method_data-call_include = me->blackboard->get_focused_include( ).
    method_data-definition_include = i_class4new_method->get_definition_include( ).
    method_data-implemention_include = i_class4new_method->get_implementation_include( ).

    IF i_method_name CS '~'.
      "interface method
      method_data-interface_name = segment( val = i_method_name index = 1 sep = '~' ).
      method_data-name           = segment( val = i_method_name index = 2 sep = '~' ).
      method_data-classname      = i_class4new_method->name.
      method_data-visibility     = if_rfac_impl_types=>co_method_visibility-public.

      implemented_interface = i_class4new_method->find_implemented_interface( method_data-interface_name ).
      IF implemented_interface IS INITIAL.
        RETURN. " abort if interface is not implemented in the class which should implement an interface method
      ELSE.
        method_data-definition_include = implemented_interface->get_definition_include( ).
      ENDIF.

    ELSE.
      method_data-name      = i_method_name.
      method_data-classname = i_class4new_method->name.
    ENDIF.

    IF strlen( method_data-name ) > 30.
      RETURN.
    ENDIF.

    method_data-is_static = i_create_method_call_object->is_static( ).
    method_data-call_reference = i_create_method_call_object->get_call_reference( i_token_string = me->blackboard->get_selection_content( ) ).
    IF me->class_name = i_class4new_method->name.
      "call is in the same class as the definition -> make it a private method
      method_data-visibility = if_rfac_impl_types=>co_method_visibility-private.
    ELSE.
      method_data-visibility = if_rfac_impl_types=>co_method_visibility-public.
    ENDIF.
    i_create_method_call_object->parse_signature( CHANGING method_descr = method_data ).

    me->method_description = build_method_description( method_data ).

    add_qf_create_method(
        i_qfix             = ce_art_qfix=>create_method_from_call
        i_method_data      = method_data ).

  ENDMETHOD.


  METHOD ADD_EVAL_FOR_CREATE_METHOD_DEF.

    DATA: method_data                TYPE if_rfac_impl_types=>ts_method,
          superclass_candidate       TYPE REF TO cl_pst_class,
          redefined_method_candidate TYPE REF TO cl_pst_node,
          alias_ref_node             TYPE REF TO cl_pst_node,
          pst_interface              TYPE REF TO cl_pst_interface,
          avoid_creation_by_wizard   TYPE abap_bool.

    method_data-classname = i_pst_class->name.

    IF i_pst_method_imp->is_interface_method( ) = abap_true.
      method_data-name           = substring_after(  val = i_pst_method_imp->name sub = '~' ).
      method_data-interface_name = substring_before( val = i_pst_method_imp->name sub = '~' ).
      method_data-visibility     = if_rfac_impl_types=>co_method_visibility-public.

      pst_interface = i_pst_class->find_implemented_interface( method_data-interface_name ).
      IF pst_interface IS BOUND.
        method_data-definition_include = pst_interface->get_definition_include( ).
      ENDIF.

    ELSE.
      method_data-name = i_pst_method_imp->name.
      method_data-definition_include = i_pst_class->get_definition_include( ).

      "check if the method definition should be a redefinition
      superclass_candidate = i_pst_class->get_superclass( ).
      IF superclass_candidate IS BOUND.
        redefined_method_candidate = superclass_candidate->find_member( i_member_name = method_data-name i_inheritable_only = abap_true ).
      ENDIF.
      IF redefined_method_candidate IS BOUND.
        "non final inheritable method with same name exists in super class
        IF redefined_method_candidate->kind->is_kind_of( ce_pst_kind=>any_instance_method ) = abap_true AND redefined_method_candidate->get_property( cl_pst_node=>co_properties-is_final ) = abap_false.
          method_data-is_redefinition = abap_true.
          method_data-visibility = redefined_method_candidate->get_property( cl_pst_node=>co_properties-visibility ).
        ENDIF.

        "alias which references a method with same name exists in super class
        IF redefined_method_candidate->kind->is_kind_of( ce_pst_kind=>alias ) = abap_true.
          alias_ref_node = redefined_method_candidate->find_last_child( ce_pst_kind=>alias_reference ).
          IF alias_ref_node->reference_node->kind->is_kind_of( ce_pst_kind=>any_instance_method ) = abap_true.
            method_data-is_redefinition = abap_true.
            method_data-visibility = alias_ref_node->reference_node->get_property( cl_pst_node=>co_properties-visibility ).
          ENDIF.
        ENDIF.

      ENDIF.

      IF method_data-is_redefinition = abap_true.
        avoid_creation_by_wizard = abap_true.
      ELSE.
        IF to_lower( method_data-name ) = 'class_constructor'.
          method_data-is_static = abap_true.
          method_data-visibility = if_rfac_impl_types=>co_method_visibility-public.
          avoid_creation_by_wizard = abap_true.
        ELSEIF to_lower( method_data-name ) = 'constructor'.
          method_data-visibility = i_pst_class->get_creation_visibility( ).
        ELSE.
          method_data-visibility = if_rfac_impl_types=>co_method_visibility-private.
        ENDIF.

        "inside of a test class
        IF i_pst_class->is_declared_for_testing( ) = abap_true.
          IF to_lower( method_data-name ) IN invalid_test_method_names.
            avoid_creation_by_wizard = abap_true.
            IF to_lower( method_data-name ) IN static_test_fixture_methods.
              method_data-is_static = abap_true.
            ENDIF.
          ELSE.
            method_data-is_for_testing = abap_true.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.

    IF strlen( method_data-name ) > 30.
      RETURN.
    ENDIF.

    me->method_description = build_method_description( method_data ).

    IF method_data-is_for_testing = abap_true.

      add_qf_create_method(
        i_qfix             = ce_art_qfix=>create_method_def_testing
        i_method_data      = method_data ).

      method_data-is_for_testing = abap_false.

    ENDIF.

    add_qf_create_method(
      i_qfix             = ce_art_qfix=>create_method_def
      i_method_data      = method_data ).

    IF avoid_creation_by_wizard = abap_false.

      add_qf_create_method(
        i_qfix             = ce_art_qfix=>create_method_def_dialog
        i_method_data      = method_data ).

    ENDIF.


  ENDMETHOD.


  METHOD ADD_QF_CREATE_METHOD.

    DATA: proposal  TYPE REF TO cl_art_proposal,
          clif_name TYPE string.

    IF i_method_data-interface_name IS INITIAL.
      clif_name = i_method_data-classname.
    ELSE.
      clif_name = i_method_data-interface_name.
    ENDIF.

    proposal = i_qfix->create_proposal(
      i_clif_name   = clif_name
      i_method_name = i_method_data-name
      i_class_name  = i_method_data-classname
    ).

    proposal->set_user_content( transf_method_desc_to_usr_cont( build_method_description( i_method_data ) ) ).
    blackboard->add_proposal( proposal ).

  ENDMETHOD.


  METHOD BUILD_CREATE_METHOD_CALL.

    DATA: unqualified_tokens TYPE if_ris_adt_source_handler=>ty_t_token,
          qualified_tokens   TYPE if_ris_adt_source_handler=>ty_t_token.

    blackboard->get_scan_result( )->get_unqualified_scan_for_stmnt(
        EXPORTING i_statement_index = blackboard->get_start_stmnt_index( )
                  i_include         = blackboard->get_focused_include( )
        IMPORTING e_tokens          = unqualified_tokens ).

    blackboard->get_scan_result( )->get_qualified_scan_for_stmnt(
        EXPORTING i_statement_index = blackboard->get_start_stmnt_index( )
                  i_include         = blackboard->get_focused_include( )
        IMPORTING e_tokens          = qualified_tokens ).

    CREATE OBJECT r_result
      EXPORTING
        i_tokens_qualified   = qualified_tokens
        i_tokens_unqualified = unqualified_tokens
        i_blackboard         = blackboard.

  ENDMETHOD.


  METHOD BUILD_CURSOR_SELECTION.
    DATA:
      row_shift               TYPE i,
      start_of_implementation TYPE i.

    row_shift = 2.

    CHECK i_ris_impl_include IS NOT INITIAL.
    IF i_ris_def_include IS NOT INITIAL AND i_ris_impl_include IS NOT INITIAL AND
       i_ris_def_include->name EQ i_ris_impl_include->name.
      row_shift = row_shift + get_lines_count_of_definition( i_ris_def_include ).
    ENDIF.

    start_of_implementation = get_start_row_of_implemetation( i_ris_impl_include ).

    DATA: source_position TYPE REF TO cl_pst_source_position.

    source_position = cl_pst_source_position=>create(
      "i_main_prog = i_main_prog
      i_include   = i_ris_impl_include->name
      i_start_row = start_of_implementation + row_shift
      i_start_col = 1 ).

    r_result = cl_quickfix_source_object=>create(
     i_wb_object       = cl_rfac_utility=>get_wb_object_for_include( i_include = i_ris_impl_include->name )
     i_source_position = source_position ).

  ENDMETHOD.


  METHOD BUILD_METHOD_DESCRIPTION.

    r_result = cl_rfac_method_description=>create( ).
    r_result->data = i_method_data.

  ENDMETHOD.


  METHOD BUILD_QUICKFIX_RESULT.
    DATA: include TYPE REF TO if_ris_include,
          range   LIKE LINE OF include->ranges,
          delta   TYPE REF TO if_art_source_delta.

    r_result = blackboard->get_qfix_result( ).

    LOOP AT affected_includes INTO include.

      LOOP AT include->ranges INTO range.

        delta = cl_art_delta_factory=>create_delta_missing_method(
            i_mainprogram = include->main_program
            i_include     = include->name
            i_include_range = range ).

        r_result->add_delta( delta ).

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD CLASS_CONSTRUCTOR.

    add_equal_entry_to_range( EXPORTING i_method_name = 'setup'             CHANGING c_range_tab = invalid_test_method_names   ).
    add_equal_entry_to_range( EXPORTING i_method_name = 'teardown'          CHANGING c_range_tab = invalid_test_method_names   ).
    add_equal_entry_to_range( EXPORTING i_method_name = 'class_teardown'    CHANGING c_range_tab = invalid_test_method_names   ).
    add_equal_entry_to_range( EXPORTING i_method_name = 'class_setup'       CHANGING c_range_tab = invalid_test_method_names   ).
    add_equal_entry_to_range( EXPORTING i_method_name = 'constructor'       CHANGING c_range_tab = invalid_test_method_names   ).
    add_equal_entry_to_range( EXPORTING i_method_name = 'class_constructor' CHANGING c_range_tab = invalid_test_method_names   ).
    add_equal_entry_to_range( EXPORTING i_method_name = 'class_setup'       CHANGING c_range_tab = static_test_fixture_methods ).
    add_equal_entry_to_range( EXPORTING i_method_name = 'class_teardown'    CHANGING c_range_tab = static_test_fixture_methods ).

  ENDMETHOD.


  METHOD CREATE.
    CREATE OBJECT r_instance.
    r_instance->blackboard = i_blackboard.
  ENDMETHOD.


  METHOD CREATE_RIS_INCLUDE.

    DATA: source TYPE rswsourcet.

    CHECK i_include IS NOT INITIAL.

    r_result = cl_ris_include=>create( i_name = i_include ).

    " set source try to get source from the blackboard first
    source =  me->blackboard->get_scan_result( )->get_source_of_include( i_include = i_include ).
    IF source IS INITIAL.
      r_result->read_source( IMPORTING e_source  = source ).
    ENDIF.
    r_result->set_source( i_source = source ).

  ENDMETHOD.


  METHOD DERIVE_RANGE_FROM_METHOD_IMP.
    DATA token_method_name TYPE cl_pst_statement=>ty_s_token.
    token_method_name = i_node->first_statement->get_last_local_token( ).

    r_result-old_content = i_old_method_name.
    r_result-new_content = to_lower( i_new_method_name ).
    r_result-start_row =  r_result-end_row = token_method_name-row.
    r_result-start_col = token_method_name-col.
    r_result-end_col = token_method_name-col + token_method_name-len1.
    r_result-kind = if_ris_occurrence_finder=>occurrence_kind-imp.

  ENDMETHOD.


  METHOD GET_LINES_COUNT_OF_DEFINITION.
    DATA range                   LIKE LINE OF i_ris_include->ranges.
    DATA range_lines_new         TYPE i.

    CHECK i_ris_include IS NOT INITIAL.

    LOOP AT i_ris_include->ranges INTO range WHERE kind = if_ris_occurrence_finder=>occurrence_kind-def . "#EC CI_SORTSEQ
      FIND ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN range-new_content MATCH COUNT range_lines_new.
      r_row = r_row + range-end_row - range-start_row  + range_lines_new.
    ENDLOOP.
  ENDMETHOD.


  METHOD GET_METHOD_DESCRIPTION.

    r_method_description = get_method_from_user_content( i_evaluation_result = i_evaluation_result ).

    r_method_description->determine_call_type( ).

  ENDMETHOD.


  METHOD GET_METHOD_FROM_USER_CONTENT.

    DATA: user_content   TYPE string,
          x_user_content TYPE xstring,
          adt_method     TYPE if_rfac_adt_res_definitions=>ts_extract_method_refactoring.

    user_content = i_evaluation_result->get_user_content( ).

    x_user_content = cl_abap_codepage=>convert_to( user_content ).

    CALL TRANSFORMATION rfac_st_adt_extractmethod
       SOURCE XML x_user_content
      RESULT root = adt_method.

    "selection = adt_2_selection( adt_method-generic-adt_object_uri ).

    r_method_description = cl_rfac_method_description=>create_from_adt( adt_method ).

  ENDMETHOD.


  METHOD GET_PST_NODE4METHOD.
    r_result = i_root_node->find_node_by_name( i_name = i_clif_name i_kind = ce_pst_kind=>interface ).
    IF r_result IS NOT BOUND AND i_method_name NS '~'.
      r_result = i_root_node->find_node_by_name( i_name = i_clif_name i_kind = ce_pst_kind=>class ).
    ENDIF.
    IF r_result IS NOT BOUND.
      r_result = cl_pst_builder=>get_pst_node_4clif( i_name = i_clif_name ).
    ENDIF.
  ENDMETHOD.


  METHOD GET_START_ROW_OF_IMPLEMETATION.

    DATA range                   LIKE LINE OF i_ris_include->ranges.

    CHECK i_ris_include IS NOT INITIAL.
    READ TABLE i_ris_include->ranges INTO range INDEX 1.
    r_row = range-start_row.
    LOOP AT i_ris_include->ranges INTO range WHERE kind = if_ris_occurrence_finder=>occurrence_kind-imp . "#EC CI_SORTSEQ
      IF range-start_row < r_row.
        r_row = range-start_row.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD IF_ART_BLACKBOARD_CONTRIBUTOR~CONTRIBUTE.

    DATA: program_node       TYPE REF TO cl_pst_node,
          pst_method_imp     TYPE REF TO cl_pst_method_implementation,
          surrounding_class  TYPE REF TO cl_pst_class,
          class4new_method   TYPE REF TO cl_pst_class,
          intf4new_method    TYPE REF TO cl_pst_interface,
          pst_method_def     TYPE REF TO cl_pst_method_definition,
          method_name        TYPE string,
          clif_name          TYPE string,
          create_method_call TYPE REF TO lcl_method_call,
          pst_node4method    TYPE REF TO cl_pst_node,
          pst_alias          TYPE REF TO cl_pst_alias.

    CHECK if_art_blackboard_contributor~is_applicable( ) = abap_true.

    surrounding_class = blackboard->get_surrounding_class( ).
    IF surrounding_class IS BOUND.
      me->class_name = surrounding_class->name. "store for later comparison when a method is called on another class
    ENDIF.
    program_node = blackboard->get_scan_result( )->root_pst_node.

    pst_method_imp = blackboard->get_surrounding_method( ).
    IF pst_method_imp IS BOUND.
      IF pst_method_imp->is_position_in_method_stmnt( blackboard->get_pst_position4selection( ) ) = abap_true.
        "we are in the start statement of a method implementation and the class is complete
        IF pst_method_imp->does_definition_exist( ) = abap_false AND surrounding_class IS BOUND.
          add_eval_for_create_method_def( i_pst_class = surrounding_class i_pst_method_imp = pst_method_imp ).
        ENDIF.
        RETURN.
      ENDIF.
    ENDIF.

    "we must be on a call method statement since we did not return earlier
    create_method_call = build_create_method_call( ).
    method_name = create_method_call->get_method_name( ).
    clif_name   = create_method_call->get_clif_name( ).

    IF method_name IS INITIAL OR clif_name IS INITIAL.
      RETURN.
    ENDIF.

    pst_node4method = get_pst_node4method( i_clif_name = clif_name i_method_name = method_name i_root_node = program_node ).
    CHECK pst_node4method IS BOUND.

    IF pst_node4method->kind->is_kind_of( ce_pst_kind=>interface ) = abap_true.
      IF surrounding_class IS NOT BOUND OR surrounding_class->is_interface_implemented( clif_name ) = abap_false.
        IF method_name CS '~'.
          method_name = substring_after( val = method_name sub = '~' ).
        ENDIF.
        intf4new_method = cl_pst_interface=>create( i_node = pst_node4method ).
        pst_method_def = intf4new_method->get_own_method_node( i_name = method_name ).
        pst_alias = intf4new_method->find_own_alias( i_name = method_name ).
        IF pst_method_def IS NOT BOUND AND pst_alias IS NOT BOUND.
          add_eval_create_def_intf_meth( i_intfs4new_method = intf4new_method i_method_name = method_name i_is_static = create_method_call->is_static(  ) ).
        ENDIF.
        RETURN.
      ELSE.
        class4new_method = surrounding_class.
        IF method_name NS '~'.
          method_name = clif_name && '~' && method_name.
        ENDIF.
      ENDIF.
    ENDIF.
    IF class4new_method IS NOT BOUND.
      IF pst_node4method->kind->is_kind_of( ce_pst_kind=>class ) = abap_true.
        class4new_method = cl_pst_class=>create( i_node = pst_node4method ).
      ENDIF.
    ENDIF.
    pst_method_def = class4new_method->get_method_definition( method_name ).
    IF pst_method_def IS BOUND.
      IF pst_method_def->is_abstract( ) = abap_false AND
       class4new_method->implements_method( method_name ) = abap_false.
        "add_eval_for_create_method_imp( i_surrounding_class = class4new_method i_method_def = pst_method_def ).
      ENDIF.
    ELSEIF class4new_method->implements_method( method_name ) = abap_true.

      IF method_name NS '~'.
        add_eval_for_create_method_def( i_pst_class = class4new_method i_pst_method_imp = class4new_method->get_method_implementation( method_name ) ).
      ENDIF.

    ELSE.
      IF class4new_method->find_member( i_member_name = method_name ) IS INITIAL.
        " if there is a member with the same name already in the hierachy we should not offer the quickfix
        IF surrounding_class IS NOT BOUND OR ( surrounding_class IS BOUND AND pst_method_imp IS BOUND ).
          "if we are in a class we have to be in a method implementation as well for create method from call
          add_eval_for_create_from_call( i_create_method_call_object = create_method_call i_method_name = method_name i_class4new_method = class4new_method ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD IF_ART_BLACKBOARD_CONTRIBUTOR~IS_APPLICABLE.
    CHECK boolc( blackboard->get_scan_result( ) IS BOUND ) = abap_true.
    r_result = boolc( blackboard->get_selection_kind( ) = ce_art_selection_kind=>method ).
  ENDMETHOD.


  METHOD IF_QFIX_QUICKFIX~APPLY.
    DATA: inserter                    TYPE REF TO cl_rfac_method_inserter,
          ris_def_include             TYPE REF TO if_ris_include,
          ris_imp_include             TYPE REF TO if_ris_include,
          ris_call_include            TYPE REF TO if_ris_include,
          proposed_method_description LIKE me->method_description,
          cx                          TYPE REF TO cx_static_check.

    " we analyze the original input like during evaluation
    " so the quickfix->apply( ) can decide whether the signature was changed in the user dialog
    " which leads to replacement of the method call else the method call in editor stays untouched
    me->if_art_blackboard_contributor~contribute( ).

    TRY.
        IF method_description IS BOUND.
          method_description->data-name = to_lower( method_description->data-name ).
        ENDIF.
        " method description is prefilled from input analysis
        " so it is possible to find out if user changed signature in the method creation wizard
        proposed_method_description = method_description.
        IF proposed_method_description IS BOUND.
          proposed_method_description->determine_call_type( ).
        ENDIF.
        method_description = get_method_description( evaluation_result ).
        method_description->data-name = to_lower( method_description->data-name ).

        inserter = CL_RFAC_METHOD_INSERTER=>create( me->blackboard ).

        ris_def_include = create_ris_include( method_description->data-definition_include ).
        IF ris_def_include IS BOUND.
          APPEND ris_def_include TO me->affected_includes.

          ris_def_include->add_range( inserter->derive_range4method_definition( method_description ) ).

        ENDIF.


        ris_imp_include = create_ris_include( method_description->data-implemention_include ).
        IF ris_imp_include IS BOUND.
          APPEND ris_imp_include TO me->affected_includes.

          ris_imp_include->add_range( inserter->derive_range4method_imp( method_description ) ).

        ELSE.

          IF proposed_method_description IS BOUND AND proposed_method_description->data-name <> method_description->data-name.
            " name was changed by user in wizard -> adapt method implementation
            DATA: class_imp  TYPE REF TO cl_pst_node,
                  method_imp TYPE REF TO cl_pst_node,
                  pst_class  TYPE REF TO cl_pst_class,
                  selection  TYPE REF TO cl_quickfix_source_object.

            class_imp = me->blackboard->get_scan_result( )->root_pst_node->find_node_by_name( i_name = proposed_method_description->data-classname i_kind = ce_pst_kind=>class_imp ).
            pst_class = cl_pst_class=>create( class_imp ).
            method_imp = pst_class->get_method_implementation( proposed_method_description->data-name )->method_imp_node.

            proposed_method_description->data-implemention_include = method_imp->source_position->include.
            ris_imp_include = create_ris_include( proposed_method_description->data-implemention_include ).
            APPEND ris_imp_include TO me->affected_includes.
            ris_imp_include->add_range( derive_range_from_method_imp( i_node = method_imp i_old_method_name = proposed_method_description->data-name i_new_method_name = method_description->data-name ) ).

          ENDIF.

        ENDIF.

        ris_call_include = create_ris_include( method_description->data-call_include ).
        IF ris_call_include IS BOUND.
          APPEND ris_call_include TO me->affected_includes.
          "if me->method_descr_data_is_similar( i_proposed_data  = proposed_method_description->data i_data_from_user = method_description->data ) = abap_false.
          IF proposed_method_description->is_similar( method_description ) = abap_false.
            ris_call_include->add_range( inserter->derive_range4method_call( method_description ) ).
          ENDIF.
        ENDIF.


        result = me->build_quickfix_result( ).

        " set selection
        selection = build_cursor_selection( i_ris_def_include  = ris_def_include
                                            i_ris_impl_include = ris_imp_include ).

        result->set_source_selection( selection ).

      CATCH cx_static_check INTO cx.
        RAISE EXCEPTION TYPE cx_quickfix_error
          EXPORTING
            textid   = cx_quickfix_error=>create_textid_from_exc_text( cx )
            previous = cx.
    ENDTRY.

  ENDMETHOD.


  METHOD TRANSF_METHOD_DESC_TO_USR_CONT.

    DATA adt_method TYPE if_rfac_adt_res_definitions=>ts_extract_method_refactoring.
    DATA x_result TYPE xstring.

    adt_method = i_method_description->to_adt( ).

    CALL TRANSFORMATION rfac_st_adt_extractmethod
      SOURCE root = adt_method
       RESULT XML x_result.

    r_result = cl_abap_codepage=>convert_from( x_result ).

  ENDMETHOD.
ENDCLASS.
