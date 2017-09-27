class ZTH_ART_CONTRIBUTOR definition
  public
  inheriting from CL_AUNIT_ASSERT
  create public
  for testing
  duration short
  risk level harmless .

public section.

  types:
    BEGIN OF ty_exp_delta,
             line_number            TYPE i,
             include                TYPE program,
             source_position_string TYPE string,
             new_content            TYPE string,
           END OF ty_exp_delta .

  constants NEWLINE like CL_ABAP_CHAR_UTILITIES=>NEWLINE value CL_ABAP_CHAR_UTILITIES=>NEWLINE ##NO_TEXT.
  class-data:
    BEGIN OF workbench_object_types READ-ONLY,
                  report                    TYPE wbobjtype,
                  class                     TYPE wbobjtype,
                  interface                 TYPE wbobjtype,
                  include_in_global_class   TYPE wbobjtype,
                  function_module           TYPE wbobjtype,
                  include_in_function_group TYPE wbobjtype,
                END OF workbench_object_types .
  data ACT_DELTAS type IF_QUICKFIX_SOURCE_OBJECT=>TAB .
  data BLACKBOARD type ref to CL_ART_BLACKBOARD .
  data SOURCE_CODE type STRING_TABLE .
  data INCLUDE_TO_REGISTER type PROGRAMM .
  data:
    exp_quickfix_deltas TYPE STANDARD TABLE OF ty_exp_delta WITH DEFAULT KEY .
  data ACT_EDIT_POSITIONS type IF_QUICKFIX_SOURCE_OBJECT=>TAB .

  class-methods CLASS_CONSTRUCTOR .
  methods CREATE_SOURCE_OBJECT_DOUBLE
    importing
      !I_SOURCE_CODE type STRING_TABLE optional
      !I_SELECTION type STRING default '1,1'
      !I_INCLUDE type PROGRAMM default 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE'
      !I_TROBJTYPE type TROBJTYPE default 'PROG'
      !I_WBOBJTYPE type WBOBJTYPE optional
      !I_MAIN_PROGRAM type PROGRAMM optional
    returning
      value(R_DOUBLE) type ref to IF_QUICKFIX_SOURCE_OBJECT .
  methods CREATE_STRUCTURE_OBJECT_DOUBLE
    importing
      !I_SOURCE_CODE type STRING_TABLE
      !I_SELECTION type STRING default '1,1'
    returning
      value(R_DOUBLE) type ref to IF_QUICKFIX_SOURCE_OBJECT .
  methods APPLY_QUICKFIX
    importing
      !I_QFIX_TYPE type ref to CE_ART_QFIX
      !I_SELECTION type STRING
      !I_USER_CONTENT type STRING optional
      !I_INCLUDE type PROGRAMM default 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE'
      !I_WBOBJTYPE type WBOBJTYPE optional
      !I_TROBJTYPE type TROBJTYPE default 'PROG'
      !I_MAIN_PROGRAM type PROGRAMM optional
    returning
      value(R_RESULT) type ref to IF_QUICKFIX_RESULT
    raising
      CX_QUICKFIX_ERROR .
  methods VERIFY_DELTA
    importing
      !INDEX type I
      !POS_STRING type STRING
      !EXP_CODE_SNIPPET type STRING .
  methods ADD_SAMPLE_CODE
    importing
      !I_LINE_NUMBER type I
      !I_CODE_BEFORE type CSEQUENCE
      !I_CODE_CHANGE type CSEQUENCE optional .
  methods VERIFY_DELTAS .
  methods ASSERT_APPLICABLE
    importing
      !I_CONTRIBUTOR type ref to IF_ART_BLACKBOARD_CONTRIBUTOR .
  methods ASSERT_NOT_APPLICABLE
    importing
      !I_CONTRIBUTOR type ref to IF_ART_BLACKBOARD_CONTRIBUTOR .
  methods ASSERT_IDENTIFIER
    importing
      !I_IDENTIFIER type CSEQUENCE .
  methods ASSERT_SELECTION_KIND
    importing
      !I_SELECTION_KIND type ref to CE_ART_SELECTION_KIND .
      "! Verifies if a certain proposal has been written to the blackboard.
  methods ASSERT_PROPOSED
    importing
      !INDEX type I default 0
      !QFIX type ref to CE_ART_QFIX
      !USER_CONTENT type STRING optional
    returning
      value(R_RESULT) type ref to CL_ART_PROPOSAL .
      "! Verifies if a certain proposal has <strong>not</strong> been written to the blackboard.
  methods ASSERT_NOT_PROPOSED
    importing
      !QFIX type ref to CE_ART_QFIX .
      "! Verifies if a certain number of proposals has been written to the blackboard.
  methods ASSERT_NUMBER_OF_PROPOSALS
    importing
      !EXP type I .
  methods ASSERT_POSITION
    importing
      !ACT type ref to CL_PST_SOURCE_POSITION
      !EXP type CSEQUENCE .
  methods PREPARE_BLACKBOARD
    importing
      !I_SELECTION type STRING
      !I_WBOBJTYPE type WBOBJTYPE optional
      !I_INCLUDE type PROGRAM default 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE'
      !I_MAIN_PROGRAM type PROGRAM default 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE'
      !I_SCAN_RESULT_ONLY type ABAP_BOOL default ABAP_FALSE .
  methods PREP_BLACKBOARD_BY_SOURCE_OBJ
    importing
      !I_SOURCE_OBJECT type ref to IF_QUICKFIX_SOURCE_OBJECT
      !I_SCAN_RESULT_ONLY type ABAP_BOOL default ABAP_FALSE .
  methods EXERCISE_SCAN_RESULT_CONTRIB
    importing
      !I_SELECTION type STRING
      !I_INCLUDE type PROGRAMM default 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE'
      !I_MAIN_PROG type PROGRAMM optional .
  methods CLEAR_BUILDER_INSTANCES .
  methods REPLACE_LINEBREAKS
    changing
      !C_CONTENT type STRING .
  methods ASSERT_SELECTION_CONTENT
    importing
      !I_SELECTION_CONTENT type STRING .
  methods VERIFY_EDIT_POSITIONS
    importing
      !EXP_POSITION_STRING type STRING .
protected section.

  data SOURCE_REPOSITORY type ref to ZCL_ART_SOURCE_REPOSITORY .
  data PROPOSALS type CL_ART_PROPOSAL=>TAB .
  data SEPARATOR4DELTAS type STRING value ';' ##NO_TEXT.

  methods CREATE_QUICKFIX
    importing
      !I_PROPOSAL type ref to CL_ART_PROPOSAL
      !I_SOURCE_OBJECT type ref to IF_QUICKFIX_SOURCE_OBJECT
    returning
      value(R_RESULT) type ref to IF_QFIX_QUICKFIX .
  methods REGISTER_SOURCE
    importing
      !I_CLEAR_SOURCE_TAB type ABAP_BOOL optional .
    "! central switch to disable unit tests in lower release e.g.: due to language issues
  methods ABORT_IF_WRONG_RELEASE
    importing
      !MSG type STRING default 'test for unsupported feature in this release'
      !QUIT type AUNIT_FLOWCTRL default IF_AUNIT_CONSTANTS=>METHOD
    preferred parameter MSG  ##NO_TEXT.
  methods SETUP_FRESH_SOURCE_REPOSITORY
    importing
      !I_FLG_GRAIN_METHODS type ABAP_BOOL .
  methods SET_INCLUDE_TO_REGISTER
    importing
      !I_INCLUDE type PROGRAMM .
  methods ABORT_IF_KERNEL_IS_LOWER_THAN
    importing
      !I_KERNEL_RELEASE type CSEQUENCE
      !I_PATCH_LEVEL type CSEQUENCE .
  methods ABORT_IF_KERNEL_LOWER_742SP17 .
  methods ABORT_IF_KERNEL_LOWER_745SP0 .
  PRIVATE SECTION.

    METHODS:
      setup,
      teardown.

    TYPES:
      ty_change_command TYPE LINE OF string_table,
      ty_remove         TYPE LINE OF string_table.
    DATA replace_start_position TYPE REF TO cl_pst_source_position.

    METHODS create_expected_deltas
      IMPORTING
        i_code_change TYPE csequence
        i_code_before TYPE csequence
        i_line_number TYPE i.
    METHODS get_start_column
      IMPORTING
        i_current_line  TYPE string
      RETURNING
        VALUE(r_result) TYPE i.
    METHODS derive_wbobjtype_by_include
      IMPORTING
        i_include       TYPE programm
      RETURNING
        VALUE(r_result) TYPE wbobjtype.


ENDCLASS.



CLASS ZTH_ART_CONTRIBUTOR IMPLEMENTATION.


  METHOD ABORT_IF_KERNEL_IS_LOWER_THAN.

    CONSTANTS: BEGIN OF co_index,
                 operating_system         TYPE i VALUE 2,
                 operating_system_version TYPE i VALUE 19,
                 machine_type             TYPE i VALUE 3,
                 node_name                TYPE i VALUE 4,
                 sap_system_id            TYPE i VALUE 5,
                 ip_address               TYPE i VALUE 11,
                 database_library         TYPE i VALUE 13,
                 kernel_release           TYPE i VALUE 12,
                 kernel_patch_level       TYPE i VALUE 15,
                 kernel_compilation_date  TYPE i VALUE 14,
                 kernel_kind              TYPE i VALUE 22,
               END OF co_index.

    TYPES: BEGIN OF ty_s_kernel_information,
             id      TYPE c LENGTH 21,
             content TYPE c LENGTH 666,
           END OF ty_s_kernel_information.

    TYPES ty_t_kernel_information TYPE STANDARD TABLE OF ty_s_kernel_information WITH NON-UNIQUE DEFAULT KEY.
    DATA: kernel_information_table TYPE ty_t_kernel_information,
          release type char10,
          patchlevel type char10.

    CALL 'SAPCORE' ID 'ID' FIELD 'VERSION' ID 'TABLE' FIELD kernel_information_table.

    release = kernel_information_table[ co_index-kernel_release ]-content.
    patchlevel = kernel_information_table[ co_index-kernel_patch_level ]-content.

    if i_kernel_release is initial or release is initial or release < i_kernel_release.
      abort( msg = |required kernel release { i_kernel_release } but was { release }| quit = if_aunit_constants=>method ).
    elseif i_kernel_release = release.
      if i_patch_level is initial or patchlevel is initial or patchlevel < i_patch_level.
        abort( msg = |required kernel patch level { i_patch_level } but was { patchlevel }| quit = if_aunit_constants=>method ).
      endif.
    endif.

  ENDMETHOD.


  method ABORT_IF_KERNEL_LOWER_742SP17.

    abort_if_kernel_is_lower_than( i_kernel_release = '742'  i_patch_level = '17' ).

  endmethod.


  method ABORT_IF_KERNEL_LOWER_745SP0.

    abort_if_kernel_is_lower_than( i_kernel_release = '745'  i_patch_level = '0' ).

  endmethod.


  METHOD ABORT_IF_WRONG_RELEASE.

    IF cl_art_softswitch=>is_inline_decl_supported( ) = abap_false.
      abort( msg = msg quit = quit ).
    ENDIF.

  ENDMETHOD.


  METHOD ADD_SAMPLE_CODE.

    IF i_line_number > lines(  source_code ) + 1.
      fail( |Invalid line number { i_line_number }!| ).
    ELSEIF i_line_number <= lines(  source_code ).
      fail( |Line { i_line_number } already exists!| ).
    ENDIF.

    APPEND i_code_before TO source_code.

    create_expected_deltas(
      i_code_change = i_code_change
      i_code_before = i_code_before
      i_line_number = i_line_number ).

  ENDMETHOD.


  METHOD APPLY_QUICKFIX.

    DATA: quickfix      TYPE REF TO if_qfix_quickfix,
          source_object TYPE REF TO if_quickfix_source_object,
          proposal      TYPE REF TO cl_art_proposal.

    source_object = create_source_object_double(
      i_include = i_include i_source_code = source_code
      i_selection = i_selection i_wbobjtype = i_wbobjtype
      i_trobjtype = i_trobjtype i_main_program = i_main_program ).

    proposal = i_qfix_type->create_proposal( ).
    proposal->set_user_content(  i_user_content ).

    quickfix = create_quickfix( i_proposal = proposal i_source_object = source_object ).

    r_result = quickfix->apply(
      input             = source_object
      evaluation_result = proposal ).

    act_deltas = r_result->get_source_deltas( ).
    act_edit_positions = r_result->get_edit_positions( ).

  ENDMETHOD.


  METHOD ASSERT_APPLICABLE.
    cl_abap_unit_assert=>assert_true( i_contributor->is_applicable( ) ).
  ENDMETHOD.


  METHOD ASSERT_IDENTIFIER.
    cl_abap_unit_assert=>assert_equals( exp = to_lower( i_identifier ) act = blackboard->get_identifier_name( ) ).
  ENDMETHOD.


  METHOD ASSERT_NOT_APPLICABLE.
    cl_abap_unit_assert=>assert_false( i_contributor->is_applicable( ) ).
  ENDMETHOD.


  METHOD ASSERT_NOT_PROPOSED.

    DATA: proposal  TYPE REF TO cl_art_proposal.

    if proposals is initial.
      proposals = blackboard->get_proposals( ).
    endif.

    LOOP AT proposals INTO proposal.
      IF proposal->qfix = qfix.
        fail( |{ qfix->type } proposed, but not expected!| ).
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD ASSERT_NUMBER_OF_PROPOSALS.
    assert_equals( exp = exp act = lines( blackboard->get_proposals( ) ) ).
  ENDMETHOD.


  METHOD ASSERT_POSITION.
    assert_equals(  act = act->to_string( ) exp = exp ).
  ENDMETHOD.


  METHOD ASSERT_PROPOSED.

    DATA: proposal           TYPE REF TO cl_art_proposal,
          found              TYPE abap_bool VALUE abap_false,
          found_user_content TYPE abap_bool VALUE abap_false.

    if proposals is initial.
      proposals = blackboard->get_proposals( ).
    endif.

    IF index > 0.

      READ TABLE proposals INDEX index INTO proposal.
      assert_subrc( act = sy-subrc msg = |Proposal { qfix->type } not found at index { index }!| ).
      cl_abap_unit_assert=>assert_equals( exp = qfix act = proposal->qfix msg = |Proposal at index { index } is of type { proposal->qfix->type } but should be of type { qfix->type }!| ).
      IF user_content IS SUPPLIED.
        cl_abap_unit_assert=>assert_equals( exp = user_content act = proposal->if_quickfix_evaluation_result~get_user_content( )
                                            msg = |Proposal at index { index } user content is { proposal->if_quickfix_evaluation_result~get_user_content( ) } but should be { user_content }.| ).
      ENDIF.

    ELSE.

      IF user_content IS SUPPLIED.

        LOOP AT proposals INTO proposal.
          IF proposal->qfix = qfix.
            found = abap_true.
            r_result = proposal.
            IF to_lower( proposal->if_quickfix_evaluation_result~get_user_content( ) ) = to_lower( user_content ).
              found_user_content = abap_true.
              r_result = proposal.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF found = abap_true.
          cl_abap_unit_assert=>assert_true( act = found_user_content msg = |Proposal has different user-content: Expected [{ user_content }] Actual [{ r_result->user_content }]| ).
        ELSE.
          fail( |Proposal not found: { qfix->type }| ).
        ENDIF.

      ELSE.
        READ TABLE proposals WITH KEY table_line->qfix = qfix INTO r_result.
        IF sy-subrc <> 0.
          fail( |Proposal not found: { qfix->type }| ).
        ENDIF.
      ENDIF.

    ENDIF.

    IF r_result IS INITIAL.
      r_result = proposal.
    ENDIF.

  ENDMETHOD.


  METHOD ASSERT_SELECTION_CONTENT.
    cl_abap_unit_assert=>assert_equals( exp = i_selection_content act = blackboard->get_selection_content( ) ).
  ENDMETHOD.


  METHOD ASSERT_SELECTION_KIND.
    cl_abap_unit_assert=>assert_equals( exp = i_selection_kind act = blackboard->get_selection_kind( ) ).
  ENDMETHOD.


  METHOD CLASS_CONSTRUCTOR.

    zTH_ART_CONTRIBUTOR=>workbench_object_types-report-objtype_tr = 'PROG'.
    zTH_ART_CONTRIBUTOR=>workbench_object_types-report-subtype_wb = swbm_c_type_prg_source.
    zTH_ART_CONTRIBUTOR=>workbench_object_types-class-objtype_tr = 'CLAS'.
    zTH_ART_CONTRIBUTOR=>workbench_object_types-class-subtype_wb = swbm_c_type_class.
    zTH_ART_CONTRIBUTOR=>workbench_object_types-interface-objtype_tr = 'INTF'.
    zTH_ART_CONTRIBUTOR=>workbench_object_types-interface-subtype_wb = swbm_c_type_interface.
    zTH_ART_CONTRIBUTOR=>workbench_object_types-include_in_global_class-objtype_tr = 'CLAS'.
    zTH_ART_CONTRIBUTOR=>workbench_object_types-include_in_global_class-subtype_wb = swbm_c_type_cls_include.
    zTH_ART_CONTRIBUTOR=>workbench_object_types-function_module-objtype_tr = 'FUGR'.
    zTH_ART_CONTRIBUTOR=>workbench_object_types-function_module-subtype_wb = swbm_c_type_function.
    zTH_ART_CONTRIBUTOR=>workbench_object_types-include_in_function_group-objtype_tr = 'FUGR'.
    zTH_ART_CONTRIBUTOR=>workbench_object_types-include_in_function_group-subtype_wb = swbm_c_type_prg_include.


  ENDMETHOD.


  METHOD CLEAR_BUILDER_INSTANCES.
    cl_pst_builder=>clear_builder_instances( ).
  ENDMETHOD.


  METHOD CREATE_EXPECTED_DELTAS.

    DATA from TYPE string.
    DATA to TYPE string.
    DATA exp_delta TYPE zTH_ART_CONTRIBUTOR=>ty_exp_delta.
    DATA change_commands TYPE string_table.
    DATA offset TYPE i.
    DATA change_command TYPE ty_change_command.
    DATA remove TYPE ty_remove.
    DATA lines TYPE i.
    DATA: source_position TYPE REF TO cl_pst_source_position.

    IF i_code_change IS INITIAL.
      RETURN.
    ENDIF.

    SPLIT i_code_change AT separator4deltas INTO TABLE change_commands.

    LOOP AT change_commands INTO change_command.

      exp_delta-line_number = i_line_number.

      IF contains( val = change_command start = 'append:' ).

        source_position = cl_pst_source_position=>create(
          i_start_row = i_line_number
          i_start_col = strlen( i_code_before )
          i_include = include_to_register ).

        exp_delta-new_content = change_command+7.
        replace_linebreaks( CHANGING c_content = exp_delta-new_content ).

      ELSEIF contains( val = change_command start = 'prepend:' ).

        source_position = cl_pst_source_position=>create(
          i_start_row = i_line_number
          i_start_col = get_start_column( i_code_before )
          i_include = include_to_register ).

        exp_delta-new_content = change_command+8.
        replace_linebreaks( CHANGING c_content = exp_delta-new_content ).

      ELSEIF contains( val = change_command start = 'replace_starts_before:' ).

        assert_not_bound( replace_start_position ).

        replace_start_position = cl_pst_source_position=>create(
          i_start_row = i_line_number
          i_start_col = get_start_column( i_code_before )
          i_include = include_to_register ).

      ELSEIF contains( val = change_command start = 'replace_ends_before:' ).

        assert_bound( replace_start_position ).

        source_position = cl_pst_source_position=>create(
          i_start_row = replace_start_position->range-start-row
          i_start_col = replace_start_position->range-start-col
          i_end_row   = i_line_number
          i_end_col   = get_start_column( i_code_before )
          i_include = include_to_register ).

        exp_delta-new_content = change_command+20.
        replace_linebreaks( CHANGING c_content = exp_delta-new_content ).

        exp_delta-line_number = replace_start_position->range-start-row. "!!!

        CLEAR replace_start_position.

      ELSEIF contains( val = change_command start = 'replace_ends_after:' ).

        assert_bound( replace_start_position ).

        source_position = cl_pst_source_position=>create(
          i_start_row = replace_start_position->range-start-row
          i_start_col = replace_start_position->range-start-col
          i_end_row   = i_line_number
          i_end_col   = strlen( i_code_before )
          i_include = include_to_register ).

        exp_delta-new_content = change_command+19.
        replace_linebreaks( CHANGING c_content = exp_delta-new_content ).

        exp_delta-line_number = replace_start_position->range-start-row. "!!!

        CLEAR replace_start_position.

      ELSEIF contains( val = change_command start = 'replace:' ).

        SPLIT change_command+8 AT ' ~> ' INTO from to.

        offset = find( val = i_code_before sub = from ).

        source_position = cl_pst_source_position=>create(
          i_start_row = i_line_number
          i_start_col = offset
          i_end_row   = i_line_number
          i_end_col   = offset + strlen( from )
          i_include = include_to_register ).

        replace_linebreaks( CHANGING c_content = to ).
        exp_delta-new_content = to.

      ELSEIF contains( val = change_command start = 'remove:' ).

        remove = change_command+7.

        DATA: removes               TYPE string_table,
              first_remove          TYPE string,
              last_line_for_removal TYPE string,
              end_col               TYPE i.
        SPLIT remove AT '#' INTO TABLE removes.

        READ TABLE removes INDEX 1 INTO first_remove.
        offset = find( val = i_code_before sub = first_remove ).

        lines = lines( removes ).
        IF lines = 1.
          end_col = offset + strlen( first_remove ).
        ELSE.
          READ TABLE removes INDEX lines( removes ) INTO last_line_for_removal.
          end_col = cond #( when last_line_for_removal = ` ` then 0 else strlen( last_line_for_removal ) ).
        ENDIF.

        source_position = cl_pst_source_position=>create(
          i_start_row = i_line_number
          i_start_col = offset
          i_end_row   = i_line_number + lines( removes ) - 1
          i_end_col   = end_col
          i_include = include_to_register ).

        exp_delta-new_content = ''.

      ELSEIF contains( val = change_command start = 'insert:' ).

        DATA col TYPE i.
        SPLIT change_command+7 AT ' ' INTO from  to.
        col = from.
        source_position = cl_pst_source_position=>create(
          i_start_row = i_line_number
          i_start_col = col
          i_include = include_to_register ).

        exp_delta-new_content = to.
        replace_linebreaks( CHANGING c_content = exp_delta-new_content ).

      ELSE.
        fail( |Unknown change command!| ).
      ENDIF.

      IF source_position IS BOUND.

        exp_delta-source_position_string = source_position->to_string( ).
        exp_delta-include = source_position->include.
        exp_delta-new_content = to_lower( exp_delta-new_content ).

        APPEND exp_delta TO exp_quickfix_deltas.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD CREATE_QUICKFIX.
    r_result = cl_art_quickfix_provider=>create( )->create_quickfix( i_proposal = i_proposal i_source_object = i_source_object ).
  ENDMETHOD.


  METHOD CREATE_SOURCE_OBJECT_DOUBLE.
    DATA: source_from_db TYPE string_table.
    IF i_source_code IS NOT INITIAL.
      source_repository->register_source( i_include = i_include i_source = i_source_code
                                          i_cursor_position = cl_pst_source_position=>create_from_string( i_selection ) ).
    ELSE.
      source_from_db = source_repository->read_source_from_db( i_include = i_include ).
      source_repository->register_source( i_include = i_include i_source = source_from_db
                                          i_cursor_position = cl_pst_source_position=>create_from_string( i_selection ) ).

    ENDIF.
    r_double = ltd_source_object=>create( i_selection = i_selection
                                          i_include = i_include i_trobjtype = i_trobjtype
                                          i_wbobjtype = i_wbobjtype i_main_program = i_main_program ).
  ENDMETHOD.


   METHOD CREATE_STRUCTURE_OBJECT_DOUBLE.

    r_double = ltd_structure_source_object=>create( i_selection = i_selection i_source = i_source_code ).

  ENDMETHOD.


  METHOD DERIVE_WBOBJTYPE_BY_INCLUDE.

    IF strlen( i_include ) = 32 AND i_include+30(2) = 'CS'.
      r_result = workbench_object_types-class.
    ELSEIF strlen( i_include ) >= 34 AND i_include+30(2) = 'CC'.
      r_result = workbench_object_types-include_in_global_class.
    ELSEIF cl_art_include_services=>include_belongs_to_function( i_include ).
      r_result = workbench_object_types-function_module.
    ELSEIF cl_art_include_services=>include_belongs_to_fugr( i_include ).
      r_result = workbench_object_types-include_in_function_group.
    ELSE.
      r_result = workbench_object_types-report.
    ENDIF.


  ENDMETHOD.


  METHOD EXERCISE_SCAN_RESULT_CONTRIB.

    DATA: contributor   TYPE REF TO if_art_blackboard_contributor,
          source_object TYPE REF TO if_quickfix_source_object,
          main_prog     TYPE programm.

    IF i_main_prog IS SUPPLIED.
      main_prog = i_main_prog.
    ELSE.
      main_prog = cl_art_include_services=>derive_mainprog_by_include( i_include ).
    ENDIF.

    source_object = create_source_object_double( i_include = i_include i_source_code = source_code
                                                 i_selection = i_selection i_wbobjtype = derive_wbobjtype_by_include( i_include )
                                                 i_main_program = main_prog ).
    IF blackboard IS NOT BOUND.
      blackboard = cl_art_blackboard=>create( ).
    ENDIF.
    blackboard->set_source_object( source_object ).
    contributor = cl_art_contrib_scan_result=>create( blackboard ).

    contributor->contribute( ).

  ENDMETHOD.


  METHOD GET_START_COLUMN.
    DATA: shift TYPE string,
          line  TYPE string.
    shift = | |.
    r_result = 0.
    CHECK i_current_line IS NOT INITIAL.
    line = i_current_line.
    WHILE substring( val = line off = 0 len = 1 ) = shift.
      line = shift_left( val = line places = 1 ).
      r_result = r_result + 1.
    ENDWHILE.
  ENDMETHOD.


  METHOD PREPARE_BLACKBOARD.

    "clear proposals before initializing a new blackboard
    clear proposals.

    me->prep_blackboard_by_source_obj( i_scan_result_only = i_scan_result_only
                                       i_source_object = create_source_object_double( i_source_code = source_code
                                                                                      i_selection = i_selection
                                                                                      i_include = i_include
                                                                                      i_wbobjtype = i_wbobjtype
                                                                                      i_main_program = i_main_program ) ).

  ENDMETHOD.


  METHOD PREP_BLACKBOARD_BY_SOURCE_OBJ.

    DATA: scan_result_contributor    TYPE REF TO if_art_blackboard_contributor,
          type_selection_contributor TYPE REF TO if_art_blackboard_contributor.

    blackboard = cl_art_blackboard=>create( ).

    blackboard->set_source_object( i_source_object ).

    scan_result_contributor = cl_art_contrib_scan_result=>create( blackboard ).
    scan_result_contributor->contribute( ).

    IF i_scan_result_only = abap_false.
      type_selection_contributor = cl_art_contrib_selection_type=>create( blackboard ).
      type_selection_contributor->contribute( ).
    ENDIF.

  ENDMETHOD.


  METHOD REGISTER_SOURCE.

    ASSERT include_to_register IS NOT INITIAL.

    source_repository->register_source( i_include = include_to_register i_source = source_code ).

    IF i_clear_source_tab = abap_true.
      CLEAR source_code.
    ENDIF.

  ENDMETHOD.


  METHOD REPLACE_LINEBREAKS.
    REPLACE ALL OCCURRENCES OF '##NO_TEXT' IN c_content WITH '??NO_TEXT'.
    REPLACE ALL OCCURRENCES OF '#(' IN c_content WITH '?('.
    REPLACE ALL OCCURRENCES OF '#' IN c_content WITH newline.
    REPLACE ALL OCCURRENCES OF '??NO_TEXT' IN c_content WITH '##NO_TEXT'.
    REPLACE ALL OCCURRENCES OF '?(' IN c_content WITH '#('.
  ENDMETHOD.


  METHOD SETUP.
    source_repository = zcl_art_source_repository=>create( ).
  ENDMETHOD.


  METHOD SETUP_FRESH_SOURCE_REPOSITORY.
    source_repository = zcl_art_source_repository=>create( i_flg_grain_methods = i_flg_grain_methods ).
  ENDMETHOD.


  METHOD SET_INCLUDE_TO_REGISTER.

    IF include_to_register IS NOT INITIAL AND include_to_register <> i_include.
      source_repository->register_source( i_include = include_to_register i_source = source_code ).
      CLEAR source_code.
    ENDIF.

    include_to_register = i_include.

  ENDMETHOD.


  METHOD TEARDOWN.
    me->clear_builder_instances( ).
    cl_pst_node=>clear_node_buffer( ).
    cl_rfac_scan_result_register=>clear_scan_result_register( ).
    CLEAR source_code.
  ENDMETHOD.


  METHOD VERIFY_DELTA.

    DATA: delta            LIKE LINE OF me->act_deltas,
          act_code_snippet TYPE string.

    READ TABLE me->act_deltas INDEX index INTO delta.

    assert_equals( act = delta->get_source_position( )->to_string( ) exp = pos_string msg = |delta position differs| ).

    delta->get_source_code_as_string( IMPORTING source_code = act_code_snippet ).
    cl_abap_unit_assert=>assert_equals( exp = to_lower( exp_code_snippet ) act = to_lower( act_code_snippet ) msg = |code snippet differs| ).

  ENDMETHOD.


  METHOD VERIFY_DELTAS.

    DATA: act_delta                   TYPE REF TO if_quickfix_source_object,
          act_content                 TYPE string,
          exp_delta                   TYPE zTH_ART_CONTRIBUTOR=>ty_exp_delta,
          act_source_position         TYPE REF TO cl_pst_source_position,
          line_numbers_missing_deltas TYPE string,
          copy_exp_qfix_deltas        TYPE STANDARD TABLE OF zTH_ART_CONTRIBUTOR=>ty_exp_delta.

    assert_equals( act = lines( act_deltas ) exp = lines( exp_quickfix_deltas ) ).

    copy_exp_qfix_deltas = exp_quickfix_deltas.

    LOOP AT act_deltas INTO act_delta.

      act_delta->get_source_code_as_string( IMPORTING source_code = act_content ).
      act_source_position = act_delta->get_source_position( ).

      IF act_source_position IS INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE copy_exp_qfix_deltas WITH KEY line_number = act_source_position->range-start-row INTO exp_delta.
      IF exp_delta IS INITIAL.
        replace_linebreaks( CHANGING c_content = act_content ).
        fail( |delta not in expected deltas at { act_source_position->to_string( ) } with content: { act_content }| ).
      ELSE.
        DELETE TABLE copy_exp_qfix_deltas FROM exp_delta.
      ENDIF.
      IF exp_delta-include IS NOT INITIAL.
        cl_abap_unit_assert=>assert_equals( exp = exp_delta-include act = act_source_position->include ).
      ENDIF.
      assert_equals( act = act_source_position->to_string( ) exp = exp_delta-source_position_string msg = |position differs delta at line { exp_delta-line_number }| ).
      assert_equals( act = to_lower( act_content ) exp = to_lower( exp_delta-new_content ) msg = |content differs delta at line: { exp_delta-line_number }| ).

    ENDLOOP.

    "check all expected deltas were delivered
    IF copy_exp_qfix_deltas IS NOT INITIAL.
      LOOP AT copy_exp_qfix_deltas INTO exp_delta.
        IF exp_delta-source_position_string IS INITIAL.
          CONTINUE.
        ENDIF.
        IF line_numbers_missing_deltas IS INITIAL.
          line_numbers_missing_deltas = |{ exp_delta-line_number }|.
        ELSE.
          line_numbers_missing_deltas = |{ line_numbers_missing_deltas },{ exp_delta-line_number }|.
        ENDIF.
      ENDLOOP.
      IF line_numbers_missing_deltas IS NOT INITIAL.
        fail( |expected deltas at line(s) { line_numbers_missing_deltas } not delivered.  | ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD VERIFY_EDIT_POSITIONS.

    DATA: edit_position LIKE LINE OF act_edit_positions,
          exp_positions TYPE string_table,
          exp_pos       TYPE string.

    SPLIT exp_position_string AT space INTO TABLE exp_positions.

    assert_equals( exp = lines( exp_positions ) act = lines( act_edit_positions ) ).

    LOOP AT exp_positions INTO exp_pos.
      READ TABLE me->act_edit_positions INDEX sy-tabix INTO edit_position.
      assert_equals( exp = exp_pos act = edit_position->get_source_position( )->to_string( ) msg = |Edit position { exp_pos } at index { sy-tabix } not found.| ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
