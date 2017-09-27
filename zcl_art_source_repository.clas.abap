class ZCL_ART_SOURCE_REPOSITORY definition
  public
  final
  create private

  global friends CL_ART_QUICKFIX_PROVIDER
                 TH_ART_CONTRIBUTOR
                 ZTH_ART_CONTRIBUTOR .

public section.

  constants:
    BEGIN OF mode_4_compiler,
                 full_registered_source    TYPE string VALUE 'full_registered_source',
                 grained_registered_source TYPE string VALUE 'grained_registered_source',
                 fake_source               TYPE string VALUE 'fake_source',
               END OF mode_4_compiler .

  methods REGISTER_SOURCE
    importing
      !I_INCLUDE type PROGRAM
      !I_SOURCE type STRING_TABLE optional
      !I_CURSOR_POSITION type ref to CL_PST_SOURCE_POSITION optional
      !I_IS_DIRTY type ABAP_BOOL default ABAP_FALSE .
  methods READ_SOURCE
    importing
      !I_INCLUDE type PROGRAM
      !I_MODE type STRING default MODE_4_COMPILER-GRAINED_REGISTERED_SOURCE
    returning
      value(R_SOURCE) type STRING_TABLE
    raising
      CX_RFAC_DYNAMIC .
      "! get a compiler instance prepared with all registered dirty sources
      "! @parameter i_main_program | the main program which is under investigation
      "! @parameter i_mode         | controls which source is used to populate the compiler
      "! <ul><li>registered source</li>
      "!     <br/>All dirty sources registered in the registry are used to populate the compiler.
      "!          So the compiler relies on these dirty sources instead of their saved version on the database.
      "!          If the registry was created with the flag for graining method implementations, then all code inside
      "!          of methods is replaced by whitespace except the method which contains the cursor or the current selection.
      "!          The main reasons for this are performance gains since less statements have to be scanned and analyzed.
      "!     <li>fake source</li>
      "!     <br/>Same as registered source but for all includes which belong to the main program given in parameter i_main_program fake source is considered.
      "!          Fake source can be registered per include using the method register_fake_source( ).
      "!          This is used for example when the type of a undeclared variable is determined because the original registered source contains syntax errors then.
      "! </ul>
  methods GET_COMPILER_FOR_MAIN_PROGRAM
    importing
      !I_MAIN_PROGRAM type PROGRAM
      !I_MODE type STRING default MODE_4_COMPILER-GRAINED_REGISTERED_SOURCE
    returning
      value(R_RESULT) type ref to CL_ABAP_COMPILER .
      "! get a compiler instance prepared with registered dirty sources (full source not grained) for the main program under investigation.
      "! @parameter i_main_program             | the main program which is under investigation
  methods GET_COMPILER_FOR_WHERE_USED
    importing
      !I_MAIN_PROGRAM type PROGRAM
    returning
      value(R_RESULT) type ref to CL_ABAP_COMPILER .
  methods GET_REGISTERED_STANDALONE_INCS
    importing
      !I_FOCUSED_INCUDE_FOR_EXCLUSION type PROGRAM optional
      !I_MODE type STRING default MODE_4_COMPILER-GRAINED_REGISTERED_SOURCE
    returning
      value(R_RESULT) type SREPTAB .
  methods GET_REGISTERED_FUGR_INCLUDES
    importing
      !I_FUNCTION_POOL type PROGRAM
      !I_MODE type STRING default MODE_4_COMPILER-GRAINED_REGISTERED_SOURCE
    returning
      value(R_RESULT) type SREPTAB .
  methods GET_OO_CLIF_SOURCE_OBJECT
    importing
      !I_CLIF_NAME type SEOCLSNAME
      !I_MODE type STRING
    returning
      value(R_RESULT) type ref to IF_OO_CLIF_SOURCE
    raising
      CX_OO_CLIF_NOT_EXISTS .
      "! Returns the code of the entire method, including
      "! METHOD and ENDMETHOD statement.
  methods GET_METHOD_SOURCE
    importing
      !I_METHOD_NODE type ref to CL_PST_NODE
    returning
      value(R_RESULT) type STRING .
      "! Returns the code of the entire method, <em>without</em>
      "! METHOD and ENDMETHOD statement.
  methods GET_METHOD_BODY
    importing
      !I_METHOD_NODE type ref to CL_PST_NODE
    returning
      value(R_RESULT) type STRING .
      "! Returns the code of a arbitrary source position (selection)
  methods GET_SOURCE_SNIPPET
    importing
      !I_SOURCE_POSITION type ref to CL_PST_SOURCE_POSITION
    returning
      value(R_RESULT) type STRING .
  methods GET_SOURCE_LINE
    importing
      !I_SOURCE_POSITION type ref to CL_PST_SOURCE_POSITION
    returning
      value(R_RESULT) type STRING .
  methods REGISTER_FAKE_SOURCE
    importing
      !I_INCLUDE type PROGRAM
      !I_FAKE_SOURCE type STRING_TABLE .
  methods GET_FUNCTION_SOURCE_CONVERTER
    importing
      !I_FUNC_U_INCLUDE type PROGRAM
    returning
      value(R_RESULT) type ref to CL_FB_FUNC_SOURCE_CONVERTER
    raising
      CX_FB_SCAN_ERROR .
  methods IS_DIRTY_OUTSIDE_MAIN_PROGRAM
    importing
      !I_MAIN_PROGRAM type PROGRAM
    returning
      value(R_IS_DIRTY) type ABAP_BOOL .
  class-methods CLEAR_REGISTER .
      "! Returns the existing temporary singleton instance created via method create( ) before.
      "! The usage of the create( ) method is restricted to the class cl_art_quickfix_provider via friendship.
      "! If create was not called before it is called internally.
  class-methods GET_INSTANCE
    returning
      value(R_RESULT) type ref to ZCL_ART_SOURCE_REPOSITORY .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_source_registry_entry,
        include        TYPE program,
        source         TYPE string_table,
        grained_source TYPE string_table,
        fake_source    TYPE string_table,
        is_dirty       TYPE abap_bool,
      END OF ty_source_registry_entry .
  types:
    ty_source_registry TYPE SORTED TABLE OF ty_source_registry_entry WITH UNIQUE KEY include .
  types:
    BEGIN OF ty_compiler_entry,
        main_program TYPE program,
        compiler_ref TYPE REF TO cl_abap_compiler,
      END OF ty_compiler_entry .
  types:
    BEGIN OF ty_oo_clif_source_entry,
        clif        TYPE string,
        oo_clif_ref TYPE REF TO if_oo_clif_source,
      END OF ty_oo_clif_source_entry .
  types:
    BEGIN OF ty_function_source_entry,
        func_u_include        TYPE program,
        funcname              TYPE rs38l_fnam,
        func_source_converter TYPE REF TO cl_fb_func_source_converter,
        func_source           TYPE REF TO cl_fb_func_source,
      END OF ty_function_source_entry .
  types:
    ty_compiler_buffer TYPE SORTED TABLE OF ty_compiler_entry WITH UNIQUE KEY main_program .
  types:
    ty_oo_clif_buffer  TYPE SORTED TABLE OF ty_oo_clif_source_entry WITH UNIQUE KEY clif .
  types:
    ty_function_buffer TYPE SORTED TABLE OF ty_function_source_entry WITH UNIQUE KEY func_u_include .
  types:
    BEGIN OF ty_u_include_buffer,
             main_prog_fugr             TYPE program,
             substitutions_4_u_includes TYPE sreptab,
           END OF ty_u_include_buffer .

  constants:
    BEGIN OF regex,
        include_in_function_group TYPE string VALUE `(/.+/)?L.*`,
        function_pool             TYPE string VALUE `(/.+/)?SAPL.+`,
      END OF regex .
  data SOURCE_REGISTRY type TY_SOURCE_REGISTRY .
  data COMPILER_REFERENCE_BUFFER type TY_COMPILER_BUFFER .
  data OO_CLIF_SOURCE_BUFFER type TY_OO_CLIF_BUFFER .
  data FUNCTION_SOURCE_BUFFER type TY_FUNCTION_BUFFER .
  data U_INCLUDE_BUFFER type TY_U_INCLUDE_BUFFER .
  data FLG_GRAIN_METHODS type ABAP_BOOL .
  class-data SINGLETON type ref to ZCL_ART_SOURCE_REPOSITORY .
  class-data SOURCE_CORER type ref to IF_ART_SOURCE_CORER .

  methods READ_SOURCE_FROM_DB
    importing
      !I_INCLUDE type PROGRAM
    returning
      value(R_SOURCE) type STRING_TABLE
    raising
      CX_RFAC_DYNAMIC .
  methods PREPARE_INCLUDE_4_COMPILER
    importing
      !I_INCLUDE type PROGRAM
      !I_MODE type STRING
    returning
      value(R_RESULT) type SREPTABLN .
  methods GET_SYNTAX_CHECK_INCLUDES4FUBA
    importing
      !I_MODE type STRING
      !I_INCLUDE type PROGRAMM
    returning
      value(R_RESULT) type SREPTAB .
  methods GET_SYNTAX_CHECK_INCLUDES4CS
    importing
      !I_CS_INCLUDE type PROGRAMM
      !I_MODE type STRING
    returning
      value(R_RESULT) type SREPTAB .
  methods GET_REGISTERED_CLASS_INCLUDES
    importing
      !I_CLASSNAME type SEOCLSNAME
      !I_MODE type STRING
    returning
      value(R_RESULT) type SREPTAB .
  methods GET_REGISTERED_INTF_INCLUDES
    importing
      !I_INTERFACENAME type SEOCLSNAME
      !I_MODE type STRING
    returning
      value(R_RESULT) type SREPTAB .
  methods CLEAR_COMPILER_BUFFER_FOR
    importing
      !I_MAIN_PROG type PROGRAM .
  methods GET_SYNTAX_CHECK_INCLUDES4ALL
    importing
      !I_MODE type STRING
      !I_FOCUSED_INCUDE_FOR_EXCLUSION type PROGRAM optional
    returning
      value(R_RESULT) type SREPTAB .
  methods IS_CS_INCLUDE_OR_INTF_POOL
    importing
      !I_INCLUDE type PROGRAM
    returning
      value(R_RESULT) type ABAP_BOOL .
  methods UPDATE_OO_CLIF_SOURCE
    importing
      !I_INCLUDE type PROGRAM
      !I_SOURCE type STRING_TABLE .
  methods INCLUDE_BELONGS_TO_MAIN_PROG
    importing
      !I_MAIN_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
    returning
      value(R_RESULT) type ABAP_BOOL .
  methods GRAIN_SOURCE
    importing
      !I_CURSOR_POSITION type ref to CL_PST_SOURCE_POSITION optional
      !I_SOURCE type STRING_TABLE
    returning
      value(R_RESULT) type STRING_TABLE .
  methods READ_SOURCE_INTERNAL
    importing
      !I_INCLUDE type PROGRAM
      !I_MODE type STRING
    returning
      value(R_SOURCE) type STRING_TABLE
    raising
      CX_RFAC_DYNAMIC .
  methods WAS_INCLUDE_REGISTERED
    importing
      !I_INCLUDE type PROGRAM
    returning
      value(R_RESULT) type ABAP_BOOL .
  methods IS_PART_OF_CS_INCLUDE
    importing
      !I_INCLUDE type PROGRAM
    returning
      value(R_RESULT) type ABAP_BOOL .
  methods GET_FUNCTION_MODULE_NAME
    importing
      !I_INCLUDE type PROGRAM
    returning
      value(R_RESULT) type RS38L_FNAM .
  methods GET_U_INCLUDE_FOR_FUNC_INCLUDE
    importing
      !I_INCLUDE type PROGRAM
    returning
      value(R_FUNCTION_MODULE_INCLUDE) type PROGRAM .
  methods GET_FUNCTION_SOURCE_OBJECT
    importing
      !I_FUNC_U_INCLUDE type PROGRAM
    returning
      value(R_RESULT) type ref to IF_WB_SOURCE
    raising
      CX_FB_FUNC_NOT_EXISTS .
  methods MAIN_PROGRAM_IS_FUNCTION_GROUP
    importing
      !I_MAIN_PROGRAM type PROGRAM
    returning
      value(R_RESULT) type ABAP_BOOL .
  methods ENRICH_WITH_GRAINED_U_INCLUDES
    importing
      !I_MAIN_PROGRAM type PROGRAM
    changing
      !C_SUSTITUTIONS type SREPTAB .
  methods READ_REPORT
    importing
      !I_INCLUDE type PROGRAM
    returning
      value(R_RESULT) type STRING_TABLE .
      "! Creates a fresh instance of a source repository (temporary singleton).
      "! Other callers can access the existing instance using the method get_instance( ).
      "! The import parameter i_flg_grain_methods controls whether the code inside of method implementations
      "! is replaced (abap_true) with whitespace or not (abap_false).
  class-methods CREATE
    importing
      !I_FLG_GRAIN_METHODS type ABAP_BOOL default ABAP_TRUE
    returning
      value(R_RESULT) type ref to ZCL_ART_SOURCE_REPOSITORY .
ENDCLASS.



CLASS ZCL_ART_SOURCE_REPOSITORY IMPLEMENTATION.


  METHOD CLEAR_COMPILER_BUFFER_FOR.

    DELETE TABLE me->compiler_reference_buffer WITH TABLE KEY main_program = i_main_prog.

  ENDMETHOD.


  METHOD CLEAR_REGISTER.

    CLEAR singleton.

  ENDMETHOD.


  METHOD CREATE.

    CALL FUNCTION 'RS_WORKING_AREA_INIT'.

    CREATE OBJECT singleton.
    singleton->flg_grain_methods = i_flg_grain_methods.
    source_corer = lcl_source_corer=>create( ).

    r_result = singleton.

  ENDMETHOD.


  METHOD ENRICH_WITH_GRAINED_U_INCLUDES.

    DATA: function_group_includes TYPE programt,
          source                  TYPE string_table,
          ref_to_source_code      TYPE REF TO string_table,
          substitution            TYPE sreptabln.

    FIELD-SYMBOLS: <include> LIKE LINE OF function_group_includes.

    IF u_include_buffer-main_prog_fugr <> i_main_program.

      CLEAR u_include_buffer.
      u_include_buffer-main_prog_fugr = i_main_program.

      function_group_includes = cl_rfac_utility=>get_includes_included_in_prog( i_main_program ).

      LOOP AT function_group_includes ASSIGNING <include>.

        IF cl_art_include_services=>include_belongs_to_function( <include> ) = abap_true.
          READ TABLE c_sustitutions WITH KEY name = <include> TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            source = read_report( <include> ).
            IF source IS NOT INITIAL.
              source = grain_source( i_source = source ).
              CREATE DATA ref_to_source_code.
              substitution-name = <include>.
              substitution-source = ref_to_source_code.
              APPEND substitution TO u_include_buffer-substitutions_4_u_includes.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDIF.

    APPEND LINES OF u_include_buffer-substitutions_4_u_includes TO c_sustitutions.

  ENDMETHOD.


  METHOD GET_COMPILER_FOR_MAIN_PROGRAM.

    DATA: entry                 LIKE LINE OF compiler_reference_buffer,
          substitutions         TYPE sreptab.

    READ TABLE compiler_reference_buffer WITH KEY main_program = i_main_program INTO entry.
    IF sy-subrc <> 0 OR i_mode = mode_4_compiler-fake_source.

      entry-main_program = i_main_program.

      substitutions = get_syntax_check_includes4all( i_mode = i_mode ).

      "improve performance for functiongroups grain non dirty functionmodule includes
      IF main_program_is_function_group( i_main_program ) = abap_true.
        enrich_with_grained_u_includes( EXPORTING i_main_program = i_main_program CHANGING c_sustitutions = substitutions ).
      ENDIF.

      CREATE OBJECT entry-compiler_ref
        EXPORTING
          p_name             = i_main_program
          p_substitutions    = substitutions
          p_no_package_check = abap_true.

      IF i_mode <> mode_4_compiler-fake_source.
        " instances populated with fake source are not stored they are most probably needed only once
        INSERT entry INTO TABLE compiler_reference_buffer.
      ENDIF.

    ENDIF.

    r_result = entry-compiler_ref.

  ENDMETHOD.


  METHOD GET_COMPILER_FOR_WHERE_USED.

    DATA: substitutions       TYPE sreptab,
          strlen_main_program TYPE i.

    strlen_main_program = strlen( i_main_program ).

    IF strlen_main_program = 32 AND i_main_program+30(2) = 'CP'.
      substitutions = get_registered_class_includes( i_classname = cl_oo_classname_service=>get_clsname_by_include( i_main_program ) i_mode = mode_4_compiler-full_registered_source ).

    ELSEIF strlen_main_program = 32 AND i_main_program+30(2) = 'IP'.
      substitutions = get_registered_intf_includes( i_interfacename = cl_oo_classname_service=>get_clsname_by_include( i_main_program ) i_mode = mode_4_compiler-full_registered_source ).

    ELSEIF main_program_is_function_group( i_main_program ) = abap_true.
      substitutions = get_registered_fugr_includes( i_function_pool = i_main_program i_mode = mode_4_compiler-full_registered_source ).

    ELSE.
      substitutions = get_registered_standalone_incs( i_mode = mode_4_compiler-full_registered_source ).

    ENDIF.

    CREATE OBJECT r_result
      EXPORTING
        p_name             = i_main_program
        p_substitutions    = substitutions
        p_no_package_check = abap_true.

  ENDMETHOD.


  METHOD GET_FUNCTION_MODULE_NAME.

    DATA: function_module_include TYPE program.

    function_module_include = get_u_include_for_func_include( i_include ).

    CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
      CHANGING
        funcname = r_result
        include  = function_module_include
      EXCEPTIONS
        OTHERS   = 1.

    IF sy-subrc <> 0.
      CLEAR r_result.
    ENDIF.

  ENDMETHOD.


  METHOD GET_FUNCTION_SOURCE_CONVERTER.

    DATA: buffer_entry LIKE LINE OF me->function_source_buffer,
          fb_header    TYPE header_fb.

    READ TABLE me->function_source_buffer WITH KEY func_u_include = i_func_u_include INTO buffer_entry.

    IF sy-subrc <> 0.
      buffer_entry-func_u_include = i_func_u_include.
      buffer_entry-funcname = get_function_module_name( i_func_u_include ).
      IF buffer_entry-funcname IS INITIAL.
        RETURN.
      ENDIF.
      fb_header-name = buffer_entry-funcname.
      fb_header-include = buffer_entry-func_u_include.
      buffer_entry-func_source_converter = cl_fb_func_source_converter=>create_instance(
                                         funcname    = buffer_entry-funcname
                                         includename = buffer_entry-func_u_include
                                         fb_header   = fb_header ).

      buffer_entry-func_source_converter->set_source( source = read_source( buffer_entry-func_u_include ) for_syntax_check = abap_true ).

      INSERT buffer_entry INTO TABLE function_source_buffer.

    ELSEIF buffer_entry-func_source_converter IS NOT BOUND.

      fb_header-name = buffer_entry-funcname.
      fb_header-include = buffer_entry-func_u_include.
      buffer_entry-func_source_converter = cl_fb_func_source_converter=>create_instance(
                                         funcname    = buffer_entry-funcname
                                         includename = buffer_entry-func_u_include
                                         fb_header   = fb_header ).

      buffer_entry-func_source_converter->set_source( source = read_source( buffer_entry-func_u_include ) for_syntax_check = abap_true ).

      MODIFY TABLE function_source_buffer FROM buffer_entry.

    ENDIF.

    r_result = buffer_entry-func_source_converter.

  ENDMETHOD.


  METHOD GET_FUNCTION_SOURCE_OBJECT.

    DATA: buffer_entry LIKE LINE OF me->function_source_buffer.

    READ TABLE me->function_source_buffer WITH KEY func_u_include = i_func_u_include INTO buffer_entry.

    IF sy-subrc <> 0.
      buffer_entry-func_u_include = i_func_u_include.
      buffer_entry-funcname = get_function_module_name( i_func_u_include ).
      IF buffer_entry-funcname IS INITIAL.
        RETURN.
      ENDIF.

      buffer_entry-func_source = cl_fb_func_source=>create_instance(
          func_name = buffer_entry-funcname
          version           = 'I'
          lifecycle_manager = cl_adt_corr_insert_dark=>get_instance( ) ).

      INSERT buffer_entry INTO TABLE function_source_buffer.

    ELSEIF buffer_entry-func_source IS NOT BOUND.

      buffer_entry-func_source = cl_fb_func_source=>create_instance(
          func_name = buffer_entry-funcname
          version           = 'I'
          lifecycle_manager = cl_adt_corr_insert_dark=>get_instance( ) ).

      MODIFY TABLE function_source_buffer FROM buffer_entry.

    ENDIF.

    r_result = buffer_entry-func_source.

  ENDMETHOD.


  METHOD get_instance.

    IF singleton IS NOT BOUND.
      zcl_art_source_repository=>create( ).
    ENDIF.

    r_result = singleton.

  ENDMETHOD.


  METHOD GET_METHOD_BODY.

    DATA: body_start      TYPE REF TO cl_pst_source_position,
          body_end        TYPE REF TO cl_pst_source_position,
          body_position   TYPE REF TO cl_pst_source_position.

    body_start = i_method_node->first_statement->get_source_position( )->get_end_position( ).
    body_end = i_method_node->last_statement->get_source_position( )->get_start_position( ).

    body_position = body_start->span( body_end ).

    r_result = get_source_snippet( body_position ).

  ENDMETHOD.


  METHOD GET_METHOD_SOURCE.

    r_result = get_source_snippet( i_method_node->source_position ).

  ENDMETHOD.


  METHOD GET_OO_CLIF_SOURCE_OBJECT.

    DATA: buffer_entry LIKE LINE OF me->oo_clif_source_buffer,
          include      TYPE program.

    READ TABLE me->oo_clif_source_buffer WITH KEY clif = i_clif_name INTO buffer_entry.

    IF sy-subrc <> 0.
      buffer_entry-clif = i_clif_name.
      buffer_entry-oo_clif_ref = cl_oo_factory=>create_instance( )->create_clif_source( clif_name = i_clif_name ).
      INSERT buffer_entry INTO TABLE oo_clif_source_buffer.
    ENDIF.

    CASE buffer_entry-oo_clif_ref->get_type( ).
      WHEN seoc_clstype_class.
        include = cl_oo_classname_service=>get_cs_name( i_clif_name ).
      WHEN seoc_clstype_interface.
        include = cl_oo_classname_service=>get_intfsec_name( i_clif_name ).
    ENDCASE.

    IF was_include_registered( include ) = abap_true.
      buffer_entry-oo_clif_ref->set_source( read_source_internal( i_include = include i_mode = i_mode ) ).
    ENDIF.

    r_result = buffer_entry-oo_clif_ref.

  ENDMETHOD.


  METHOD GET_REGISTERED_CLASS_INCLUDES.

    DATA: include     TYPE program,
          clif_source TYPE REF TO cl_oo_clif_source,
          exception   TYPE REF TO cx_root ##NEEDED.

    include = cl_oo_classname_service=>get_cs_name( i_classname ).
    IF was_include_registered( include ) = abap_true.

      TRY.

          clif_source ?= get_oo_clif_source_object( i_clif_name = i_classname i_mode = i_mode ).
          r_result = clif_source->get_syntax_check_includes( ).

        CATCH cx_oo_clif_scan_error cx_oo_clif_not_exists INTO exception.
          CLEAR r_result.
      ENDTRY.

    ENDIF.

    include = cl_oo_classname_service=>get_ccdef_name( i_classname ).
    IF was_include_registered( include ) = abap_true.
      APPEND prepare_include_4_compiler( i_include = include i_mode = i_mode ) TO r_result.
    ENDIF.

    include = cl_oo_classname_service=>get_ccimp_name( i_classname ).
    IF was_include_registered( include ) = abap_true.
      APPEND prepare_include_4_compiler( i_include = include i_mode = i_mode ) TO r_result.
    ENDIF.

    include = cl_oo_classname_service=>get_ccau_name( i_classname ).
    IF was_include_registered( include ) = abap_true.
      APPEND prepare_include_4_compiler( i_include = include i_mode = i_mode ) TO r_result.
    ENDIF.

    include = cl_oo_classname_service=>get_ccmac_name( i_classname ).
    IF was_include_registered( include ) = abap_true.
      APPEND prepare_include_4_compiler( i_include = include i_mode = i_mode ) TO r_result.
    ENDIF.

  ENDMETHOD.


  METHOD GET_REGISTERED_FUGR_INCLUDES.

    DATA: registered_source      LIKE LINE OF source_registry,
          regex_for_include      TYPE string,
          regex_for_fuba_include TYPE string,
          regex_matcher          TYPE REF TO cl_abap_matcher.


    IF contains( val = i_function_pool regex = '/.+/SAPL.+' case = abap_false ).
      "function group with namespace
      regex_matcher = cl_abap_matcher=>create( pattern = '(/.+/)?SAPL(.+)' text = i_function_pool ignore_case = abap_true ).
      regex_matcher->match( ).
      regex_for_include = |{ regex_matcher->get_submatch( 1 ) }L{ regex_matcher->get_submatch( 2 ) }...|.
      regex_for_fuba_include = |{ regex_matcher->get_submatch( 1 ) }L{ regex_matcher->get_submatch( 2 ) }U..|.
    ELSEIF contains( val = i_function_pool regex = 'SAPL.+' case = abap_false ).
      "function group without namespace
      regex_for_include = |{ i_function_pool+3 }...|.
      regex_for_fuba_include = |{ i_function_pool+3 }U..|.
    ELSE.
      "no function group
      RETURN.
    ENDIF.

    LOOP AT source_registry INTO registered_source.

      IF matches( val = registered_source-include regex = regex_for_include case = abap_false ).

        IF matches( val = registered_source-include regex = regex_for_fuba_include ).
          APPEND LINES OF get_syntax_check_includes4fuba( i_include = registered_source-include i_mode = i_mode ) TO r_result.
        ELSE.
          APPEND prepare_include_4_compiler( i_include = registered_source-include i_mode = i_mode ) TO r_result.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD GET_REGISTERED_INTF_INCLUDES.

    DATA: include     TYPE program.

    include = cl_oo_classname_service=>get_intfsec_name( i_interfacename ).
    IF was_include_registered( include ) = abap_true.

      APPEND prepare_include_4_compiler( i_include = include i_mode = i_mode ) TO r_result.

    ENDIF.

  ENDMETHOD.


  METHOD GET_REGISTERED_STANDALONE_INCS.

    DATA: registered_source LIKE LINE OF source_registry.

    LOOP AT source_registry INTO registered_source.

      IF registered_source-include = i_focused_incude_for_exclusion OR
         strlen( registered_source-include ) > 30 OR matches( val = registered_source-include(1) regex = regex-include_in_function_group ).
        CONTINUE.
      ENDIF.

      APPEND prepare_include_4_compiler( i_include = registered_source-include i_mode = i_mode ) TO r_result.

    ENDLOOP.

  ENDMETHOD.


  METHOD GET_SOURCE_LINE.
    DATA: entry LIKE LINE OF source_registry.

    READ TABLE source_registry WITH KEY include = i_source_position->include INTO entry.
    READ TABLE entry-source INDEX i_source_position->range-start-row INTO r_result.

  ENDMETHOD.


  METHOD GET_SOURCE_SNIPPET.

    DATA: entry LIKE LINE OF source_registry,
          line  LIKE LINE OF entry-source.

    READ TABLE source_registry WITH KEY include = i_source_position->include INTO entry.

    IF i_source_position->is_in_one_line( ) = abap_true.
      READ TABLE entry-source INDEX i_source_position->range-start-row INTO line.
      DATA(len) = i_source_position->range-end-col - i_source_position->range-start-col.
      r_result = line+i_source_position->range-start-col(len).
    ELSE.
      READ TABLE entry-source INDEX i_source_position->range-start-row INTO line.
      r_result = line+i_source_position->range-start-col. " && cl_abap_char_utilities=>newline.

      LOOP AT entry-source FROM i_source_position->range-start-row + 1 TO i_source_position->range-end-row - 1 INTO line.
        CONCATENATE r_result line INTO r_result SEPARATED BY cl_abap_char_utilities=>newline.
      ENDLOOP.

      READ TABLE entry-source INDEX i_source_position->range-end-row INTO line.
      r_result = |{ r_result }{ cl_abap_char_utilities=>newline }{ line(i_source_position->range-end-col) }|.
    ENDIF.

  ENDMETHOD.


  METHOD GET_SYNTAX_CHECK_INCLUDES4ALL.

    DATA: registered_source LIKE LINE OF source_registry.

    LOOP AT source_registry INTO registered_source.

      IF registered_source-include = i_focused_incude_for_exclusion.
        CONTINUE.
      ENDIF.

      IF strlen( registered_source-include ) = 32 AND registered_source-include+30(2) = 'CS'.
        APPEND LINES OF get_syntax_check_includes4cs( i_cs_include = registered_source-include i_mode = i_mode ) TO r_result.
        CONTINUE.
      ENDIF.

      IF cl_art_include_services=>include_belongs_to_function( registered_source-include ).
        APPEND LINES OF get_syntax_check_includes4fuba( i_include = registered_source-include i_mode = i_mode ) TO r_result.
        CONTINUE.
      ENDIF.

      APPEND prepare_include_4_compiler( i_include = registered_source-include i_mode = i_mode ) TO r_result.

    ENDLOOP.

  ENDMETHOD.


  METHOD GET_SYNTAX_CHECK_INCLUDES4CS.

    DATA: clif_source TYPE REF TO cl_oo_clif_source,
          exception   TYPE REF TO cx_root ##NEEDED.

    TRY.

        clif_source ?= get_oo_clif_source_object( i_clif_name = cl_oo_classname_service=>get_clsname_by_include( i_cs_include ) i_mode = i_mode ).
        r_result = clif_source->get_syntax_check_includes( ).

      CATCH cx_oo_clif_scan_error cx_oo_clif_not_exists INTO exception.
        CLEAR r_result.
    ENDTRY.

  ENDMETHOD.


  METHOD GET_SYNTAX_CHECK_INCLUDES4FUBA.

    DATA: fb_func_source_converter TYPE REF TO cl_fb_func_source_converter.

    TRY.
        fb_func_source_converter = get_function_source_converter( i_func_u_include = get_u_include_for_func_include( i_include ) ).

        IF fb_func_source_converter IS BOUND.


          fb_func_source_converter->set_source( source = me->read_source_internal( i_include = i_include i_mode = i_mode ) for_syntax_check = abap_true ).

          fb_func_source_converter->get_includes_4_syntax_check( EXPORTING global_check  = abap_true
                                                                 IMPORTING replacing_tab = r_result ).

        ENDIF.
      CATCH cx_fb_scan_error.
        CLEAR r_result.
    ENDTRY.


  ENDMETHOD.


  METHOD GET_U_INCLUDE_FOR_FUNC_INCLUDE.

    DATA offset TYPE i.

    r_function_module_include = i_include. " must be changing parameter -- but will not be changed
    offset = strlen( r_function_module_include ) - 3.
    r_function_module_include+offset(1) = 'U'.

  ENDMETHOD.


  METHOD GRAIN_SOURCE.

    IF me->flg_grain_methods = abap_true.
      r_result = source_corer->grain_procedures( i_source_code = i_source i_cursor_position = i_cursor_position ).
    ELSE.
      r_result = i_source.
    ENDIF.

  ENDMETHOD.


  METHOD INCLUDE_BELONGS_TO_MAIN_PROG.
    DATA: main_programs TYPE TABLE OF program,
          main_program  LIKE LINE OF main_programs.

    CALL FUNCTION 'RS_GET_MAINPROGRAMS'
      EXPORTING
        name         = i_include
      TABLES
        mainprograms = main_programs.

    IF lines( main_programs ) = 1.
      READ TABLE main_programs INDEX 1 INTO main_program.
      r_result = boolc( main_program = i_main_program ).
      RETURN.
    ENDIF.

*    "classpool
*    if strlen( i_main_program ) = 32 and i_main_program+30(2) = 'CP'.
*      r_result = boolc( strlen( i_include ) > 31 and i_include+30(1) = 'C' and i_main_program(30) = i_include(30) ).
*      return.
*    endif.
*
*    "interfacepool
*    if strlen( i_main_program ) = 32 and i_main_program+30(2) = 'IP'.
*      r_result = boolc( strlen( i_include ) > 31 and i_include+30(1) = 'I' and i_main_program(30) = i_include(30) ).
*      return.
*    endif.

  ENDMETHOD.


  METHOD IS_CS_INCLUDE_OR_INTF_POOL.
    r_result = boolc( strlen( i_include ) = 32 AND ( i_include+30(2) = seop_ext_class_source OR i_include+30(2) = srext_ext_interface_pool ) ).
  ENDMETHOD.


  METHOD IS_DIRTY_OUTSIDE_MAIN_PROGRAM.
    DATA : all_includes      TYPE programt,
           registered_source TYPE ty_source_registry_entry.

    IF i_main_program+30(1) = 'C'.
      all_includes = cl_oo_classname_service=>get_all_class_includes( class_name = cl_oo_classname_service=>get_clsname_by_include( i_main_program ) ).
    ELSE.
      CALL FUNCTION 'RS_GET_ALL_INCLUDES'
        EXPORTING
          program             = i_main_program
          with_inactive_incls = abap_true
        TABLES
          includetab          = all_includes
        EXCEPTIONS
          OTHERS              = 0.
      READ TABLE all_includes WITH KEY table_line = i_main_program TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        INSERT i_main_program INTO TABLE all_includes.
      ENDIF.
    ENDIF.

    LOOP AT me->source_registry INTO registered_source WHERE is_dirty = abap_true.
      READ TABLE all_includes WITH KEY table_line = registered_source-include TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        r_is_dirty = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD IS_PART_OF_CS_INCLUDE.

    DATA: characters_31_and_32 TYPE string.

    IF strlen( i_include ) >= 32.

      characters_31_and_32 = i_include+30(2).
      IF characters_31_and_32 = 'CM' OR characters_31_and_32 = 'CU' OR characters_31_and_32 = 'CI' OR characters_31_and_32 = 'CO'.
        r_result = abap_true.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD MAIN_PROGRAM_IS_FUNCTION_GROUP.
    r_result = boolc( matches( val = i_main_program regex = regex-function_pool ) ).
  ENDMETHOD.


  METHOD PREPARE_INCLUDE_4_COMPILER.

    DATA: source_code        TYPE string_table,
          ref_to_source_code TYPE REF TO string_table.

    CREATE DATA ref_to_source_code.
    r_result-name = i_include.
    source_code = me->read_source_internal( i_include = i_include i_mode = i_mode ).
    ref_to_source_code->* = source_code.
    r_result-source = ref_to_source_code.

  ENDMETHOD.


  METHOD READ_REPORT.

    READ REPORT i_include INTO r_result STATE 'I'.
    IF sy-subrc <> 0.
      READ REPORT i_include INTO r_result.
      IF sy-subrc <> 0.
        "include does not exist yet but we don't care
        CLEAR r_result.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD READ_SOURCE.

    r_source = me->read_source_internal( i_include = i_include i_mode = i_mode ).

  ENDMETHOD.


  METHOD READ_SOURCE_FROM_DB.

    DATA: l_function_module_wb_source TYPE REF TO if_wb_source,
          l_clif_source               TYPE REF TO if_oo_clif_source.


    IF is_cs_include_or_intf_pool( i_include ) = abap_true.

      " 2a) read class/interface
      TRY.
          l_clif_source = get_oo_clif_source_object( i_clif_name = cl_oo_classname_service=>get_clsname_by_include( i_include )
                                                     i_mode = mode_4_compiler-grained_registered_source ).
          l_clif_source->get_source( IMPORTING source = r_source ).
        CATCH cx_oo_clif_not_exists.
          RAISE EXCEPTION TYPE cx_rfac_dynamic
            EXPORTING
              textid = cx_rfac_dynamic=>include_not_found
              msgv1  = i_include && ''.
      ENDTRY.

    ELSEIF cl_art_include_services=>include_belongs_to_function( i_include ) = abap_true.

      TRY.
          l_function_module_wb_source = get_function_source_object( i_func_u_include = get_u_include_for_func_include( i_include ) ).
          IF l_function_module_wb_source IS BOUND.
            l_function_module_wb_source->get_source( IMPORTING source = r_source ).
          ENDIF.
        CATCH cx_fb_func_not_exists cx_wb_source_permission_error.
          RAISE EXCEPTION TYPE cx_rfac_dynamic
            EXPORTING
              textid = cx_rfac_dynamic=>include_not_found
              msgv1  = i_include && ''.
      ENDTRY.

    ELSE.

      r_source = read_report( i_include ).


    ENDIF.

  ENDMETHOD.


  METHOD READ_SOURCE_INTERNAL.

    DATA: l_source_registry_entry  TYPE ty_source_registry_entry,
          cs_include               TYPE program,
          clif_source              TYPE REF TO cl_oo_clif_source,
          includes_contained_in_cs TYPE sreptab,
          include                  TYPE sreptabln,
          include_to_read          TYPE program,
          function_u_include       TYPE program.


    include_to_read = i_include.

    "requested include is part of CS include delegate to cl_oo_clif_source if CS include is dirty
    IF is_part_of_cs_include( i_include ) = abap_true.

      cs_include = i_include(30) && 'CS'.

      IF was_include_registered( i_include = cs_include ) = abap_true.
        clif_source ?= me->get_oo_clif_source_object( i_clif_name = cl_oo_classname_service=>get_clsname_by_include( cs_include ) i_mode = i_mode ).

        includes_contained_in_cs = clif_source->get_includes( ).

        READ TABLE includes_contained_in_cs WITH KEY name = i_include INTO include.
        r_source = include-source->*.
        RETURN.

      ENDIF.

    ELSEIF cl_art_include_services=>include_belongs_to_function( i_include ) = abap_true.

      "turn $include of a function module into the corresponding U include
      function_u_include = get_u_include_for_func_include( i_include ).
      IF was_include_registered( i_include = function_u_include ).

        include_to_read = function_u_include.

      ENDIF.


    ENDIF.

    " 1) search in registry
    READ TABLE source_registry WITH KEY include = include_to_read INTO l_source_registry_entry.
    IF sy-subrc <> 0.

      " 2) if not found read source from database and add it to registry
      l_source_registry_entry-include = include_to_read.
      l_source_registry_entry-source = read_source_from_db( to_upper( include_to_read ) ).
      l_source_registry_entry-grained_source = grain_source( l_source_registry_entry-source ).
      INSERT l_source_registry_entry INTO TABLE source_registry.

    ENDIF.

    CASE i_mode.
      WHEN mode_4_compiler-full_registered_source.
        r_source = l_source_registry_entry-source.
      WHEN mode_4_compiler-grained_registered_source.
        r_source = l_source_registry_entry-grained_source.
      WHEN mode_4_compiler-fake_source.
        IF l_source_registry_entry-fake_source IS NOT INITIAL.
          r_source = l_source_registry_entry-fake_source.
        ELSE.
          r_source = l_source_registry_entry-grained_source.
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD REGISTER_FAKE_SOURCE.
    FIELD-SYMBOLS: <entry> LIKE LINE OF source_registry.

    READ TABLE source_registry WITH KEY include = i_include ASSIGNING <entry>.
    IF sy-subrc = 0.
      <entry>-fake_source = i_fake_source.
    ENDIF.

  ENDMETHOD.


  METHOD REGISTER_SOURCE.

    DATA: new_source_registry_entry TYPE ty_source_registry_entry.

    clear_compiler_buffer_for( i_main_prog = cl_art_include_services=>derive_mainprog_by_include( i_include ) ).

    new_source_registry_entry-include = i_include.

    READ TABLE source_registry WITH KEY include = i_include TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      "first time registration for that include

      IF i_source IS NOT SUPPLIED.
        new_source_registry_entry-source = read_source_from_db( to_upper( i_include ) ).
      ELSE.
        new_source_registry_entry-source = i_source.
      ENDIF.

      new_source_registry_entry-grained_source = grain_source( i_cursor_position = i_cursor_position i_source = new_source_registry_entry-source ).
      new_source_registry_entry-is_dirty       = i_is_dirty.

      INSERT new_source_registry_entry INTO TABLE source_registry.

    ELSE.

      "second/third time registration which happens in some test scenarios
      new_source_registry_entry-source         = i_source.
      new_source_registry_entry-grained_source = grain_source( i_cursor_position = i_cursor_position i_source = new_source_registry_entry-source ).
      new_source_registry_entry-is_dirty       = i_is_dirty.
      MODIFY TABLE source_registry FROM new_source_registry_entry.

    ENDIF.

    IF is_cs_include_or_intf_pool( i_include ) = abap_true AND i_source IS NOT INITIAL.
      update_oo_clif_source( i_include = new_source_registry_entry-include i_source = new_source_registry_entry-grained_source ).
    ENDIF.

  ENDMETHOD.


  METHOD UPDATE_OO_CLIF_SOURCE.

    DATA: clif_name    TYPE seoclsname,
          buffer_entry LIKE LINE OF me->oo_clif_source_buffer.

    clif_name = cl_oo_classname_service=>get_clsname_by_include( i_include ).

    READ TABLE me->oo_clif_source_buffer WITH KEY clif = clif_name INTO buffer_entry.
    IF sy-subrc = 0.
      buffer_entry-oo_clif_ref->set_source( i_source ).
    ENDIF.

  ENDMETHOD.


  METHOD WAS_INCLUDE_REGISTERED.

    READ TABLE source_registry WITH KEY include = i_include TRANSPORTING NO FIELDS.
    r_result = boolc( sy-subrc = 0 ).

  ENDMETHOD.
ENDCLASS.
