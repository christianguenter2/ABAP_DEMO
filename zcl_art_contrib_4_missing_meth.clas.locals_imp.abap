


CLASS lcl_method_call IMPLEMENTATION.

  METHOD constructor.
    me->tokens_qualified   = i_tokens_qualified.
    me->tokens_unqualified = i_tokens_unqualified.
    me->blackboard         = i_blackboard.
  ENDMETHOD.

  METHOD no_closing_tag.

    DATA token_check TYPE stokesx.

    READ TABLE me->tokens_qualified INTO token_check INDEX 1.
    IF token_check-type <> sana_tok_keyword.
      LOOP AT me->tokens_unqualified INTO token_check
        WHERE str = ')' AND type <> 'S'.
      ENDLOOP.
      IF sy-subrc <> 0.
        r_result = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD find_method_selector.

    r_position = find( val = i_token_string sub = co_arrow occ = -1 ).

    IF r_position <= 0.
      r_position = find( val = i_token_string sub = co_arrow_static occ = -1 ).
    ENDIF.

  ENDMETHOD.

  METHOD get_method_name.
    DATA: arrow_pos TYPE i.
    DATA: selection_content TYPE string.

    IF no_closing_tag( ) = abap_false.
      selection_content = me->blackboard->get_selection_content( ).
      arrow_pos = find_method_selector( selection_content ).
      IF arrow_pos > 0.
        r_result = substring( val = selection_content off = arrow_pos + 2 ). "cut the ->
      ELSE.
        r_result = selection_content.
      ENDIF.

      IF r_result CS '('.
        r_result = substring_before( val = r_result sub = '(' ).
      ENDIF.

      validate_method_name( CHANGING method_name = r_result ).

    ENDIF.
  ENDMETHOD.

  METHOD validate_method_name.
    IF method_name = 'CONSTRUCTOR'.
      CLEAR method_name.
    ENDIF.
  ENDMETHOD.

  METHOD get_clif_name.

    DATA: arrow_pos TYPE i.
    DATA: selection_content TYPE string.
    DATA: call_reference TYPE        string,
          type_reference TYPE REF TO cl_pst_node.

    selection_content = me->blackboard->get_selection_content( ).
    IF selection_content CS '~'.
      "interface
      r_result = substring_before( val = selection_content sub = '~' ).
      IF r_result CS co_arrow.
        r_result = substring_after( val = r_result sub = co_arrow ).
        RETURN.
      ENDIF.
      IF r_result CS co_arrow_static.
        r_result = substring_after( val = r_result sub = co_arrow_static ).
        RETURN.
      ENDIF.
    ELSE.
      "class
      arrow_pos         = find_method_selector( selection_content ).
      call_reference    = get_call_reference( selection_content ).

      IF call_reference = 'SUPER'.
        RETURN.
      ENDIF.

      IF arrow_pos > 0 AND call_reference <> 'ME'.
        type_reference = cl_art_type_determination=>create( blackboard )->determine_clif_for_method_call( i_is_static = is_static( ) ).
        IF type_reference IS BOUND.
          r_result = type_reference->reference_node->name.
        ENDIF.
        RETURN.
      ELSE.
        IF me->blackboard->get_surrounding_class( ) IS BOUND.
          r_result = me->blackboard->get_surrounding_class( )->name.
        ELSEIF me->blackboard->get_surrounding_interface( ) IS BOUND.
          r_result = me->blackboard->get_surrounding_interface( )->name.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_call_reference.
    DATA arrow_pos TYPE i.

    arrow_pos         = find_method_selector( i_token_string ).
    CHECK arrow_pos > 0.
    r_value = substring( val = i_token_string len = arrow_pos ).

    IF r_value CS lcl_method_call=>co_arrow.
      r_value   = substring_after( val = r_value sub = lcl_method_call=>co_arrow occ = -1 ).
    ELSEIF r_value CS lcl_method_call=>co_arrow_static.
      r_value   = substring_before( val = r_value sub = lcl_method_call=>co_arrow_static occ = -1 ).
    ENDIF.
  ENDMETHOD.

  METHOD parse_signature.
    DATA: method_name_token_index TYPE i,
          token                   TYPE stokesx,
          token_tabix             TYPE i.

    me->method_descr = method_descr.

    method_name_token_index = me->blackboard->get_start_token_index_in_stmnt( ).
    last_token_of_method = find_closing_token_of_method( method_name_token_index ).


    IF method_name_token_index + 1 < last_token_of_method.
      token_tabix = method_name_token_index + 1.



      WHILE token_tabix <= last_token_of_method.
        READ TABLE tokens_qualified INDEX token_tabix INTO token.
        CASE token-type.

          WHEN sana_tok_word.
            CASE token-str.
              WHEN 'EXPORTING'.
                keyword_exporting( CHANGING c_token_tabix = token_tabix ).
                CONTINUE.

              WHEN 'IMPORTING'.
                keyword_importing( CHANGING c_token_tabix = token_tabix ).
                CONTINUE.

              WHEN 'CHANGING'.
                keyword_changing( CHANGING c_token_tabix = token_tabix ).
                CONTINUE.

              WHEN 'RECEIVING'.
                keyword_receiving( CHANGING c_token_tabix = token_tabix ).
                CONTINUE.

              WHEN 'EXCEPTIONS'.
                EXIT.
              WHEN '|'.
                exporting_without_keyword( i_start_index = token_tabix ).
                EXIT.
            ENDCASE.

          WHEN OTHERS.
            exporting_without_keyword( i_start_index = token_tabix ).
            EXIT.

        ENDCASE.

        ADD 1 TO token_tabix.
      ENDWHILE.
    ENDIF.

    returning_in_move( i_from = method_name_token_index i_to = last_token_of_method ).

    method_descr = me->method_descr.

  ENDMETHOD.

  METHOD find_closing_token_of_method.
    DATA: token                    TYPE stokesx,
          tabix                    TYPE i,
          counter_open_brackets    TYPE i.

    IF i_token_index_method_name >= lines( tokens_qualified ).
      r_result = lines( tokens_qualified ).
      RETURN.
    ENDIF.

    READ TABLE tokens_qualified INDEX i_token_index_method_name INTO token.
    ASSERT token-type = sana_tok_method.
    IF ends_with_opening_bracket( token-str ) = abap_true.
      ADD 1 TO counter_open_brackets.
    ELSE.
      "CALL METHOD statements without brackets
      r_result = lines( tokens_qualified ).
      RETURN.
    ENDIF.

    LOOP AT tokens_qualified FROM i_token_index_method_name + 1 INTO token.
      tabix = sy-tabix.

      IF ( ( token-type = sana_tok_method OR token-type = sana_tok_field ) AND starts_with_closing_bracket( token-str ) = abap_true )
      OR ( token-type = sana_tok_word AND token-str = ')' ).
        SUBTRACT 1 FROM counter_open_brackets.
      ENDIF.

      IF counter_open_brackets = 0.
        r_result = tabix.
        RETURN.
      ENDIF.

      "if token ends with opening parenthesis
      IF token-type = sana_tok_method AND ends_with_opening_bracket( token-str ) = abap_true.
        ADD 1 TO counter_open_brackets.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD ends_with_opening_bracket.
    r_result = boolc( matches( val = i_token_str regex = `[^\(]+\(` ) ).
  ENDMETHOD.

  METHOD starts_with_closing_bracket.
    r_result = boolc( matches( val = i_token_str regex = `\)[^\)]*` ) ).
  ENDMETHOD.

  METHOD ends_with_closing_bracket.
    r_result = boolc( matches( val = i_token_str regex = `[^\)]+\)` ) ).
  ENDMETHOD.


  METHOD exporting_without_keyword.
    DATA token TYPE stokesx.
    DATA tabix TYPE i.
    DATA param TYPE if_rfac_impl_types=>ts_param.
    DATA in_method TYPE abap_bool VALUE abap_true.
    DATA importing_found TYPE abap_bool.

    READ TABLE tokens_qualified INTO token INDEX i_start_index.
    IF sy-subrc = 0 AND token-type <> sana_tok_word.

      tabix = i_start_index .
      WHILE tabix <= me->last_token_of_method.
        READ TABLE tokens_qualified INTO token INDEX tabix.

        IF token-type = sana_tok_method AND ends_with_opening_bracket( token-str ) = abap_true.
          in_method = abap_false.
        ENDIF.

        IF token-type = sana_tok_word AND token-str = ')'.
          in_method = abap_true.
        ENDIF.

        IF token-type = sana_tok_word AND token-str = '=' AND in_method = abap_true.
          fill_method_parameter(
            EXPORTING i_equals_tabix = tabix
            IMPORTING e_next_token_index = tabix
            CHANGING c_parameters   = method_descr-importing ).
          importing_found = abap_true.
        ELSE.
          ADD 1 TO tabix.
        ENDIF.
      ENDWHILE.

      IF importing_found IS INITIAL AND ( i_start_index <= me->last_token_of_method ).
        READ TABLE tokens_qualified INTO token INDEX i_start_index.
        param-name   = derive_parameter_name( token-str ).
        extract_actual_parameter( EXPORTING i_start_token_index = i_start_index
                                  IMPORTING e_actual = param-actual ).
        get_parameter_type( CHANGING c_parameter = param ).
        change_name_of_field_symbol( CHANGING c_field_name = param-name ).
        APPEND param TO method_descr-importing.
      ENDIF.

    ELSEIF token-type = sana_tok_word AND token-str = '|'. "string pattern
      READ TABLE tokens_qualified INTO token INDEX i_start_index + 1.
      IF sy-subrc = 0.
        param-name = derive_parameter_name( token-str ).
        extract_actual_parameter( EXPORTING i_start_token_index = i_start_index
                                  IMPORTING e_actual = param-actual ).
        get_parameter_type( CHANGING c_parameter = param ).
        APPEND param TO method_descr-importing.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD derive_parameter_name.

    r_result = to_upper( cl_art_name_service=>derive_abap_name( i_actual ) ).

    IF contains( val = r_result start = '_' ).
      r_result = 'I' && r_result.
    ELSEIF NOT contains( val = r_result start = 'I_' ).
      r_result = 'I_' && r_result.
    ENDIF.

  ENDMETHOD.

  METHOD keyword_exporting.
    DATA token TYPE stokesx.

    WHILE c_token_tabix <= lines( tokens_qualified ).
      READ TABLE tokens_qualified INDEX c_token_tabix INTO token.

      IF token-type = sana_tok_word AND
        (  token-str = 'IMPORTING'
        OR token-str = 'CHANGING'
        OR token-str = 'RECEIVING'
        OR token-str = 'EXCEPTIONS' ).
        EXIT.
      ENDIF.

      IF token-type = sana_tok_word AND token-str = '='.

        fill_method_parameter( EXPORTING i_equals_tabix     = c_token_tabix
                               IMPORTING e_next_token_index = c_token_tabix
                               CHANGING  c_parameters       = method_descr-importing ).
      ELSE.
        ADD 1 TO c_token_tabix.
      ENDIF.

    ENDWHILE.

  ENDMETHOD.

  METHOD keyword_importing.
    DATA: token TYPE stokesx.

    WHILE c_token_tabix <= lines( tokens_qualified ).
      READ TABLE tokens_qualified INDEX c_token_tabix INTO token.

      IF token-type = sana_tok_word AND
        (  token-str = 'CHANGING'
        OR token-str = 'EXCEPTIONS' ).
        EXIT.
      ENDIF.

      IF token-type = sana_tok_word AND token-str = '='.
        fill_method_parameter( EXPORTING i_equals_tabix     = c_token_tabix
                                 IMPORTING e_next_token_index = c_token_tabix
                                 CHANGING  c_parameters       = method_descr-exporting ).
      ELSE.
        ADD 1 TO c_token_tabix.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.

  METHOD keyword_changing.

    DATA token TYPE stokesx.

    WHILE c_token_tabix <= lines( tokens_qualified ).
      READ TABLE tokens_qualified INDEX c_token_tabix INTO token.

      IF token-type = sana_tok_word AND token-str = 'EXCEPTIONS'.
        EXIT.
      ENDIF.

      IF token-type = sana_tok_word AND token-str = '='.
        fill_method_parameter( EXPORTING i_equals_tabix     = c_token_tabix
                                 IMPORTING e_next_token_index = c_token_tabix
                                 CHANGING  c_parameters       = method_descr-changing ).
      ELSE.
        ADD 1 TO c_token_tabix.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.

  METHOD keyword_receiving.
    DATA name TYPE string.
    DATA actual TYPE string.

    get_name_and_actual_frm_tokens(
      EXPORTING
        i_equals_tabix  = c_token_tabix + 2
      IMPORTING
        e_name          = name
        e_actual        = actual
        e_next_token_index = c_token_tabix
    ).

    fill_returning( i_name = name i_actual = actual ).

  ENDMETHOD.

  METHOD fill_method_parameter.
    DATA type TYPE if_rfac_impl_types=>ts_param.

    get_name_and_actual_frm_tokens(
      EXPORTING
        i_equals_tabix = i_equals_tabix
      IMPORTING
        e_name         = type-name
        e_actual       = type-actual
        e_next_token_index = e_next_token_index
    ).

    IF type-name <> ')'.
      get_parameter_type( CHANGING c_parameter = type ).
      change_name_of_field_symbol( CHANGING c_field_name = type-name ).
      APPEND type TO c_parameters.
    ENDIF.

  ENDMETHOD.

  METHOD get_parameter_type.

    DATA: regex_integer_literal         TYPE        string VALUE '[\+\-]?\d+',
          type_determination_helper     TYPE REF TO cl_art_type_determination.

    type_determination_helper = cl_art_type_determination=>create( blackboard ).

    IF c_parameter-actual IS INITIAL.
      parameter_type_default( CHANGING c_parameter = c_parameter ).

    ELSEIF matches( val = c_parameter-actual regex = regex_integer_literal ).
      parameter_type_i( CHANGING c_parameter = c_parameter ).

    ELSEIF c_parameter-actual = 'ME'.
      parameter_type_me( CHANGING c_parameter = c_parameter ).

    ELSEIF ends_with_closing_bracket( c_parameter-actual ) = abap_true.
       "must be before string determination since inner parameters of a method call could contain `'|
       type_determination_helper->determine_type_4_actual_param( CHANGING c_parameter = c_parameter ).

    ELSEIF c_parameter-actual CA '|''`'.
      parameter_type_string( CHANGING c_parameter = c_parameter ).

    ELSE.
      type_determination_helper->determine_type_4_actual_param( CHANGING c_parameter = c_parameter ).
    ENDIF.

  ENDMETHOD.

  METHOD returning_in_move.

    DATA: token           TYPE stokesx,
          token_with_type TYPE stokesx.

    READ TABLE tokens_qualified INTO token INDEX i_to.
    IF token-type = sana_tok_method AND starts_with_closing_bracket( token-str ) = abap_true.
      "chained method returning ref to object
      fill_returning( i_flg_chained_call = abap_true ).
      RETURN.
    ENDIF.

    READ TABLE tokens_qualified INTO token INDEX i_from - 1.

    "method call is right side of move-statement
    IF token-type = sana_tok_word AND token-str = '='.
      READ TABLE tokens_qualified INTO DATA(target_variable_token) INDEX i_from - 2.
      fill_returning( i_name = co_returning_default i_actual = target_variable_token-str ).
    ENDIF.

    READ TABLE tokens_qualified INTO token INDEX 1.

    CASE token-type.

      WHEN sana_tok_keyword.

        CASE token-str.
          WHEN 'IF' OR 'ELSEIF'.
            token_with_type = get_token_with_type_info( EXPORTING i_method_start = i_from i_method_end = i_to ).
            fill_returning( i_name = co_returning_default i_actual = token_with_type-str ).
          WHEN 'CASE' OR 'WHEN' OR 'APPEND' OR 'INSERT' OR 'MODIFY'.
            fill_returning( i_name = co_returning_default ).
          WHEN 'LOOP' OR 'WRITE'.
            fill_returning( i_name = co_returning_default ).
        ENDCASE.

    ENDCASE.

  ENDMETHOD.

  METHOD fill_returning.

    method_descr-returning-name     = i_name.
    method_descr-returning-byvalue  = abap_true.

    IF i_flg_chained_call = abap_true.
      method_descr-returning-typetype = 'TYPE REF TO'.
      method_descr-returning-type     = 'OBJECT'.
    ELSE.
      method_descr-returning-actual   = i_actual.
      get_parameter_type( CHANGING c_parameter = method_descr-returning ).

      IF method_descr-returning-type = 'ANY'.
        method_descr-returning-actual = ''.
      ENDIF.

      change_name_of_field_symbol( CHANGING c_field_name = method_descr-returning-name ).
    ENDIF.

  ENDMETHOD.

  METHOD parameter_type_me.

    IF me->blackboard->get_surrounding_class( ) IS BOUND.
      c_parameter-typetype = 'TYPE REF TO'.
      c_parameter-type = me->blackboard->get_surrounding_class( )->name.
    ELSE.
      parameter_type_default( CHANGING c_parameter = c_parameter ).
    ENDIF.

  ENDMETHOD.

  METHOD parameter_type_string.

    c_parameter-typetype = 'TYPE'.
    c_parameter-type     = 'STRING'.

  ENDMETHOD.

  METHOD parameter_type_i.

    c_parameter-typetype = 'TYPE'.
    c_parameter-type     = 'I'.

  ENDMETHOD.

  METHOD parameter_type_default.

    c_parameter-typetype = 'TYPE'.
    c_parameter-type = 'ANY'.

  ENDMETHOD.

  METHOD change_name_of_field_symbol.

    IF c_field_name(1) = '<'.
      c_field_name = substring_after( val = c_field_name sub = '<' ).
      c_field_name = substring_before( val = c_field_name sub = '>' ).
    ENDIF.

  ENDMETHOD.

  METHOD get_name_and_actual_frm_tokens.
    DATA token TYPE stokesx.

    CLEAR: e_name, e_actual.

    READ TABLE tokens_qualified INTO token INDEX i_equals_tabix.
    IF token-type = sana_tok_word AND token-str = '='.
      READ TABLE tokens_qualified INTO token INDEX i_equals_tabix - 1.
      e_name = token-str.
      extract_actual_parameter( EXPORTING i_start_token_index = i_equals_tabix + 1
                                IMPORTING e_actual = e_actual
                                          e_next_token_index = e_next_token_index ).
    ENDIF.

  ENDMETHOD.

  METHOD extract_actual_parameter.

    DATA: current_token            TYPE          stokesx,
          next_token               TYPE          stokesx,
          string_expression        TYPE          string,
          nested_method_call       TYPE          string,
          tabix                    TYPE          i,
          determinating_words      TYPE RANGE OF string,
          determinating_word_entry LIKE LINE OF  determinating_words.

    determinating_word_entry-sign = 'I'.
    determinating_word_entry-option = 'EQ'.
    determinating_word_entry-low = ')'.
    APPEND determinating_word_entry TO determinating_words.
    determinating_word_entry-low = 'EXPORTING'.
    APPEND determinating_word_entry TO determinating_words.
    determinating_word_entry-low = 'IMPORTING'.
    APPEND determinating_word_entry TO determinating_words.
    determinating_word_entry-low = 'CHANGING'.
    APPEND determinating_word_entry TO determinating_words.
    determinating_word_entry-low = 'RECEIVING'.
    APPEND determinating_word_entry TO determinating_words.
    determinating_word_entry-low = 'EXCEPTIONS'.
    APPEND determinating_word_entry TO determinating_words.

    tabix = i_start_token_index.

    WHILE tabix <= me->last_token_of_method.
      READ TABLE tokens_qualified INDEX tabix INTO current_token.
      IF current_token-type = sana_tok_word AND current_token-str = '|'.

        extract_string_expression( EXPORTING i_start_token_index = tabix IMPORTING e_next_token_index = tabix e_string_expression = string_expression ).

        IF e_actual IS INITIAL.
          e_actual = string_expression.
        ELSE.
          e_actual = |{ e_actual } { string_expression }|.
        ENDIF.

      ELSEIF current_token-type = sana_tok_method.

        extract_nested_method_call( EXPORTING i_start_token_index = tabix IMPORTING e_next_token_index = tabix e_nested_call = nested_method_call ).

        IF e_actual IS INITIAL.
          e_actual = nested_method_call.
        ELSE.
          e_actual = |{ e_actual } { nested_method_call }|.
        ENDIF.

      ELSE.
        IF current_token-type = sana_tok_word AND current_token-str IN determinating_words.
          e_next_token_index = tabix.
          RETURN.
        ENDIF.

        IF tabix + 1 <= me->last_token_of_method.
          READ TABLE tokens_qualified INDEX tabix + 1 INTO next_token.
        ENDIF.

        IF next_token-type = sana_tok_word AND next_token-str = '='.
          e_next_token_index = tabix.
          RETURN.
        ENDIF.

        IF e_actual IS INITIAL.
          e_actual = current_token-str.
        ELSE.
          e_actual = |{ e_actual } { current_token-str }|.
        ENDIF.

        ADD 1 TO tabix.

      ENDIF.

    ENDWHILE.

    e_next_token_index = tabix.

  ENDMETHOD.

  METHOD is_static.
    DATA col_method TYPE i.
    DATA col_static TYPE i.
    DATA selection_content TYPE string.
    DATA: method_def TYPE REF TO cl_pst_method_definition.


    selection_content = me->blackboard->get_selection_content( ).
    col_method = find( val = selection_content sub = co_arrow occ = -1  ).
    col_static = find( val = selection_content sub = co_arrow_static occ = -1 ).

    IF col_static > col_method. " method call with preceding class
      r_result = abap_true.
    ENDIF.

    IF col_static = -1 AND col_method = -1. " just a method call without preceding class/object
      " static-flag should be identical to surrounding method, if definition of the later already exists, static otherwise
      IF me->blackboard->get_surrounding_method( ) IS BOUND.
        method_def = me->blackboard->get_surrounding_method( )->get_method_definition( ).
        IF method_def IS BOUND.
          r_result = method_def->is_static_method( ).
        ELSE.
          r_result = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD extract_string_expression.

    DATA token TYPE stokesx.
    DATA tabix TYPE i.
    DATA start TYPE i.
    DATA end TYPE i.
    DATA source_line TYPE string.
    DATA source_line_index TYPE i.
    DATA source_code TYPE rswsourcet.
    DATA focused_include TYPE programm.

    CLEAR: e_string_expression.

    READ TABLE tokens_qualified INTO token INDEX i_start_token_index.
    IF token-str = '|' AND token-type = sana_tok_word.
      start = token-col.
      source_line_index = token-row.
      tabix = i_start_token_index + 2.
      LOOP AT tokens_qualified INTO token FROM tabix
        WHERE str = '|' AND type = sana_tok_word.
        end = token-col + 1.
        e_next_token_index = sy-tabix + 1.
        EXIT.
      ENDLOOP.
    ENDIF.

    focused_include = me->blackboard->get_focused_include( ).
    source_code =  me->blackboard->get_scan_result( )->get_source_of_include( focused_include ).
*    source_line_index = me->blackboard->get_cursor_line( ).
    READ TABLE source_code INTO source_line INDEX source_line_index.
    e_string_expression = substring( val = source_line off = start len = end - start ).

  ENDMETHOD.


  METHOD extract_nested_method_call.

    DATA: current_token     TYPE stokesx,
          string_expression TYPE string,
          bracket_count     TYPE i,
          tabix             TYPE i.

    CLEAR: e_nested_call.

    tabix = i_start_token_index.

    WHILE tabix <= lines( tokens_qualified ).
      READ TABLE tokens_qualified INDEX tabix INTO current_token.

      IF current_token-type = sana_tok_word AND current_token-str = '|'.

        extract_string_expression( EXPORTING i_start_token_index = tabix IMPORTING e_next_token_index = tabix e_string_expression = string_expression ).

        IF e_nested_call IS INITIAL.
          e_nested_call = string_expression.
        ELSE.
          e_nested_call = |{ e_nested_call } { string_expression }|.
        ENDIF.

      ELSE.

        IF current_token-type = sana_tok_method.
          ADD 1 TO bracket_count.
        ENDIF.

        IF ( current_token-type = sana_tok_word AND current_token-str = ')' )
        OR ( ( current_token-type = sana_tok_method OR current_token-type = sana_tok_field ) AND current_token-str(1) = ')' ). " chained_call obj->( )->bla( ) ... )->bla( is one token with type sana_tok_method
          SUBTRACT 1 FROM bracket_count.
        ENDIF.

        IF e_nested_call IS INITIAL.
          e_nested_call = current_token-str.
        ELSE.
          e_nested_call = |{ e_nested_call } { current_token-str }|.
        ENDIF.

        IF bracket_count = 0.
          e_next_token_index = sy-tabix + 1.
          RETURN.
        ENDIF.

        ADD 1 TO tabix.

      ENDIF.

    ENDWHILE.

  ENDMETHOD.


  METHOD get_token_with_type_info.

    DATA: token_next_word TYPE stokesx,
          found_index     TYPE i.

    get_right_token( EXPORTING i_start_index = i_method_end i_type = sana_tok_word
                    IMPORTING e_token = token_next_word e_index = found_index ).

    CASE token_next_word-str.

      WHEN '=' OR '<>' OR '<' OR '>'.

        get_right_token( EXPORTING i_start_index = found_index i_type = sana_tok_field
                        IMPORTING e_token = r_result ).

        RETURN.

    ENDCASE.

    get_left_token( EXPORTING i_start_index = i_method_start i_type = sana_tok_word
                    IMPORTING e_token = token_next_word e_index = found_index ).

    CASE token_next_word-str.

      WHEN '=' OR '<>' OR '<' OR '>'.

        get_left_token( EXPORTING i_start_index = found_index i_type = sana_tok_field
                        IMPORTING e_token = r_result ).

        RETURN.

    ENDCASE.

  ENDMETHOD.


  METHOD get_left_token.

    DATA: token_left TYPE stokesx.

    READ TABLE tokens_qualified INTO token_left INDEX i_start_index - 1.

    IF token_left-type = i_type.
      e_token = token_left.
      e_index = i_start_index - 1.
    ELSEIF i_start_index > 1.
      get_left_token( EXPORTING i_start_index = i_start_index - 1 i_type = i_type
                      IMPORTING e_token = e_token e_index = e_index ).
    ENDIF.

  ENDMETHOD.

  METHOD get_right_token.

    DATA: token_left TYPE stokesx.

    READ TABLE tokens_qualified INTO token_left INDEX i_start_index + 1.

    IF token_left-type = i_type.
      e_token = token_left.
      e_index = i_start_index + 1.
    ELSEIF i_start_index < lines( tokens_qualified ).
      get_left_token( EXPORTING i_start_index = i_start_index + 1 i_type = i_type
                      IMPORTING e_token = e_token e_index = e_index ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
