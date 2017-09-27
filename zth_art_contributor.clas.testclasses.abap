DEFINE sample_code.
  add_sample_code( i_line_number = &1 i_code_before = &2 i_code_change = &3 ).
END-OF-DEFINITION.

CLASS ltc_expected_deltas DEFINITION FINAL INHERITING FROM zTH_ART_CONTRIBUTOR
  FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:

      replace_several_lines FOR TESTING,
      append FOR TESTING,
      prepend FOR TESTING,
      replace FOR TESTING RAISING cx_static_check,
      append_prepend FOR TESTING RAISING cx_static_check,
      replace_several_lines2 FOR TESTING RAISING cx_static_check,

      assert_expected_delta
        IMPORTING
          idx TYPE i
          pos TYPE string
          val TYPE string.

ENDCLASS.


CLASS ltc_expected_deltas IMPLEMENTATION.

  METHOD append.

    sample_code:

      1 `REPORT dummy.`     ``,

      2 `  PUBLIC SECTION.`       `append: "public`.


    assert_equals( act = lines( me->exp_quickfix_deltas ) exp = 1 ).

    assert_expected_delta( idx = 1 pos = '2,17' val = ' "public' ).

  ENDMETHOD.

  METHOD prepend.

    sample_code:

      1 `REPORT dummy.`     ``,

      2 `  var = 17.`       `prepend:DATA var TYPE i. `.


    assert_equals( act = lines( me->exp_quickfix_deltas ) exp = 1 ).

    assert_expected_delta( idx = 1 pos = '2,2' val = |DATA var TYPE i. | ).

  ENDMETHOD.

  METHOD append_prepend.

    sample_code:

      1 `REPORT dummy.`     ``,

      2 `  RETURN.`       `prepend:IF. ;append: ENDIF.`.


    assert_equals( act = lines( me->exp_quickfix_deltas ) exp = 2 ).

    assert_expected_delta( idx = 1 pos = '2,2' val = |IF. | ).
    assert_expected_delta( idx = 2 pos = '2,9' val = | ENDIF.| ).

  ENDMETHOD.

  METHOD replace.

    sample_code:

      1 `REPORT dummy.`      ``,

      2 `  DATA(var) = 17.`  `replace:DATA(var) ~> var`.


    assert_equals( act = lines( me->exp_quickfix_deltas ) exp = 1 ).

    assert_expected_delta( idx = 1 pos = '2,2-2,11' val = |var| ).

  ENDMETHOD.

  METHOD replace_several_lines.

    sample_code:

      1 `REPORT dummy.`     ``,

      2 `  IF 1 = 1.`       `replace_starts_before:`,
      3 `    ...`           ``,
      4 `  ENDIF.`          `replace_ends_after:hello anna`,
      5 `...`               ``.

    assert_equals( act = lines( me->exp_quickfix_deltas ) exp = 1 ).

    assert_expected_delta( idx = 1 pos = '2,2-4,8' val = 'hello anna' ).

  ENDMETHOD.

  METHOD replace_several_lines2.

    sample_code:

      1 `REPORT dummy.`     ``,

      2 `  TRY.`                    ``,
      3 `    CATCH cx_error.`       `replace_starts_before:`,
      4 `    ...`                   ``,
      5 `  ENDTRY.`                 `replace_ends_before:test`,
      6 `...`                       ``.

    assert_equals( act = lines( me->exp_quickfix_deltas ) exp = 1 ).

    assert_expected_delta( idx = 1 pos = '3,4-5,2' val = 'test' ).

  ENDMETHOD.

  METHOD assert_expected_delta.

    DATA: exp_delta TYPE ty_exp_delta,
          first     TYPE string,
          dummy     TYPE string ##needed,
          row       TYPE i.

    READ TABLE me->exp_quickfix_deltas INDEX idx INTO exp_delta.

    SPLIT pos AT ',' INTO first dummy.
    row = first.

    assert_equals( act = exp_delta-line_number exp = row ).
    assert_equals( act = exp_delta-new_content exp = to_lower( val ) ).
    assert_equals( act = exp_delta-source_position_string exp = pos ).

  ENDMETHOD.

ENDCLASS.



CLASS ltc_proposals DEFINITION FINAL INHERITING FROM zTH_ART_CONTRIBUTOR
  FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA: proposal TYPE REF TO cl_art_proposal.

    METHODS:

      setup,

      without_user_content FOR TESTING,
      with_user_content FOR TESTING RAISING cx_static_check,
      same_qfix_twice FOR TESTING RAISING cx_static_check,

      add_proposal_to_blackboard
        IMPORTING
          qfix type ref to ce_art_qfix
          user_content type string OPTIONAL.

ENDCLASS.

CLASS ltc_proposals IMPLEMENTATION.

  METHOD setup.

    sample_code: 1 `REPORT dummy.`     ``.
    prepare_blackboard( '1,1' ).

  ENDMETHOD.

  METHOD without_user_content.

    add_proposal_to_blackboard( qfix = ce_art_qfix=>declare_raising ).

    assert_proposed( ce_art_qfix=>declare_raising ).

  ENDMETHOD.

  METHOD with_user_content.

    add_proposal_to_blackboard( qfix = ce_art_qfix=>declare_raising user_content = 'A' ).

    proposal = assert_proposed( qfix = ce_art_qfix=>declare_raising user_content = 'A' ).
    assert_bound( proposal ).

    proposal = assert_proposed( ce_art_qfix=>declare_raising ).
    assert_bound( proposal ).

  ENDMETHOD.

  METHOD same_qfix_twice.

    add_proposal_to_blackboard( qfix = ce_art_qfix=>declare_raising user_content = 'A' ).
    add_proposal_to_blackboard( qfix = ce_art_qfix=>declare_raising user_content = 'B' ).

    assert_proposed( qfix = ce_art_qfix=>declare_raising user_content = 'A' ).
    assert_proposed( ce_art_qfix=>declare_raising ).

    assert_proposed( qfix = ce_art_qfix=>declare_raising user_content = 'B' ).
    assert_proposed( ce_art_qfix=>declare_raising ).

  ENDMETHOD.

  METHOD add_proposal_to_blackboard.

    data: new_proposal type ref to cl_art_proposal.

    new_proposal = qfix->create_proposal( ).
    new_proposal->set_user_content( user_content ).
    me->blackboard->add_proposal( new_proposal ).

  ENDMETHOD.

ENDCLASS.
