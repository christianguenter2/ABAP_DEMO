*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lth_4_lcl_method_call DEFINITION DEFERRED.
CLASS lcl_method_call DEFINITION FINAL FRIENDS lth_4_lcl_method_call.
  PUBLIC SECTION.
    CONSTANTS co_arrow              TYPE c LENGTH 2 VALUE '->' ##no_text.
    CONSTANTS co_arrow_static       TYPE c LENGTH 2 VALUE '=>' ##no_text.
    CONSTANTS co_returning_default  TYPE string VALUE 'R_RESULT' ##no_text.
    METHODS get_method_name
      RETURNING VALUE(r_result) TYPE string.
    METHODS is_static
      RETURNING VALUE(r_result) TYPE abap_bool.
    METHODS get_clif_name
      RETURNING VALUE(r_result) TYPE string.
    METHODS get_call_reference
      IMPORTING i_token_string TYPE string
      RETURNING VALUE(r_value) TYPE string.

    METHODS parse_signature
      CHANGING method_descr TYPE if_rfac_impl_types=>ts_method..
    METHODS constructor
      IMPORTING
        i_tokens_qualified   TYPE if_ris_adt_source_handler=>ty_t_token
        i_tokens_unqualified TYPE if_ris_adt_source_handler=>ty_t_token
        i_blackboard         TYPE REF TO cl_art_blackboard.
  PRIVATE SECTION.
    DATA: blackboard                TYPE REF TO cl_art_blackboard.
    DATA: tokens_qualified          TYPE if_ris_adt_source_handler=>ty_t_token.
    DATA: tokens_unqualified        TYPE if_ris_adt_source_handler=>ty_t_token.
    DATA: method_descr              TYPE if_rfac_impl_types=>ts_method.
    DATA: last_token_of_method      TYPE i.

    METHODS no_closing_tag
      RETURNING VALUE(r_result) TYPE abap_bool.

    METHODS find_method_selector
      IMPORTING i_token_string    TYPE string
      RETURNING VALUE(r_position) TYPE i.

    METHODS validate_method_name
      CHANGING method_name TYPE string.

    METHODS keyword_receiving
      CHANGING c_token_tabix TYPE i.

    METHODS keyword_changing
      CHANGING c_token_tabix TYPE i.

    METHODS keyword_importing
      CHANGING c_token_tabix TYPE i.

    METHODS keyword_exporting
      CHANGING c_token_tabix TYPE i.

    METHODS:
      exporting_without_keyword
        IMPORTING i_start_index TYPE i.

    METHODS fill_method_parameter
      IMPORTING VALUE(i_equals_tabix) TYPE i
      EXPORTING e_next_token_index    TYPE i
      CHANGING  c_parameters          TYPE if_rfac_impl_types=>tt_params.

    METHODS extract_actual_parameter
      IMPORTING i_start_token_index TYPE i
      EXPORTING e_actual            TYPE string
                e_next_token_index  TYPE i.

    METHODS get_parameter_type
      CHANGING c_parameter TYPE if_rfac_impl_types=>ts_param.

    METHODS get_name_and_actual_frm_tokens
      IMPORTING i_equals_tabix     TYPE i
      EXPORTING e_name             TYPE string
                e_actual           TYPE string
                e_next_token_index TYPE i.

    METHODS returning_in_move
      IMPORTING i_from TYPE i
                i_to   TYPE i.
    METHODS fill_returning
      IMPORTING i_name             TYPE string DEFAULT co_returning_default
                i_actual           TYPE string OPTIONAL
                i_flg_chained_call TYPE abap_bool OPTIONAL.
    METHODS parameter_type_default
      CHANGING c_parameter TYPE if_rfac_impl_types=>ts_param.

    METHODS parameter_type_i
      CHANGING c_parameter TYPE if_rfac_impl_types=>ts_param.

    METHODS parameter_type_string
      CHANGING c_parameter TYPE if_rfac_impl_types=>ts_param.

    METHODS parameter_type_me
      CHANGING c_parameter TYPE if_rfac_impl_types=>ts_param.

    METHODS change_name_of_field_symbol
      CHANGING c_field_name TYPE csequence.
    METHODS derive_parameter_name
      IMPORTING
        i_actual        TYPE csequence
      RETURNING
        VALUE(r_result) TYPE string.
    METHODS extract_string_expression
      IMPORTING i_start_token_index TYPE i
      EXPORTING
                e_next_token_index  TYPE i
                e_string_expression TYPE string.
    METHODS extract_nested_method_call
      IMPORTING
        i_start_token_index TYPE i
      EXPORTING
        e_next_token_index  TYPE i
        e_nested_call       TYPE string.
    METHODS find_closing_token_of_method
      IMPORTING
        i_token_index_method_name TYPE i
      RETURNING
        VALUE(r_result)           TYPE i.
    METHODS:
      ends_with_opening_bracket
        IMPORTING i_token_str     TYPE stokesx-str
        RETURNING VALUE(r_result) TYPE abap_bool,
      starts_with_closing_bracket
        IMPORTING i_token_str     TYPE stokesx-str
        RETURNING VALUE(r_result) TYPE abap_bool,
      ends_with_closing_bracket
        IMPORTING i_token_str     TYPE stokesx-str
        RETURNING VALUE(r_result) TYPE abap_bool,

      get_token_with_type_info
        IMPORTING
          i_method_start  TYPE i
          i_method_end    TYPE i
        RETURNING
          VALUE(r_result) TYPE stokesx,
      get_left_token
        IMPORTING
          i_start_index TYPE i
          i_type        TYPE stoken-type
        EXPORTING
          e_token       TYPE stokesx
          e_index       TYPE i,
      get_right_token
        IMPORTING
          i_start_index TYPE i
          i_type        TYPE stoken-type
        EXPORTING
          e_token       TYPE stokesx
          e_index       TYPE i.

ENDCLASS.
