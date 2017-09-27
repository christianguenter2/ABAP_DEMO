REPORT z_code_lighter3.
*----------------------------------------------------------------------*
*       CLASS LCL_SEDI_ADT_HTML_RENDERER DEFINITION
*----------------------------------------------------------------------*
* Copied from CL_SEDI_ADT_HTML_RENDERER which is missing in 702 release
*----------------------------------------------------------------------*
CLASS lcl_sedi_adt_html_renderer DEFINITION
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    "! Creates an instance of the renderer
    "!
    "! @parameter result | Instance of the renderer
    CLASS-METHODS create_instance
      RETURNING
        VALUE(result) TYPE REF TO lcl_sedi_adt_html_renderer .
    "! Renders ABAP source code as HTML snippet and sets a CSS class for the following
    "! artifacts to allow syntax coloring
    "! <ul>
    "!   <li>keywords</li>
    "!   <li>comments</li>
    "!   <li>literals</li>
    "!   <li>numbers</li>
    "! </ul>
    "!
    "! @parameter source            | source code that shall be rendered as HTML
    "! @parameter css_class_keyword | CSS class representing an ABAP keyword
    "! @parameter css_class_comment | CSS class representing an ABAP comment
    "! @parameter css_class_literal | CSS class representing a literal in ABAP coding
    "! @parameter css_class_number  | CSS class representing a number in ABAP coding
    "! @parameter html_source       | source rendered as HTML snippet
    METHODS render_source
      IMPORTING
        !source            TYPE rswsourcet
        !css_class_keyword TYPE string DEFAULT 'keyword'
        !css_class_comment TYPE string DEFAULT 'comment'
        !css_class_literal TYPE string DEFAULT 'literal'
        !css_class_number  TYPE string DEFAULT 'number'
          RETURNING
          !value(html_source) TYPE rswsourcet .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.                    "LCL_SEDI_ADT_HTML_RENDERER DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_SEDI_ADT_HTML_RENDERER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sedi_adt_html_renderer IMPLEMENTATION.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method LCL_SEDI_ADT_HTML_RENDERER=>CREATE_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RESULT                         TYPE REF TO LCL_SEDI_ADT_HTML_RENDERER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_instance.
    CREATE OBJECT result.
  ENDMETHOD.                    "CREATE_INSTANCE

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method LCL_SEDI_ADT_HTML_RENDERER->RENDER_SOURCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] SOURCE                         TYPE        RSWSOURCET
* | [--->] CSS_CLASS_KEYWORD              TYPE        STRING (default ='keyword')
* | [--->] CSS_CLASS_COMMENT              TYPE        STRING (default ='comment')
* | [--->] CSS_CLASS_LITERAL              TYPE        STRING (default ='literal')
* | [--->] CSS_CLASS_NUMBER               TYPE        STRING (default ='number')
* | [<---] HTML_SOURCE                    TYPE        RSWSOURCET
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD render_source.
    CONSTANTS:
      "! regex to find numbers
      co_regex_numbers               TYPE string VALUE '[[:digit:]]+' ##no_text,
      "! regex to match inline declarations: DATA(ABC)
      co_regex_inline_decl_data      TYPE string VALUE 'DATA\([^\)[:space:]]+\)' ##no_text,
      "! regex to match inline declarations: FIELD-SYMBOL(<ABC>)
      co_regex_inline_decl_field_sym TYPE string VALUE 'FIELD\-SYMBOL\(<[^\)[:space:]]+>\)' ##no_text.
    DATA:
      statements                    TYPE STANDARD TABLE OF sstmnt,
      tokens                        TYPE sana_stokesx_tab,
      offset_end_of_token           TYPE i,
      "! offset if a token must be splitted (e.g. token for inline declaration DATA(ABC))
      offset_to_split_token         TYPE i,
      "! length of second part of a splitted token
      length_rest_of_splitted_token TYPE i,
      in_string_template            TYPE abap_bool VALUE abap_false.
    FIELD-SYMBOLS:
      <statement>   LIKE LINE OF statements,
      <token>       LIKE LINE OF tokens,
      <source_line> LIKE LINE OF source.

    SCAN ABAP-SOURCE source STATEMENTS INTO statements TOKENS INTO tokens WITH ANALYSIS WITH COMMENTS.
    html_source = source.
    LOOP AT statements ASSIGNING <statement> WHERE type NA 'MBE'.  "ignore (E)XEC-SQL-statements
      CALL FUNCTION 'RS_QUALIFY_ABAP_TOKENS_STR'
        EXPORTING
          statement_type = <statement>-type
          index_from     = <statement>-from
          index_to       = <statement>-to
        CHANGING
          stokesx_tab    = tokens
        EXCEPTIONS
          OTHERS         = 5.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDLOOP.
    SORT tokens BY row DESCENDING col DESCENDING.
    DELETE ADJACENT DUPLICATES FROM tokens. "to remove duplicate entries for colon-comma-semantics.
    LOOP AT tokens ASSIGNING <token>.
      TRY.
          READ TABLE html_source INDEX <token>-row ASSIGNING <source_line>.
          IF ( sy-subrc = 0 ).
            offset_end_of_token = <token>-col + <token>-len1.
            IF ( <token>-len1 > 0 ).
              CASE <token>-type.
                WHEN sana_tok_keyword OR sana_tok_word.
                  <source_line> =
                    <source_line>(<token>-col) &&
                    |<span class="{ css_class_keyword }">| &&
                    escape(
                      val = <source_line>+<token>-col(<token>-len1)
                      format = cl_abap_format=>e_html_text ) &&
                    '</span>' && <source_line>+offset_end_of_token.
                  IF ( <token>-str = '|' ).
                    in_string_template = boolc( in_string_template = abap_false ).
                  ENDIF.
                WHEN sana_tok_field.
                  IF in_string_template = abap_true AND <token>-len1 > 1.
                    <token>-len1 = <token>-len1 - 2 + count( val = <token>-str regex = '[\\{}|]' ).
                    offset_end_of_token = <token>-col + <token>-len1.
                  ENDIF.
                  IF ( ( <token>-str(1) = '''' ) OR ( <token>-str(1) =  '`' ) ). "color literals
                    <source_line> =
                      <source_line>(<token>-col) &&
                      |<span class="{ css_class_literal }">| &&
                      escape(
                        val = <source_line>+<token>-col(<token>-len1)
                        format = cl_abap_format=>e_html_text ) &&
                      '</span>' && <source_line>+offset_end_of_token.
                  ELSEIF ( matches( val = <token>-str regex = co_regex_numbers ) ).
                    <source_line> =
                      <source_line>(<token>-col) &&
                      |<span class="{ css_class_number }">| &&
                      escape(
                        val = <source_line>+<token>-col(<token>-len1)
                        format = cl_abap_format=>e_html_text ) &&
                      '</span>' && <source_line>+offset_end_of_token.
                  ELSEIF ( matches( val = <token>-str regex = co_regex_inline_decl_data ) ).
                    " handle inline declaration DATA(abc) ...
                    offset_end_of_token           = <token>-col + strlen( <token>-str ).
                    offset_to_split_token         = <token>-col + 4.
                    length_rest_of_splitted_token = strlen( <token>-str ) - 4.
                    <source_line> =
                       <source_line>(<token>-col) &&
                       |<span class="{ css_class_keyword }">| &&
                       escape(
                         val = <source_line>+<token>-col(4)
                         format = cl_abap_format=>e_html_text ) &&
                       '</span>' &&
                       escape(
                         val = <source_line>+offset_to_split_token(length_rest_of_splitted_token)
                         format = cl_abap_format=>e_html_text ) &&
                    <source_line>+offset_end_of_token.
                  ELSE.
                    " escape each sub-token separately
                    DATA: lv_off TYPE i,
                          lv_len TYPE i.
                    lv_off = <token>-col + <token>-off3.
                    lv_len = <token>-len3.
                    <source_line> = replace( val = <source_line>  off = lv_off  len = lv_len
                                             with = escape(
                                               val = <source_line>+lv_off(lv_len)
                                               format = cl_abap_format=>e_html_text ) ) .
                    lv_off = <token>-col + <token>-off2.
                    lv_len = <token>-len2.
                    <source_line> = replace( val = <source_line>  off = lv_off  len = lv_len
                                             with = escape(
                                               val = <source_line>+lv_off(lv_len)
                                               format = cl_abap_format=>e_html_text ) ) .
                    lv_off = <token>-col.
                    lv_len = <token>-len1.
                    <source_line> = replace( val = <source_line>  off = lv_off  len = lv_len
                                             with = escape(
                                                val = <source_line>+lv_off(lv_len)
                                                format = cl_abap_format=>e_html_text ) ) .
                  ENDIF.
                WHEN sana_tok_field_def.
                  IF ( matches( val = <token>-str regex = co_regex_inline_decl_field_sym ) ).
                    "handle inline declaration field-symbol(abc) ...
                    offset_end_of_token           = <token>-col + strlen( <token>-str ).
                    offset_to_split_token         = <token>-col + 12.
                    length_rest_of_splitted_token = strlen( <token>-str ) - 12.
                    <source_line> =
                      <source_line>(<token>-col) &&
                      |<span class="{ css_class_keyword }">| &&
                      escape(
                        val = <source_line>+<token>-col(12)
                        format = cl_abap_format=>e_html_text ) &&
                      '</span>' &&
                      escape(
                        val = <source_line>+offset_to_split_token(length_rest_of_splitted_token)
                        format = cl_abap_format=>e_html_text ) &&
                      <source_line>+offset_end_of_token.
                  ELSE.
                    <source_line> =
                      <source_line>(<token>-col) &&
                      escape(
                        val = <source_line>+<token>-col(<token>-len1)
                        format = cl_abap_format=>e_html_text ) &&
                        <source_line>+offset_end_of_token.
                  ENDIF.
                WHEN 'C'. "comment
                  <source_line> =
                    <source_line>(<token>-col) &&
                    |<span class="{ css_class_comment }">| &&
                    escape(
                       val = <source_line>+<token>-col
                      format = cl_abap_format=>e_html_text ) &&
                    '</span>'.
                WHEN OTHERS.
                  <source_line> =
                    <source_line>(<token>-col) &&
                    escape(
                      val = <source_line>+<token>-col(<token>-len1)
                      format = cl_abap_format=>e_html_text ) &&
                    <source_line>+offset_end_of_token.
              ENDCASE.
            ELSE.
              <source_line> =
                <source_line>(<token>-col) &&
                escape(
                  val = <source_line>+<token>-col(<token>-len1)
                  format = cl_abap_format=>e_html_text ) &&
                  <source_line>+offset_end_of_token.
            ENDIF.
          ENDIF.
        CATCH cx_sy_range_out_of_bounds.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.                    "RENDER_SOURCE
ENDCLASS.                    "LCL_SEDI_ADT_HTML_RENDERER IMPLEMENTATION

DATA:
  go_dock        TYPE REF TO cl_gui_docking_container,
  go_container_1 TYPE REF TO cl_gui_container,
  go_editor_1    TYPE REF TO cl_gui_textedit,
  go_container_2 TYPE REF TO cl_gui_container,
  go_editor_2    TYPE REF TO cl_gui_textedit,
  gt_code        TYPE STANDARD TABLE OF text1024.

SELECTION-SCREEN BEGIN OF SCREEN 2000.
SELECTION-SCREEN END OF SCREEN 2000.

START-OF-SELECTION.

  CALL SELECTION-SCREEN 2000.

AT SELECTION-SCREEN.
  FIELD-SYMBOLS <gs_code> LIKE LINE OF gt_code.
  DATA gv_xml         TYPE string.
  DATA go_root        TYPE REF TO cx_root.
  DATA gt_rswsourcet  TYPE rswsourcet.

  REFRESH gt_code.
  CALL METHOD go_editor_1->get_text_as_r3table
    IMPORTING
      table = gt_code.

  "below method call will put css styles at correct positions of code
  gt_rswsourcet = gt_code.
  gt_rswsourcet = lcl_sedi_adt_html_renderer=>create_instance( )->render_source(
      source = gt_rswsourcet
  ).
  gt_code = gt_rswsourcet.

  "each code line is a paragraph in html with fixed width font
  LOOP AT gt_code ASSIGNING <gs_code>.
    CONCATENATE
      '<span style="font-family: courier new,courier; font-size: 8pt;">'
      <gs_code> '</span><br/>'
      INTO <gs_code>.
  ENDLOOP.

  "wrap root node to create valid xml
  "it will also insert quote in jive text box
  INSERT '<pre class="jive_text_macro jive_macro_quote" jivemacro="quote"><p>'
    INTO gt_code INDEX 1.
  APPEND '</p></pre>' TO gt_code.

  "convert source table to string and do xslt transformation
  "the transformation will do css inlining
  "resulting code will have css style as well as inline style
  "it is also possible be remove css completely but doing so will reduce readability
  gv_xml = concat_lines_of( gt_code ).
  TRY .
      CALL TRANSFORMATION z_code_lighter2
        SOURCE XML gv_xml
        RESULT XML gt_code.
    CATCH cx_root INTO go_root.
      gv_xml = go_root->get_text( ).  "view exception text in debug
  ENDTRY.

  CALL METHOD go_editor_2->set_text_as_stream
    EXPORTING
      text            = gt_code
    EXCEPTIONS
      error_dp        = 1
      error_dp_create = 2
      OTHERS          = 3.

  CALL SELECTION-SCREEN 2000.

AT SELECTION-SCREEN OUTPUT.
  CHECK go_dock IS NOT BOUND.
  CREATE OBJECT go_dock
    EXPORTING
      parent = cl_gui_container=>screen0
      side   = cl_gui_docking_container=>dock_at_left
      ratio  = 90.

  DATA go_splitter TYPE REF TO cl_gui_splitter_container.

  CREATE OBJECT go_splitter
    EXPORTING
      parent  = go_dock
      rows    = 1
      columns = 2.

  CHECK go_container_1 IS NOT BOUND.
  CALL METHOD go_splitter->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = go_container_1.

  CHECK go_editor_1 IS NOT BOUND.

  CREATE OBJECT go_editor_1
    EXPORTING
      parent                     = go_container_1
      wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
      wordwrap_position          = 1024
      wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

  CHECK go_container_2 IS NOT BOUND.
  CALL METHOD go_splitter->get_container
    EXPORTING
      row       = 1
      column    = 2
    RECEIVING
      container = go_container_2.

  CHECK go_container_2 IS BOUND.
  CHECK go_editor_2 IS NOT BOUND.

  CREATE OBJECT go_editor_2
    EXPORTING
      parent                     = go_container_2
      wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
      wordwrap_position          = 1024
      wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

  IF go_editor_2 IS BOUND.
    CALL METHOD go_editor_2->set_readonly_mode.
  ENDIF.
