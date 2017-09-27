REPORT z_code_lighter2.

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
  cl_sedi_adt_html_renderer=>create_instance( )->render_source(
    EXPORTING
      source            = gt_rswsourcet
    IMPORTING
      html_source       = gt_rswsourcet
  ).
  gt_code = gt_rswsourcet.

  "each code line is a paragraph in html with fixed width font
  LOOP AT gt_code ASSIGNING <gs_code>.
    CONCATENATE '<span style="font-family: courier new,courier;">' <gs_code> '</span><br/>' INTO <gs_code>.
  ENDLOOP.

  "wrap root node to create valid xml
  "it will also insert quote in jive text box
  INSERT '<pre class="jive_text_macro jive_macro_quote" jivemacro="quote"><p>' INTO gt_code INDEX 1.
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
