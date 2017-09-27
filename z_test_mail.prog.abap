*&---------------------------------------------------------------------*
*& Report z_test_mail
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_mail.

CLASS bcs_wrapper DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        RAISING
          cx_send_req_bcs,

      set_document
        IMPORTING
          !i_document   TYPE REF TO if_document_bcs
        RETURNING
          VALUE(r_self) TYPE REF TO bcs_wrapper
        RAISING
          cx_send_req_bcs,

      add_recipient
        IMPORTING
          !i_recipient  TYPE REF TO if_recipient_bcs
          !i_express    TYPE os_boolean OPTIONAL
          !i_copy       TYPE os_boolean OPTIONAL
          !i_blind_copy TYPE os_boolean OPTIONAL
          !i_no_forward TYPE os_boolean OPTIONAL
        RETURNING
          VALUE(r_self) TYPE REF TO bcs_wrapper
        RAISING
          cx_send_req_bcs,

      set_send_immediately
        IMPORTING
          i_send_immediately TYPE os_boolean
        RETURNING
          VALUE(r_self)      TYPE REF TO bcs_wrapper
        RAISING
          cx_send_req_bcs,

      send
        IMPORTING
          !i_with_error_screen TYPE os_boolean DEFAULT space
        RETURNING
          VALUE(result)        TYPE os_boolean
        RAISING
          cx_send_req_bcs.

  PRIVATE SECTION.
    DATA: bcs TYPE REF TO cl_bcs.

ENDCLASS.

CLASS html_template DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor,

      render
        RETURNING
          VALUE(r_html) TYPE string.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_data,
             matnr TYPE matnr,
             text  TYPE maktx,
             datum TYPE datum,
             uname TYPE xubname,
           END OF ty_data,
           tty_data TYPE STANDARD TABLE OF ty_data
                    WITH NON-UNIQUE DEFAULT KEY.

    DATA: html_template         TYPE string,
          lt_data               TYPE html_template=>tty_data,
          table_row_template    TYPE string,
          table_cell_template   TYPE string,
          table_header_template TYPE string,
          header_cell_template  TYPE string.

    METHODS:
      _render
        RETURNING
          VALUE(r_html) TYPE string.

ENDCLASS.


CLASS bcs_wrapper IMPLEMENTATION.

  METHOD constructor.

    bcs = cl_bcs=>create_persistent( ).

  ENDMETHOD.

  METHOD add_recipient.

    bcs->add_recipient(
      EXPORTING
        i_recipient     = i_recipient     " Empfänger einer Nachricht
        i_express       = i_express       " Senden als Expressnachricht
        i_copy          = i_copy          " Senden in Kopie
        i_blind_copy    = i_blind_copy    " Senden als geheime Kopie
        i_no_forward    = i_no_forward    " Weiterleiten verboten
    ).

    r_self = me.

  ENDMETHOD.

  METHOD send.

    result = bcs->send( ).

  ENDMETHOD.

  METHOD set_document.

    bcs->set_document( i_document ).

    r_self = me.

  ENDMETHOD.

  METHOD set_send_immediately.

    bcs->set_send_immediately( i_send_immediately ).

    r_self = me.

  ENDMETHOD.

ENDCLASS.

CLASS html_template IMPLEMENTATION.

  METHOD constructor.

    html_template =
      `<style type="text/css">` &&
      `.tg  {border-collapse:collapse;border-spacing:0;}` &&
      `.tg td{font-family:Arial, sans-serif;font-size:14px;padding:10px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;}` &&
      `.tg th{font-family:Arial, sans-serif;font-size:14px;font-weight:normal;padding:10px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;}` &&
      `.tg .tg-le8v{background-color:#c0c0c0;vertical-align:top}` &&
      `.tg .tg-yw4l{vertical-align:top}` &&
      `</style>` &&
      `<table class="tg">` &&
      `{TABLE_HEADER}` &&
      `{TABLE_ROW}` &&
      `</table>`.

    table_header_template =
      `  <tr>` &&
      `   {HEADER}` &&
      `  </tr>`.

    header_cell_template =
      `    <th class="tg-le8v">{CELL}</th>`.

    table_row_template =
      `  <tr>` &&
      `   {TABLE_CELL}`  &&
      `  </tr>`.

    table_cell_template = `<td class="tg-yw4l">{CELL}</td>`.

    lt_data = VALUE tty_data( ( matnr = '01800180'
                                text  = 'ibox'
                                datum = sy-datum
                                uname = sy-uname )
                              ( matnr = '12345678'
                                text  = 'Test'
                                datum = sy-datum + 10
                                uname = 'RUFALEXA' )
                              ( matnr = '31700000'
                                text  = 'Test 2'
                                datum = sy-datum + 15
                                uname = 'HARTUNGJ' ) ).

  ENDMETHOD.

  METHOD render.

    r_html = _render( ).

  ENDMETHOD.


  METHOD _render.

    DATA: ls_data          TYPE html_template=>ty_data,
          table_row        LIKE table_row_template,
          table_row_result LIKE table_row_template,
          table_cell       TYPE string,
          table_cells      TYPE string,
          header           TYPE string,
          header_cell      TYPE string,
          header_row       TYPE string.

    DATA(components) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( ls_data ) )->get_components( ).

    header = table_header_template.

    r_html = html_template.

    LOOP AT components ASSIGNING FIELD-SYMBOL(<component>).

      header_cell = header_cell_template.

      cl_reca_ddic_dtel=>get_text(
        EXPORTING
          id_name      = <component>-name    " Name des Datenelementes
        IMPORTING
          ed_reptext   = DATA(text)    " Überschrift
        EXCEPTIONS
          not_found    = 1
          OTHERS       = 2 ).

      REPLACE ALL OCCURRENCES OF `{CELL}`
              IN header_cell
              WITH text.

      header_row = header_row && header_cell.

    ENDLOOP.

    REPLACE ALL OCCURRENCES OF `{HEADER}`
            IN header
            WITH header_row.

    REPLACE ALL OCCURRENCES OF `{TABLE_HEADER}`
            IN r_html
            WITH header.

    LOOP AT lt_data INTO ls_data.

      CLEAR: table_cells.

      table_row = table_row_template.

      LOOP AT components ASSIGNING <component>.

        table_cell = table_cell_template.

        ASSIGN COMPONENT <component>-name
               OF STRUCTURE ls_data
               TO FIELD-SYMBOL(<field>).
        ASSERT sy-subrc = 0.

        REPLACE ALL OCCURRENCES OF |\{CELL\}|
                IN table_cell
                WITH <field>.

        table_cells = table_cells && table_cell.

      ENDLOOP.

      REPLACE ALL OCCURRENCES OF |\{TABLE_CELL\}|
              IN table_row
              WITH table_cells.

      table_row_result = table_row_result && table_row.

    ENDLOOP.


    REPLACE ALL OCCURRENCES OF |\{TABLE_ROW\}|
            IN r_html
            WITH table_row_result.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  TRY.

      NEW bcs_wrapper(
            )->set_document( cl_document_bcs=>create_document( i_type    = 'HTM'    " Kürzel für den Dokumenttyp
                                                               i_subject = 'Test Mail'
                                                               i_text    = cl_bcs_convert=>string_to_soli( NEW html_template( )->render( ) ) )
            )->add_recipient( cl_cam_address_bcs=>create_internet_address( 'christianguenter@googlemail.com' )
            )->add_recipient( cl_sapuser_bcs=>create( i_user = sy-uname )
            )->set_send_immediately( abap_true
            )->send(  ).

      COMMIT WORK.

    CATCH cx_send_req_bcs INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
