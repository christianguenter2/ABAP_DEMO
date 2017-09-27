*&---------------------------------------------------------------------*
*& Report  Z_TEST_HTML_VIEWER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_html_viewer.

DATA: go_cust_container TYPE REF TO cl_gui_custom_container,
      go_viewer         TYPE REF TO cl_gui_html_viewer.

DATA: stxh  TYPE stxh,
      lines TYPE ty_tline.

START-OF-SELECTION.

  stxh-tdid     = 'ST'.
  stxh-tdspras  = 'D'.
  stxh-tdname   = 'Z_MAIL_INFO_APPROVAL'.
  stxh-tdobject = 'TEXT'.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = stxh-tdid    " Text-Id des zu lesenden Textes
      language                = stxh-tdspras    " Sprache des zu lesenden Textes
      name                    = stxh-tdname    " Name des zu lesenden Textes
      object                  = stxh-tdobject    " Objekt des zu lesenden Textes
    TABLES
      lines                   = lines    " Textzeilen des gelesenen Textes
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.



  CALL SCREEN 0100.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  IF go_cust_container IS NOT BOUND.
    CREATE OBJECT go_cust_container
      EXPORTING
*        parent                      = parent    " Parent container
        container_name              = 'CUST_CONTAINER'
*        style                       = style    " Windows style attributes applied to this container
*        lifetime                    = LIFETIME_DEFAULT    " Lifetime
*        repid                       = repid    " Dynpro to which this container is linked to
*        dynnr                       = dynnr    " Report to which this container is linked to
*        no_autodef_progid_dynnr     = no_autodef_progid_dynnr    " dont autodefine progid and dynnr?
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6
      .
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT go_viewer
      EXPORTING
*        shellstyle           = shellstyle    " Shell Style
        parent               = go_cust_container
*        lifetime             = LIFETIME_DEFAULT    " Lifetime
*        saphtmlp             = saphtmlp    " Aktiviert das Pluggable Protokoll
*        uiflag               = uiflag    " IE WebBrowser Control UI flag
*        name                 = name    " name
*        saphttp              = saphttp    " benutzt das HTTP Daten Service
*        query_table_disabled = ''    " ob QUERY_TABLE in SAPEVENT benutzt wird
      EXCEPTIONS
        cntl_error           = 1
        cntl_install_error   = 2
        dp_install_error     = 3
        dp_error             = 4
        OTHERS               = 5 .

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    DATA lv_url  TYPE c LENGTH 400.
    DATA lt_text TYPE STANDARD TABLE OF tdline.
    FIELD-SYMBOLS: <line> LIKE LINE OF lines.

    LOOP AT lines ASSIGNING <line>.
      APPEND <line>-tdline TO lt_text.
      APPEND cl_abap_char_utilities=>cr_lf TO lt_text.
    ENDLOOP.

    go_viewer->load_data(
*      EXPORTING
*        url                    = url    " URL
*        type                   = 'text'    " Typ von  MIME object
*        subtype                = 'html'    " Subtype von MIME object
*        size                   = 0    " Läange der Daten data
*        encoding               = encoding    " Encoding für MIME object
*        charset                = charset    " Encoding für MIME object
*        i_tidyt                = i_tidyt    " Nur für spezielle Aufrufe!
*        needfiltering          = 0    " Set to 1 if content needs to be filtered else it will be 0
*        language               = language
      IMPORTING
        assigned_url           = lv_url
      CHANGING
        data_table             = lt_text
*        iscontentchanged       = iscontentchanged    " sets to 1 if the content is filtered else it is 0.
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        OTHERS                 = 5
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    go_viewer->show_data(
      EXPORTING
        url                    = lv_url
*        frame                  = frame    " frame where the data should be shown
*        in_place               = 'X '    " wird die Dikument in GUI angezeigt?
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        OTHERS                 = 5
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
