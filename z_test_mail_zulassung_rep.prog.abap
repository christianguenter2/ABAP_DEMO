*&---------------------------------------------------------------------*
*& Report  Z_TEST_MAIL_ZULASSUNG_REP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_mail_zulassung_rep.

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: start.

  PRIVATE SECTION.
    CLASS-METHODS: _add_shortcut CHANGING co_document TYPE REF TO cl_document_bcs.
ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD start.
    DATA: lo_bcs      TYPE REF TO cl_bcs,
          lo_document TYPE REF TO cl_document_bcs.

    lo_bcs = cl_bcs=>create_persistent( ).

    lo_document = cl_document_bcs=>create_document(
        i_type        = 'HTM'
        i_subject     = 'Test'
        i_text        = cl_bcs_convert=>string_to_soli( `Dies <b> ist ein Test</b>` ) ).

    _add_shortcut( CHANGING co_document = lo_document ).

    lo_bcs->set_document( lo_document ).

    lo_bcs->add_recipient( cl_cam_address_bcs=>create_internet_address( 'christian.guenter@hansgrohe.com' ) ).

    lo_bcs->send( ).

    COMMIT WORK.
  ENDMETHOD.                    "start

  METHOD _add_shortcut.
    DATA: shortcut_string TYPE string,
          lv_parameter    TYPE text255.

    lv_parameter = 'S_MATNR-LOW=01800180;S_LABOR-LOW=002;DYNP_OKCODE=ONLI'.

    CALL FUNCTION 'SWN_CREATE_SHORTCUT'
      EXPORTING
        i_transaction   = '*ZEHSAPP04'
        i_user          = ''
        i_title         = ''
        i_windowsize    = 'M'
        i_parameter     = lv_parameter
      IMPORTING
        shortcut_string = shortcut_string.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                DISPLAY LIKE sy-msgty.
    ENDIF.

    co_document->add_attachment(
      EXPORTING
        i_attachment_type     = 'SAP'    " Document Class for Attachment
        i_attachment_subject  = 'Zulassung'    " Attachment Title
        i_att_content_text    = cl_bcs_convert=>string_to_soli( shortcut_string ) ).
  ENDMETHOD.                    "_add_shortcut
ENDCLASS.                    "lcl_application IMPLEMENTATION

START-OF-SELECTION.
  lcl_application=>start( ).
