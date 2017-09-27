*&---------------------------------------------------------------------*
*& Report  Z_TEST_BCS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_bcs.

DATA: lo_bcs      TYPE REF TO cl_bcs,
      lo_document TYPE REF TO cl_document_bcs,
      lt_content  TYPE soli_tab.

lo_bcs = cl_bcs=>create_persistent( ).

lo_bcs->add_recipient( cl_cam_address_bcs=>create_internet_address( i_address_string = 'chguenter@web.de' ) ).
lo_bcs->add_recipient( cl_sapuser_bcs=>create( i_user = 'GUENTERC' ) ).

lo_document = cl_document_bcs=>create_document( i_type    = 'HTM'
                                                i_subject = 'Test'
                                                i_text    = cl_bcs_convert=>string_to_soli( 'Dies ist ein Test <b>mit fettem Tag</b> und einem <br>Zeilenumbruch' )
                                                i_objnam  = 'HG_100000150' ).

DATA: shortcut_string TYPE string,
      lv_parameter    TYPE text255.

CONCATENATE 'OBJECT_TYPE=ZEHSAPP; OBJECT_KEY=' '10000300' '; DYNP_OKCODE=ONLI'
                INTO lv_parameter.

CALL FUNCTION 'SWN_CREATE_SHORTCUT'
  EXPORTING
    i_transaction   = '*SPO1'
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

lo_document->add_attachment(
  EXPORTING
    i_attachment_type     = 'SAP'    " Document Class for Attachment
    i_attachment_subject  = 'Zulassung'    " Attachment Title
*    i_attachment_size     = i_attachment_size    " Size of Document Content
*    i_attachment_language = SPACE    " Language in Which Attachment Is Created
    i_att_content_text    = cl_bcs_convert=>string_to_soli( shortcut_string )    " Content (Textual)
*    i_att_content_hex     = i_att_content_hex    " Content (Binary)
*    i_attachment_header   = i_attachment_header    " Attachment Header Data
).
*  CATCH cx_document_bcs.    " BCS: Document Exceptions

lo_bcs->set_document( lo_document ).

DATA: borident TYPE borident.

borident-objtype = 'ZEHSAPP'.
borident-objkey  = '10000542'.

lo_bcs->create_link( borident ).

*DATA: sibflpor TYPE sibflpor.
*
*sibflpor-objtype = 'ZEHSAPP'.
*sibflpor-instid  = '10000542'.
*sibflpor-catid   = 'BO'.
*
*lo_bcs->create_link_to_app( sibflpor ).

lo_bcs->send( ).

COMMIT WORK.
