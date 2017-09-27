*&---------------------------------------------------------------------*
*& Report  Z_TEST_TANGRO_LINK_MAIL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_tangro_link_mail.

PARAMETER: p_doc   TYPE /ssc/docid OBLIGATORY DEFAULT '100000018880',
           p_email TYPE string OBLIGATORY DEFAULT 'christian.guenter@hansgrohe.com',
           p_user  TYPE uname DEFAULT sy-uname.

START-OF-SELECTION.
  IF zcl_tangro_mailer=>send_document_link( i_doc_id   = p_doc
                                            i_email    = p_email
                                            i_sap_user = p_user
                                            i_commit   = abap_true ) = abap_true.
    MESSAGE 'Nachricht versendet'(001) TYPE 'S'.
  ELSE.
    MESSAGE 'Nachricht nicht versendet'(002) TYPE 'S'
                                             DISPLAY LIKE 'E'.
  ENDIF.
