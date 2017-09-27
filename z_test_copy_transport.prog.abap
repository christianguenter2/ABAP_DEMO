*&---------------------------------------------------------------------*
*& Report  Z_TEST_COPY_TRANSPORT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_copy_transport.

PARAMETERS: p_trkorr TYPE trkorr.

CALL FUNCTION 'TRINT_TDR_USER_COMMAND'
  EXPORTING
    iv_object  = p_trkorr
    iv_type    = 'REQU'
    iv_command = 'ADDO'
*  IMPORTING
*    ev_exit    = ev_exit
  .

*CALL FUNCTION 'TR_REQUEST_MODIFY'
*  EXPORTING
*    iv_action            = 'CREA'    " Aktion: Anlegen/Anzeigen/Ändern/Löschen
**    iv_new_request       = SPACE    " Anlegen: Name des neuen Auftrags (nur für D,P,F)
*    iv_new_request_type  = 'K'
**    iv_new_task_type     = iv_new_task_type    " Anlegen: Typ der neuen Aufgabe
*    iv_new_tarsystem     = 'QS1'
*    iv_new_as4text       = 'Test'
**    is_new_attribute     = is_new_attribute    " Anlegen: Attribut des neuen Auftrags
**    iv_request           = iv_request    " Nummer des Auftrags
**    is_request           = is_request    " Nummer des Auftrags
*    it_users             = lt_users    " Mitarbeiterliste für Aufgaben
**    iv_start_column      = 1    " Spalte, ab der das Popup beginnt
**    iv_start_row         = 4    " Zeile,  ab der das Popup beginnt
**  IMPORTING
**    es_new_request       = es_new_request    " Neu angelegter Auftrag (Aktion Anlegen)
**    et_new_tasks         = et_new_tasks    " Neu angelegte Aufgaben (Aktion Anlegen)
**    ev_request_changed   = ev_request_changed    " Auftrag wurde verändert ('X',' ')
**    es_request           = es_request    " Geänderter Auftrag
**    ev_attributes_filled = ev_attributes_filled    " Attribute sind gefüllt
**    et_attributes        = et_attributes    " Attribute des Auftrags
*  EXCEPTIONS
*    cancelled_by_user    = 1
*    no_authorization     = 2
*    invalid_action       = 3
*    invalid_request      = 4
*    invalid_request_type = 5
*    request_not_created  = 6
*    request_not_deleted  = 7
*    enqueue_failed       = 8
*    db_access_error      = 9
*    OTHERS               = 10.
*
*IF sy-subrc <> 0.
*  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
*          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*          DISPLAY LIKE sy-msgty.
*ENDIF.
