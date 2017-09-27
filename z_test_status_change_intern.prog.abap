*&---------------------------------------------------------------------*
*& Report  Z_TEST_STATUS_CHANGE_INTERN
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_status_change_intern.

DATA: status TYPE STANDARD TABLE OF jstat.
FIELD-SYMBOLS: <status> LIKE LINE OF status.

CALL FUNCTION 'STATUS_READ'
  EXPORTING
*    client           = SY-MANDT    " Mandant
    objnr            = 'QM000300702512'    " Objektnummer
*    only_active      = SPACE    " Kennz. 'Nur aktive Status übergeben'
*  IMPORTING
*    obtyp            = obtyp    " Objekttyp
*    stsma            = stsma    " Statusschema
*    stonr            = stonr    " Statusordnungsnummer
  TABLES
    status           = status    " Tabelle der Einzelstatus zum Objekt
  EXCEPTIONS
    object_not_found = 1
    OTHERS           = 2
  .
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

READ TABLE status ASSIGNING <status>
                  WITH KEY stat = 'I0076'.
IF sy-subrc = 0.
  IF <status>-inact = abap_true.
    <status>-inact = abap_false.
  ELSE.
    <status>-inact = abap_true.
  ENDIF.
ENDIF.

CALL FUNCTION 'STATUS_SET_INTERN'
  EXPORTING
    iv_objnr            = 'QM000300702512'    " Objektnummer
    iv_status           = 'I0076'    " Einzelstatus eines Objekts (Systemstatus)
  EXCEPTIONS
    object_not_found    = 1
    status_inconsistent = 2
    status_not_allowed  = 3
    OTHERS              = 4.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

COMMIT WORK.


*CALL   FUNCTION 'STATUS_CHANGE_INTERN'
*  EXPORTING
**    check_only          = SPACE    " Kennzeichen 'Nur Prüfungen durchführen'
**    client              = SY-MANDT    " Mandant (nur für Ausnahmefälle!)
*    objnr               = 'QM000300702512'    " Objektnummer
**    zeile               = SPACE    " Vorgebene Zeilennnummer bei MESSAGE_STORE
**    set_chgkz           = set_chgkz    " Flag 'Änderungsbelege aktivieren'
**  IMPORTING
**    error_occurred      = error_occurred    " allgemeiner Fehler aufgetreten
**    object_not_found    = object_not_found    " Statusobjekt nicht gefunden
**    status_inconsistent = status_inconsistent    " Folgestatussituation inkonsistent
**    status_not_allowed  = status_not_allowed    " Kennzeichen 'Statusänderung nicht erlaubt'
*  TABLES
*    status              = status    " Tabelle der Einzelstatus zum Objekt
*  EXCEPTIONS
*    object_not_found    = 1
*    status_inconsistent = 2
*    status_not_allowed  = 3
*    OTHERS              = 4
*  .
*IF sy-subrc <> 0.
*  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*ENDIF.
