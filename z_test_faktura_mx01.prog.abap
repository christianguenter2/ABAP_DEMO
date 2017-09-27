*&---------------------------------------------------------------------*
*& Report  Z_TEST_FAKTURA_MX01
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_faktura_mx01.

DATA: objecttype  TYPE toav0-sap_object VALUE 'VBRK',
      object_id   TYPE toav0-object_id VALUE '6423000013',
      connections TYPE STANDARD TABLE OF toav0.

CALL FUNCTION 'ARCHIV_GET_CONNECTIONS'
  EXPORTING
    objecttype         = objecttype    " Business Objekttyp
    object_id          = object_id    " ID des Business Objekttyps
*    client             = client    " Mandant
*    archiv_id          = archiv_id    " ID des Ablagesystems
*    arc_doc_id         = arc_doc_id    " Dokument-Id
*    documenttype       = documenttype    " SAP ArchiveLink Dokumentart
*    from_ar_date       = from_ar_date    " Anfangsdatum für die Suche
*    until_ar_date      = SY-DATUM    " Endedatum für die Suche
*    documentclass      = documentclass    " Technischer Dokumenttyp
*    del_date           = del_date    " Technischer Dokumenttyp
*    limited            = limited    " Limitierte Suche (500 Zeilen)
*    limit              = limit    " SAP ArchiveLink Anzahl der Dokumente
*  IMPORTING
*    count              = count    " Anzahl gefundener Verknüpfungen
*    reducedbylimit     = reducedbylimit    " Es sind noch mehr Treffer vorhanden
*    reducedbyauthority = reducedbyauthority    " Trefferliste wurde wg. Berechtigung reduziert
  TABLES
    connections        = connections    " Verknüpfungseinträge
*    parameter          = parameter    " Struktur der Parameter für Range Tabelle
  EXCEPTIONS
    nothing_found      = 1
    OTHERS             = 2
  .
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

FIELD-SYMBOLS: <connection> LIKE LINE OF connections.

READ TABLE connections ASSIGNING <connection> INDEX 1.
CHECK sy-subrc = 0.

CALL FUNCTION 'ARCHIVOBJECT_DISPLAY'
  EXPORTING
    archiv_doc_id            = <connection>-arc_doc_id    " Identifikator des anzuzeigenden Dokumentes
    "archiv_doc_index         = <connection>-space    " (wird nicht mehr verwendet)
    archiv_id                = <connection>-archiv_id    " Identifikation des Ablagesystems
    objecttype               = objecttype
    object_id                = <connection>-object_id
*    ar_object                = SPACE    " Identifikation des Ablagesystems
*    language                 = SPACE    " Anmeldesprache
*    sign                     = SPACE    " Bezugszeichen
*    window_id                = SPACE    " Eindeutige Windowidentifikation
*    window_title             = SPACE    " Fenstertitel
*    doc_type                 = SPACE    " Dokumenttyp
*    positioninalffile        = SPACE    " Position
*    noget                    = SPACE
*    pathoffile               = SPACE
*    eof                      = SPACE
*    dalength                 = SPACE
*    pfstatus                 = SPACE
*    report                   = SPACE
*    multiple                 = SPACE
*  IMPORTING
*    returncode               = returncode    " Rückmeldung
*  TABLES
*    dispdocs                 = dispdocs    " Struktur für die Anzeige von Dokumenten
  EXCEPTIONS
    error_archiv             = 1
    error_communicationtable = 2
    error_kernel             = 3
    OTHERS                   = 4
  .
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

cl_demo_output=>display_data( connections ).
