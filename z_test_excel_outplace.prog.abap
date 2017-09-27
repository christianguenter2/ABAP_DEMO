*&---------------------------------------------------------------------*
*& Report  Z_TEST_EXCEL_OUTPLACE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_excel_outplace.

TABLES: draw.
DATA: BEGIN OF intdraz OCCURS 0.
        INCLUDE STRUCTURE draz.
DATA: END OF intdraz.
DATA: pfad(140).
DATA: textname           LIKE thead-tdname,
      applikationsnummer,
      applikationstyp,
      display.

IMPORT draw intdraz pfad applikationsnummer applikationstyp
       FROM MEMORY ID 'SAP_APPLICATION'.

textname = pfad.
display  = 'X'.

IF applikationstyp = '2'.
  display  = ' '.
ENDIF.

cl_gui_frontend_services=>execute(
  EXPORTING
    document               = |{ pfad }|    " Pfad+Dokumentname
*    application            = application    " Pfad + Anwendungsname
*    parameter              = parameter    " Parameter für Anwendung
*    default_directory      = default_directory    " Vorschlagsverzeichnis
    maximized              = |{ abap_true }|    " Fenster maximiert anzeigen
*    minimized              = minimized    " Fenster minimiert anzeigen
*    synchronous            = synchronous    " Wenn 'X': die Anwendung läuft synchron
*    operation              = 'OPEN'    " Reserviert: Verb für ShellExecute
  EXCEPTIONS
    cntl_error             = 1
    error_no_gui           = 2
    bad_parameter          = 3
    file_not_found         = 4
    path_not_found         = 5
    file_extension_unknown = 6
    error_execute_failed   = 7
    synchronous_failed     = 8
    not_supported_by_gui   = 9
    OTHERS                 = 10 ).

CALL FUNCTION 'CV120_DOC_CHECKIN'
  EXPORTING
*    ps_cin_def       = ps_cin_def    " Definitionen für Checkin
*    pf_hostname      = pf_hostname    " Netzadresse
    pf_storage       = draw-dttrg    " Ablage für Dokument
*    pf_newfile       = SPACE    " Erzeuge neue Datei
*    pf_targetfile    = pf_targetfile    " Zieldatei auf dem Vault
    ps_draw          = draw    " Dokumentdaten
*    ps_doc_file      = ps_doc_file    " Originaldaten
*  IMPORTING
*    pfx_targetfile   = pfx_targetfile    " Zieldatei auf dem Vault (export)
  TABLES
    pt_draz          = intdraz    " Zusatzdateien
*    ptx_drao         = ptx_drao    " Originaldaten
*    ptx_draoz        = ptx_draoz    " Originaldaten - Zusatzdateien
*    ptx_archive_conn = ptx_archive_conn    " Verknüpfungstabelle (Archiv)
  EXCEPTIONS
    error            = 1
    OTHERS           = 2
  .
