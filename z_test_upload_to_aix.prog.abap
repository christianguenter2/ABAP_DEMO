*&---------------------------------------------------------------------*
*& Report  Z_TEST_UPLOAD_TO_AIX
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_upload_to_aix.

PARAMETERS: p_file TYPE string OBLIGATORY,
            p_path TYPE string OBLIGATORY DEFAULT '/usr/sap/trans/smi/DV1/' LOWER CASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: file_table  TYPE filetable,
        rc  TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
*    EXPORTING
*      window_title            =     " Titel des Datei-Öffnen Dialogs
*      default_extension       =     " Vorschlagserweiterung
*      default_filename        =     " Vorschlagsdateiname
*      file_filter             =     " Filterstring für Dateierweiterung
*      with_encoding           =     " file encoding
*      initial_directory       =     " Ausgangsverzeichnis
*      multiselection          =     " Mehrfachselektion möglich
    CHANGING
      file_table              = file_table     " Tabelle, die selektierte Dateien enthält
      rc                      = rc    " Rückgabewert: Anzahl Dateien oder -1 falls Fehler auftritt
*      user_action             =     " Benutzeraktion( s. Kl.konstanten ACTION_OK, ACTION_CANCEL)
*      file_encoding           =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE file_table INTO p_file INDEX 1.

START-OF-SELECTION.
  TYPES: BEGIN OF ty_data,
             buf(8192) TYPE x,
           END OF ty_data.

  DATA: datatab TYPE STANDARD TABLE OF ty_data WITH DEFAULT KEY.

  cl_gui_frontend_services=>gui_upload(
    EXPORTING
      filename                = p_file    " Name der Datei
      filetype                = 'BIN'    " Dateityp (Ascii, Binär)
*      has_field_separator     = SPACE    " Spalten durch TAB getrennt bei ASCII Upload
*      header_length           = 0    " Länge des Headers bei Binärdaten
*      read_by_line            = 'X'    " Die Datei wird zeilenweise in die interne Tabelle geschriebe
*      dat_mode                = SPACE    " Zahl- und Datumsfelder werden im 'DAT' Format des ws_downloa
*      codepage                = SPACE    " Zeichenrepräsentation für Ausgabe
*      ignore_cerr             = ABAP_TRUE    " Gibt an, ob Fehler bei der Zeichensatzkonvertierung ignorier
*      replacement             = '#'    " Ersatzzeichen für nicht-konvertierbare Zeichen.
*      virus_scan_profile      =     " Viren-Scan-Profil
*    IMPORTING
*      filelength              =     " Dateilänge
*      header                  =     " Header der Datei bei binärem Upload
    CHANGING
      data_tab                = datatab     " Übergabetabelle für Datei-Inhalt
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19
  ).
  IF sy-subrc <> 0.
   MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  data: name TYPE string.

  CL_BCS_UTILITIES=>split_path(
    EXPORTING
      iv_path = p_file
    IMPORTING
*      ev_path =
      ev_name = name
  ).

  p_path = P_path && name.
  data: mess TYPE string.

  OPEN DATASET P_path FOR OUTPUT in BINARY MODE
                      MESSAGE mess.
  if sy-subrc <> 0.
    write: mess.
    return.
  ENDIF.

  FIELD-SYMBOLS: <data> like LINE OF datatab.

  loop at datatab ASSIGNING <data>.
    TRANSFER <data> to P_path.
  endloop.

  close DATASET P_path.

  MESSAGE 'Dateiupload erfolgreich' TYPE 'I'.
