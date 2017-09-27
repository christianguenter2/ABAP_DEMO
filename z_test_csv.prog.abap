*&---------------------------------------------------------------------*
*& Report z_test_csv
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_csv.

DATA: data_tab TYPE truxs_t_text_data .

cl_gui_frontend_services=>gui_upload(
  EXPORTING
    filename                = 'C:\Temp\test.csv'
    filetype                = 'ASC'
*    has_field_separator     = has_field_separator    " Spalten durch TAB getrennt bei ASCII Upload
*    header_length           = header_length    " Länge des Headers bei Binärdaten
*    read_by_line            = read_by_line    " Die Datei wird zeilenweise in die interne Tabelle geschriebe
*    dat_mode                = dat_mode    " Zahl- und Datumsfelder werden im 'DAT' Format des ws_downloa
*    codepage                = codepage    " Zeichenrepräsentation für Ausgabe
*    ignore_cerr             = ignore_cerr    " Gibt an, ob Fehler bei der Zeichensatzkonvertierung ignorier
*    replacement             = replacement    " Ersatzzeichen für nicht-konvertierbare Zeichen.
*    virus_scan_profile      = virus_scan_profile    " Viren-Scan-Profil
*  IMPORTING
*    filelength              = filelength    " Dateilänge
*    header                  = header    " Header der Datei bei binärem Upload
  CHANGING
    data_tab                = data_tab
*    isscanperformed         = isscanperformed    " File ist bereits gescannt
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
    OTHERS                  = 19 ).

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          DISPLAY LIKE sy-msgty.
  RETURN.
ENDIF.

TYPES: BEGIN OF ty_data,
         col1 TYPE string,
         col2 TYPE string,
       END OF ty_data,
       tty_data TYPE STANDARD TABLE OF ty_data
                     WITH NON-UNIQUE DEFAULT KEY.

DATA: lt_data TYPE tty_data.

CALL FUNCTION 'TEXT_CONVERT_CSV_TO_SAP'
  EXPORTING
*   i_field_seperator    = i_field_seperator
*   i_line_header        = i_line_header
    i_tab_raw_data       = data_tab
*   i_filename           = i_filename
  TABLES
    i_tab_converted_data = lt_data
  EXCEPTIONS
    conversion_failed    = 1
    OTHERS               = 2.

    cl_demo_output=>display( lt_data ).
