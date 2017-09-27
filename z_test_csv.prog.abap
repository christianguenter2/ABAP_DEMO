*&---------------------------------------------------------------------*
*& Report  Z_TEST_CSV
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_csv.

DATA: lt_data      TYPE STANDARD TABLE OF draw,
      lv_data      LIKE LINE OF lt_data,
      lt_xdata     TYPE solix_tab,
      lo_excel     TYPE REF TO zcl_excel,
      lo_worksheet TYPE REF TO zcl_excel_worksheet,
      lo_writer    TYPE REF TO zif_excel_writer,
      xs           TYPE xstring.

START-OF-SELECTION.
  select * FROM draw INTO TABLE lt_data
                     UP TO 10 ROWS.

  CREATE OBJECT lo_excel.
  lo_worksheet = lo_excel->get_active_worksheet( ).

  lo_worksheet->set_table(
    EXPORTING
      ip_table           = lt_data
      ip_table_title     = 'Test'
      ip_top_left_column = 'A'
      ip_top_left_row    = 1 ).

  CREATE OBJECT lo_writer TYPE zcl_excel_writer_csv.

  xs = lo_writer->write_file( lo_excel ).
  cl_bcs_convert=>xstring_to_xtab( EXPORTING iv_xstring = xs
                                   IMPORTING et_xtab    = lt_xdata ).

  cl_gui_frontend_services=>gui_download(
    EXPORTING
*      bin_filesize              = bin_filesize    " Dateilänge bei Binärdateien
      filename                  = 'c:\temp\test.csv'
*      filetype                  = 'ASC'    " Dateityp (Ascii, Binär, ...)
*      append                    = SPACE    " Charakterfeld der Länge 1
*      write_field_separator     = SPACE    " Spalten durch TAB trennen bei ASCII Download.
*      header                    = '00'    " Bytekette, die im Binärmodus an den Anfang der Datei geschri
*      trunc_trailing_blanks     = SPACE    " Bei Char-Feldern Leerzeichen am Ende nicht schreiben
*      write_lf                  = 'X'    " Beim Char-Download am Zeilenende CR/LF einfügen
*      col_select                = SPACE    " Es sollen nur einzelne Spalten der Tabelle übertragen werden
*      col_select_mask           = SPACE    " Vektor, der für zu übertragende Spalten 'X' enthält.
*      dat_mode                  = SPACE    " Zahl- und Datumsfelder werden im 'DAT' Format des ws_downloa
*      confirm_overwrite         = SPACE    " Überschreiben der Datei nur nach Bestätigung
*      no_auth_check             = SPACE    " Überprüfung der Zugriffsrechte abschalten.
*      codepage                  = SPACE    " Zeichenrepräsentation für Ausgabe
*      ignore_cerr               = ABAP_TRUE    " Gibt an, ob Fehler bei der Zeichensatzkonvertierung ignorier
*      replacement               = '#'    " Ersatzzeichen für nicht-konvertierbare Zeichen.
*      write_bom                 = SPACE    " Schreibt ein Unicode Byte-Order-Mark, falls gesetzt
*      trunc_trailing_blanks_eol = 'X'    " Remove trailing blanks of the last column
*      wk1_n_format              = SPACE
*      wk1_n_size                = SPACE
*      wk1_t_format              = SPACE
*      wk1_t_size                = SPACE
*      show_transfer_status      = 'X'    " Ermöglicht das Unterdrücken der Transfer Status Meldung
*      fieldnames                = fieldnames    " Feldnamen Tabelle
*      write_lf_after_last_line  = 'X'    " Schreibt nach letzem Datensatz ein CR/LF
*      virus_scan_profile        = '/SCET/GUI_DOWNLOAD'    " Viren-Scan-Profil
*    IMPORTING
*      filelength                = filelength    " Anzahl der übertragenen Bytes
    CHANGING
      data_tab                  = lt_xdata
    EXCEPTIONS
      file_write_error          = 1
      no_batch                  = 2
      gui_refuse_filetransfer   = 3
      invalid_type              = 4
      no_authority              = 5
      unknown_error             = 6
      header_not_allowed        = 7
      separator_not_allowed     = 8
      filesize_not_allowed      = 9
      header_too_long           = 10
      dp_error_create           = 11
      dp_error_send             = 12
      dp_error_write            = 13
      unknown_dp_error          = 14
      access_denied             = 15
      dp_out_of_memory          = 16
      disk_full                 = 17
      dp_timeout                = 18
      file_not_found            = 19
      dataprovider_exception    = 20
      control_flush_error       = 21
      not_supported_by_gui      = 22
      error_no_gui              = 23
      OTHERS                    = 24
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE sy-msgty.
  ENDIF.
