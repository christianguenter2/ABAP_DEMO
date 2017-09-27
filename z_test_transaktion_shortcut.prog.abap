*&---------------------------------------------------------------------*
*& Report  Z_TEST_TRANSAKTION_SHORTCUT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_transaktion_shortcut.

START-OF-SELECTION.
  DATA: shortcut_table  TYPE soli_tab,
        shortcut_string TYPE string.

  CALL FUNCTION 'SWN_CREATE_SHORTCUT'
    EXPORTING
      i_transaction           = 'ZPLM73'
*     i_report                =
*     i_system_command        =
*     i_parameter             =
*     i_saplogon_id           =
*     i_sysid                 = SY-SYSID
*     i_guiparm               =
*     i_client                = SY-MANDT
*     i_user                  = SY-UNAME
*     i_language              = SY-LANGU
*     i_windowsize            = 'Normal window'
*     i_title                 =
*     i_custom                =
    IMPORTING
      shortcut_table          = shortcut_table
      shortcut_string         = shortcut_string
    EXCEPTIONS
      inconsistent_parameters = 1
      OTHERS                  = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  cl_gui_frontend_services=>gui_download(
    EXPORTING
*      bin_filesize              =     " File length for binary files
      filename                  = 'C:\temp\test.sap'     " Name of file
*      filetype                  = 'ASC'    " File type (ASCII, binary ...)
*      append                    = SPACE    " Character Field Length 1
*      write_field_separator     = SPACE    " Separate Columns by Tabs in Case of ASCII Download
*      header                    = '00'    " Byte Chain Written to Beginning of File in Binary Mode
*      trunc_trailing_blanks     = SPACE    " Do not Write Blank at the End of Char Fields
*      write_lf                  = 'X'    " Insert CR/LF at End of Line in Case of Char Download
*      col_select                = SPACE    " Copy Only Selected Columns of the Table
*      col_select_mask           = SPACE    " Vector Containing an 'X' for the Column To Be Copied
*      dat_mode                  = SPACE    " Numeric and date fields are in DAT format in WS_DOWNLOAD
*      confirm_overwrite         = SPACE    " Overwrite File Only After Confirmation
*      no_auth_check             = SPACE    " Switch off Check for Access Rights
*      codepage                  = SPACE    " Character Representation for Output
*      ignore_cerr               = ABAP_TRUE    " Specifies whether errors converting char. sets are ignored
*      replacement               = '#'    " Replacement Character for Non-Convertible Characters
*      write_bom                 = SPACE    " If set, writes a Unicode byte order mark
*      trunc_trailing_blanks_eol = 'X'    " Remove Trailing Blanks in Last Column
*      wk1_n_format              = SPACE
*      wk1_n_size                = SPACE
*      wk1_t_format              = SPACE
*      wk1_t_size                = SPACE
*      show_transfer_status      = 'X'    " Enables suppression of transfer status message
*      fieldnames                =     " Table Field Names
*      write_lf_after_last_line  = 'X'    " Writes a CR/LF after final data record
*      virus_scan_profile        = '/SCET/GUI_DOWNLOAD'    " Virus Scan Profile
*    IMPORTING
*      filelength                =     " Number of bytes transferred
    CHANGING
      data_tab                  = shortcut_table    " Transfer table
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
      OTHERS                    = 24 ).

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DATA: receivers TYPE STANDARD TABLE OF soos1,
        receiver  LIKE LINE OF receivers,
        objcont   TYPE soli_tab,
        attribs   TYPE sood1.

  attribs-objnam   = 'HG_100000045'. "'TEST'.
  attribs-objdes   = 'TEST'.
  attribs-objsns   = 'F'.              "F=Funktion  P=privat.
  attribs-file_ext = 'SAP'.
  attribs-acout    = 'X'.

  receiver-recextnam = 'christian.guenter@hansgrohe.com'.
  receiver-sortfield = 'christian.guenter@hansgrohe.com'.
  receiver-rcdat     = sy-datum.
  receiver-recesc    = 'U'.
  receiver-recnam    = 'U-'.
  receiver-sndart    = 'INT'.
  receiver-sndex     = 'X'.
  receiver-sndpri    = '1'.
  receiver-sortclass = 5.
  INSERT receiver INTO TABLE receivers.

  CALL FUNCTION 'Z_MAIL_SENDEN'
    EXPORTING
      i_attribs         = attribs
*      i_obj_rolea       =
*      i_path_and_file   =
*      i_dialog          =
*      i_its             = SPACE
*      i_shortcut        = SPACE
*      i_originator      = SY-UNAME
*      i_originator_type = 'B'
*    IMPORTING
*      e_return          =
    TABLES
      t_receivers       = receivers
      t_objcont         = objcont
      t_downapp         = shortcut_table
*      t_downtab         =
    .

  COMMIT WORK.
