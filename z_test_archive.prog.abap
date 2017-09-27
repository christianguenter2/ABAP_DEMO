
*&---------------------------------------------------------------------*
*& Report  Z_TEST_ARCHIVE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_archive.

PARAMETER: p_id TYPE toav0-object_id DEFAULT '10290200000002458233'.
*PARAMETER: p_id TYPE toav0-object_id DEFAULT '10015934000000172758'.

DATA: connections TYPE STANDARD TABLE OF toav0.

CALL FUNCTION 'ARCHIV_GET_CONNECTIONS'
  EXPORTING
    object_id     = p_id    " ID des Business Objekttyps
  TABLES
    connections   = connections    " Verknüpfungseinträge
  EXCEPTIONS
    nothing_found = 1
    OTHERS        = 2.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

FIELD-SYMBOLS: <connection> LIKE LINE OF connections.

READ TABLE connections ASSIGNING <connection>
                       INDEX 1.
CHECK sy-subrc = 0.

DATA: status        TYPE c,
      doc_type(256) TYPE c,
      arch_date     TYPE d,
      arch_time     TYPE t,
      mimetype(256) TYPE c,
      comps         TYPE STANDARD TABLE OF scms_comps,
      compsl        TYPE STANDARD TABLE OF scms_compsl.

CALL FUNCTION 'SCMS_AO_STATUS'
  EXPORTING
*   mandt        = SY-MANDT    " R/3 System, Client Number from Logon
    arc_id       = <connection>-archiv_id    " R/3 System, Client Number from Logon
    doc_id       = <connection>-arc_doc_id    " hR/3 System, Client Number from Logon
  IMPORTING
    status       = status    " R/3 System, Client Number from Logon
    doc_type     = doc_type    " R/3 System, Client Number from Logon
    arch_date    = arch_date    " R/3 System, Client Number from Logon
    arch_time    = arch_time    " R/3 System, Client Number from Logon
    mimetype     = mimetype    " R/3 System, Client Number from Logon
  TABLES
    comps        = comps    " Components of Stored Documents
    compsl       = compsl    " Components of Stored Documents
  EXCEPTIONS
    error_http   = 1
    error_kernel = 2
    error_archiv = 3
    error_config = 4
    OTHERS       = 5.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.


DATA: lt_data TYPE STANDARD TABLE OF tbl1024,
      length  TYPE i.

CALL FUNCTION 'SCMS_AO_TABLE_GET'
  EXPORTING
*   mandt        = SY-MANDT    " R/3-System, Mandantennummer aus Anmeldung
    arc_id       = <connection>-archiv_id
    doc_id       = <connection>-arc_doc_id
*   comp_id      = 'data' " R/3-System, Mandantennummer aus Anmeldung
  IMPORTING
    length       = length    " R/3-System, Mandantennummer aus Anmeldung
  TABLES
    data         = lt_data    " R/3-System, Mandantennummer aus Anmeldung
  EXCEPTIONS
    error_http   = 1
    error_archiv = 2
    error_kernel = 3
    error_config = 4
    OTHERS       = 5.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

DATA: filename TYPE string.

filename = |C:\\temp\\ { cl_uuid_factory=>create_system_uuid( )->create_uuid_c32( ) }.{ doc_type }|.

cl_gui_frontend_services=>gui_download(
  EXPORTING
    filename                  = filename
    filetype                  = 'BIN'    " Dateityp (Ascii, Binär, ...)
  CHANGING
    data_tab                  = lt_data
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

cl_gui_frontend_services=>execute(
  EXPORTING
    document               = filename
*    application            = application    " Path and Name of Application
*    parameter              = parameter    " Parameter for Application
*    default_directory      = default_directory    " Default Directory
*    maximized              = maximized    " Show Window Maximized
*    minimized              = minimized    " Show Window Minimized
*    synchronous            = synchronous    " When 'X': Runs the Application in Synchronous Mode
*    operation              = 'OPEN'    " Reserved: Verb für ShellExecute
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

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
