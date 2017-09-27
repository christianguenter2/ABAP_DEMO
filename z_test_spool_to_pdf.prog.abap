REPORT z_test_spool_to_pdf.

DATA: print_parameters TYPE pri_params,
      archi_parameters TYPE arc_params,
      valid_flag       TYPE c LENGTH 1.

DATA(pernr) = CONV persno( '00012667' ).
DATA(datum_low) = CONV datum( '20140301' ).
DATA(datum_high) = CONV datum( '20140331' ).

CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
CALL FUNCTION 'HR_CLUSTER_AUTHCHECK_DISABLE'.

CALL FUNCTION 'GET_PRINT_PARAMETERS'
  EXPORTING
    report                 = 'SUBMITABLE'
    archive_mode           = '1'
    no_dialog              = abap_true
  IMPORTING
    out_parameters         = print_parameters
    out_archive_parameters = archi_parameters
    valid                  = valid_flag
  EXCEPTIONS
    invalid_print_params   = 2
    OTHERS                 = 4.

DATA: number TYPE tbtcjob-jobcount,
      name   TYPE tbtcjob-jobname VALUE 'JOB_TEST'.

CALL FUNCTION 'JOB_OPEN'
  EXPORTING
    jobname          = name
  IMPORTING
    jobcount         = number
  EXCEPTIONS
    cant_create_job  = 1
    invalid_job_data = 2
    jobname_missing  = 3
    OTHERS           = 4.

SUBMIT rptedt00
       WITH pnpbegda EQ datum_low
       WITH pnpendda EQ datum_high
       WITH pnppernr EQ pernr
       WITH form-nr  EQ '§F00'
       AND RETURN
       TO SAP-SPOOL
       SPOOL PARAMETERS print_parameters
       WITHOUT SPOOL DYNPRO
       VIA JOB name NUMBER number.

CHECK sy-subrc = 0.

CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
    jobcount             = number
    jobname              = name    " Job-Name
    strtimmed            = abap_true    " Sofortausführung des Batch-Jobs
  EXCEPTIONS
    cant_start_immediate = 1
    invalid_startdate    = 2
    jobname_missing      = 3
    job_close_failed     = 4
    job_nosteps          = 5
    job_notex            = 6
    lock_failed          = 7
    invalid_target       = 8
    OTHERS               = 9.

DATA finished TYPE abap_bool.

WHILE finished = abap_false.
  CALL FUNCTION 'SHOW_JOBSTATE'
    EXPORTING
      jobcount         = number
      jobname          = name
    IMPORTING
      finished         = finished
    EXCEPTIONS
      jobcount_missing = 1
      jobname_missing  = 2
      job_notex        = 3
      OTHERS           = 4.
ENDWHILE.


SELECT SINGLE spoolid
       FROM tbtc_spoolid
       INTO @DATA(spoolid)
       WHERE jobname  = @name
       AND   jobcount = @number.

CHECK sy-subrc = 0.

DATA: pdf TYPE TABLE OF tline.

CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
  EXPORTING
    src_spoolid              = CONV tsp01-rqident( spoolid )   " Spool: Spool-Auftragsnummer
  TABLES
    pdf                      = pdf     " PDF Datei
  EXCEPTIONS
    err_no_abap_spooljob     = 1
    err_no_spooljob          = 2
    err_no_permission        = 3
    err_conv_not_possible    = 4
    err_bad_destdevice       = 5
    user_cancelled           = 6
    err_spoolerror           = 7
    err_temseerror           = 8
    err_btcjob_open_failed   = 9
    err_btcjob_submit_failed = 10
    err_btcjob_close_failed  = 11
    OTHERS                   = 12.


cl_gui_frontend_services=>gui_download(
  EXPORTING
    filename                  = 'C:\Temp\test_spool.pdf'    " Name der Datei
    filetype                  = 'BIN'    " Dateityp (Ascii, Binär, ...)
  CHANGING
    data_tab                  = pdf    " Übergabetabelle
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
    document               = 'C:\Temp\test_spool.pdf'    " Pfad+Dokumentname
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
    OTHERS                 = 10
).
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
