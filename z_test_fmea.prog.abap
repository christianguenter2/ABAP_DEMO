REPORT z_test_fmea.

DATA: parameter TYPE text255,
      shortcut  TYPE string,
      soli_tab  TYPE soli_tab.

parameter = 'P_EXTID='   && 'F-000005-01-01-01-01-01'  && ';' &&
            'DYNP_OKCODE=ONLI'.

CALL FUNCTION 'SWN_CREATE_SHORTCUT'
  EXPORTING
    i_transaction   = '*QM_FMEA_DISPLAY'
    i_user          = ''
    i_title         = ''
    i_windowsize    = 'M'
    i_parameter     = parameter
  IMPORTING
    shortcut_string = shortcut.


soli_tab = cl_bcs_convert=>string_to_soli( shortcut ).

cl_gui_frontend_services=>gui_download(
  EXPORTING
    filename                  = 'C:\temp\test.sap'
  CHANGING
    data_tab                  = soli_tab
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
  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          DISPLAY LIKE sy-msgty.
ENDIF.
