REPORT z_test_execute_cmd_frontend.

CONSTANTS:  filename TYPE string VALUE `c:\temp\test_output.txt`.
DATA: data_tab TYPE stringtab.

cl_gui_frontend_services=>execute(
  EXPORTING
    application            = |cmd.exe|
    parameter              = |/c ls > { filename } |
    minimized              = 'X'
    synchronous            = 'X'
  EXCEPTIONS
    OTHERS                 = 10 ).

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          DISPLAY LIKE sy-msgty.
  RETURN.
ENDIF.

cl_gui_frontend_services=>gui_upload(
  EXPORTING
    filename                = filename
    filetype                = 'ASC'    " Dateityp (Ascii, Binär)
  CHANGING
    data_tab                = data_tab    " Übergabetabelle für Datei-Inhalt
  EXCEPTIONS
    OTHERS                  = 19 ).

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          DISPLAY LIKE sy-msgty.
  RETURN.
ENDIF.

cl_demo_output=>display_data( data_tab ).
