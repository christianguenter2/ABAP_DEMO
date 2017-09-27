*&---------------------------------------------------------------------*
*& Report  Z_TEST_CLIPBOARD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_clipboard.

DATA: lt_data TYPE STANDARD TABLE OF char12,
      rc      TYPE i.

APPEND 'DV1K9A0XXX' TO lt_data.

IF sy-uname = 'GUENTERC'.
  cl_gui_frontend_services=>clipboard_export(
    EXPORTING
      no_auth_check        = abap_true    " Überprüfung der Zugriffsrechte abschalten.
    IMPORTING
      data                 = lt_data
    CHANGING
      rc                   = rc    " Rückgabewert
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      no_authority         = 4
      OTHERS               = 5
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE sy-msgty.
  ENDIF.
ENDIF.
