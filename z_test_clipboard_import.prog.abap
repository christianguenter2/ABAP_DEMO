*&---------------------------------------------------------------------*
*& Report  Z_TEST_CLIPBOARD_IMPORT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_clipboard_import.

DATA: lt_data TYPE table_matnr.

cl_gui_frontend_services=>clipboard_import(
  IMPORTING
    data                 = lt_data    " Datentabelle
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    not_supported_by_gui = 3
    OTHERS               = 4
).
IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
