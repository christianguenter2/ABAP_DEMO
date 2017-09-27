REPORT z_test_reca.

TYPES: BEGIN OF ty_data,
         mark TYPE xfeld,
         text TYPE string,
       END OF ty_data,
       tty_data TYPE STANDARD TABLE OF ty_data.

DATA: lt_data TYPE tty_data,
      lv_data LIKE LINE OF lt_data.

lv_data-text = |Test|.
INSERT lv_data INTO TABLE lt_data.

CALL FUNCTION 'RECA_GUI_F4_ITAB_POPUP'
  EXPORTING
    id_return_field    = 'TEXT'
*    id_title           = SPACE
*    if_display         = ABAP_FALSE
*    if_multi           = ABAP_FALSE
*    it_mark            = it_mark
*    if_send_cancel_msg = ABAP_TRUE
*  IMPORTING
*    ed_result          = ed_result
*    ef_canceled        = ef_canceled
  TABLES
    it_f4value         = lt_data
*    et_result          = et_result
*  EXCEPTIONS
*    parameter_error    = 1
*    no_values_found    = 2
*    error              = 3
*    others             = 4
  .
IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
