REPORT z_test_release_lifsk.

PARAMETERS: vbeln TYPE vbeln OBLIGATORY DEFAULT '5801023002',
            test TYPE abap_bool DEFAULT 'X' AS CHECKBOX.

DATA: order_header_in  TYPE bapisdh1,
      order_header_inx TYPE bapisdh1x,
      return           TYPE STANDARD TABLE OF bapiret2
                            WITH NON-UNIQUE DEFAULT KEY.

order_header_in-dlv_block  = ''.
order_header_inx-dlv_block = abap_true.

order_header_inx-updateflag = 'U'.

CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
  EXPORTING
    salesdocument    = vbeln
    order_header_in  = order_header_in    " Auftragskopf
    order_header_inx = order_header_inx    " Ankreuzleiste Auftragskopf
    simulation       = test
  TABLES
    return           = return.    " Returncode

IF test = abap_false.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
ENDIF.

cl_demo_output=>display_data( return ).
