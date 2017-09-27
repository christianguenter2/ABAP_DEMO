REPORT z_test_salesorder_cond.

CLASS test_sales_order_cond DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS: start.

ENDCLASS.

CLASS test_sales_order_cond IMPLEMENTATION.

  METHOD start.

    DATA: salesdocument    TYPE bapivbeln-vbeln,
          order_header_inx TYPE bapisdh1x,
          conditions_in    TYPE STANDARD TABLE OF bapicond,
          condition_in     TYPE bapicond,
          conditions_inx   TYPE STANDARD TABLE OF bapicondx,
          condition_inx    TYPE bapicondx,
          return           TYPE STANDARD TABLE OF bapiret2.

    salesdocument = '4204692180'.

    order_header_inx-updateflag = 'U'.

    condition_in-itm_number = '000010'.
    condition_in-cond_type  = 'ZR42'.
    condition_in-cond_value = '10'.
    INSERT condition_in INTO TABLE conditions_in.

    condition_inx-itm_number = '000010'.
    condition_inx-cond_type  = abap_true.
    condition_inx-cond_value = abap_true.
    INSERT condition_inx INTO TABLE conditions_inx.

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
      EXPORTING
        salesdocument    = salesdocument    " Nummer des Auftrages
        order_header_inx = order_header_inx    " Ankreuzleiste Auftragskopf
      TABLES
        return           = return    " Returncode
        conditions_in    = conditions_in    " Konditionen
        conditions_inx   = conditions_inx.    " Ankreuzleiste Konditionen

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .

    cl_demo_output=>display_data( return ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  test_sales_order_cond=>start( ).
