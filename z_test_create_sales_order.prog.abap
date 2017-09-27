*&---------------------------------------------------------------------*
*& Report  Z_TEST_CREATE_SALES_ORDER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_create_sales_order.


DATA:
*     salesdocumentin           TYPE bapivbeln-vbeln,
      order_header_in  TYPE bapisdhd1,
      order_header_inx TYPE bapisdhd1x,
*     sender                    TYPE bapi_sender,
*     binary_relationshiptype   TYPE bapireltype-reltype,
*     int_number_assignment     TYPE bapiflag-bapiflag,
*     behave_when_error         TYPE bapiflag-bapiflag,
*     logic_switch              TYPE bapisdls,
*     testrun                   TYPE bapiflag-bapiflag,
*     convert                   TYPE bapiflag-bapiflag,
      salesdocument       TYPE bapivbeln-vbeln,
      return              TYPE STANDARD TABLE OF bapiret2,
      order_items_in      TYPE STANDARD TABLE OF bapisditm,
      order_item_in       LIKE LINE OF order_items_in,
      order_items_inx     TYPE STANDARD TABLE OF bapisditmx,
      order_item_inx      LIKE LINE OF order_items_inx,
      order_partners      TYPE STANDARD TABLE OF bapiparnr,
      order_partner       TYPE bapiparnr,
      order_schedules_in  TYPE STANDARD TABLE OF bapischdl,
      order_schedule_in   TYPE bapischdl,
      order_schedules_inx TYPE STANDARD TABLE OF bapischdlx,
      order_schedule_inx  TYPE bapischdlx
*     order_conditions_in       TYPE STANDARD TABLE OF bapicond,
*     order_conditions_inx      TYPE STANDARD TABLE OF bapicondx,
*     order_cfgs_ref            TYPE STANDARD TABLE OF bapicucfg,
*     order_cfgs_inst           TYPE STANDARD TABLE OF bapicuins,
*     order_cfgs_part_of        TYPE STANDARD TABLE OF bapicuprt,
*     order_cfgs_value          TYPE STANDARD TABLE OF bapicuval,
*     order_cfgs_blob           TYPE STANDARD TABLE OF bapicublb,
*     order_cfgs_vk             TYPE STANDARD TABLE OF bapicuvk,
*     order_cfgs_refinst        TYPE STANDARD TABLE OF bapicuref,
*     order_ccard               TYPE STANDARD TABLE OF bapiccard,
*     order_text                TYPE STANDARD TABLE OF bapisdtext,
*     order_keys                TYPE STANDARD TABLE OF bapisdkey,
*     extensionin               TYPE STANDARD TABLE OF bapiparex,
*     partneraddresses          TYPE STANDARD TABLE OF bapiaddr1
.

order_header_in-doc_type  = 'ZL'.
order_header_inx-doc_type = abap_true.

order_header_in-sales_org  = 'DE01'.
order_header_inx-sales_org = abap_true.

order_header_in-division  = '01'.
order_header_inx-division = abap_true.

order_header_in-distr_chan  = '01'.
order_header_inx-distr_chan = abap_true.

order_partner-partn_numb = '0000490005'.
order_partner-partn_role = 'AG'.
INSERT order_partner INTO TABLE order_partners.

DATA: lv_posnr TYPE posnr.

lv_posnr = '000010'.

order_item_in-itm_number  = lv_posnr.
order_item_inx-itm_number = lv_posnr.
order_item_in-material    = '01800180'.
order_item_inx-material   = abap_true.
INSERT order_item_in INTO TABLE order_items_in.
INSERT order_item_inx INTO TABLE order_items_inx.

order_schedule_in-itm_number  = lv_posnr.
order_schedule_inx-itm_number = lv_posnr.
order_schedule_in-req_qty     = 10000000.
order_schedule_inx-req_qty    = abap_true.
INSERT order_schedule_in INTO TABLE order_schedules_in.
INSERT order_schedule_inx INTO TABLE order_schedules_inx.

CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
  EXPORTING
*     salesdocumentin               = salesdocumentin
     order_header_in               = order_header_in
     order_header_inx              = order_header_inx
*     sender                        = sender
*     binary_relationshiptype       = binary_relationshiptype
*     int_number_assignment         = int_number_assignment
*     behave_when_error             = behave_when_error
*     logic_switch                  = logic_switch
*     testrun                       = testrun
*     convert                       = convert
* IMPORTING
*     salesdocument                 = salesdocument
  TABLES
     return                        = return
     order_items_in                = order_items_in
     order_items_inx               = order_items_inx
     order_partners                = order_partners
     order_schedules_in            = order_schedules_in
     order_schedules_inx           = order_schedules_inx
*     order_conditions_in           = order_conditions_in
*     order_conditions_inx          = order_conditions_inx
*     order_cfgs_ref                = order_cfgs_ref
*     order_cfgs_inst               = order_cfgs_inst
*     order_cfgs_part_of            = order_cfgs_part_of
*     order_cfgs_value              = order_cfgs_value
*     order_cfgs_blob               = order_cfgs_blob
*     order_cfgs_vk                 = order_cfgs_vk
*     order_cfgs_refinst            = order_cfgs_refinst
*     order_ccard                   = order_ccard
*     order_text                    = order_text
*     order_keys                    = order_keys
*     extensionin                   = extensionin
*     partneraddresses              = partneraddresses
          .

CALL FUNCTION 'C14ALD_BAPIRET2_SHOW'
  TABLES
    i_bapiret2_tab = return.

CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
