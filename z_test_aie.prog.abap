REPORT z_test_aie.

DATA:
*     salesdocumentin           TYPE bapivbeln-vbeln,
     order_header_in_xx           TYPE bapisdhd1,
*     order_header_inx          TYPE bapisdhd1x,
*     sender                    TYPE bapi_sender,
*     binary_relationshiptype   TYPE bapireltype-reltype,
*     int_number_assignment     TYPE bapiflag-bapiflag,
*     behave_when_error         TYPE bapiflag-bapiflag,
*     logic_switch              TYPE bapisdls,
*     testrun                   TYPE bapiflag-bapiflag,
*     convert                   TYPE bapiflag-bapiflag,
     salesdocument             TYPE bapivbeln-vbeln,
*     return                    TYPE STANDARD TABLE OF bapiret2,
*     order_items_in            TYPE STANDARD TABLE OF bapisditm,
*     order_items_inx           TYPE STANDARD TABLE OF bapisditmx,
     order_partners            TYPE STANDARD TABLE OF bapiparnr
*     order_schedules_in        TYPE STANDARD TABLE OF bapischdl,
*     order_schedules_inx       TYPE STANDARD TABLE OF bapischdlx,
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



call FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
  EXPORTING
*    salesdocumentin         = salesdocumentin    " Sales and Distribution Document Number
    order_header_in         = order_header_in_xx    " Order Header
*    order_header_inx        = order_header_inx    " Sales Order Check List
*    sender                  = sender    " Logical System - Sender
*    binary_relationshiptype = binary_relationshiptype    " Binary Relationship Type (Private)
*    int_number_assignment   = int_number_assignment    " Internal Item Number Assignment
*    behave_when_error       = behave_when_error    " Error Handling
*    logic_switch            = logic_switch    " Internal Control Parameter
*    testrun                 = testrun    " Test Run
*    convert                 = SPACE    " Conversion of Partner Function + Order Type
*  IMPORTING
*    salesdocument           = salesdocument    " Number of Generated Document
  TABLES
*    return                  = return    " Return Messages
*    order_items_in          = order_items_in    " Item Data
*    order_items_inx         = order_items_inx    " Item Data Checkbox
    order_partners          = order_partners    " Document Partner
*    order_schedules_in      = order_schedules_in    " Schedule Line Data
*    order_schedules_inx     = order_schedules_inx    " Checkbox Schedule Line Data
*    order_conditions_in     = order_conditions_in    " Conditions
*    order_conditions_inx    = order_conditions_inx    " Conditions Checkbox
*    order_cfgs_ref          = order_cfgs_ref    " Configuration: Reference Data
*    order_cfgs_inst         = order_cfgs_inst    " Configuration: Instances
*    order_cfgs_part_of      = order_cfgs_part_of    " Configuration: Part-of Specifications
*    order_cfgs_value        = order_cfgs_value    " Configuration: Characteristic Values
*    order_cfgs_blob         = order_cfgs_blob    " Configuration: BLOB Internal Data (SCE)
*    order_cfgs_vk           = order_cfgs_vk    " Configuration: Variant Condition Key
*    order_cfgs_refinst      = order_cfgs_refinst    " Configuration: Reference Item / Instance
*    order_ccard             = order_ccard    " Credit Card Data
*    order_text              = order_text    " Texts
*    order_keys              = order_keys    " Output Table of Reference Keys
*    extensionin             = extensionin    " Customer Enhancement for VBAK, VBAP, VBEP
*    partneraddresses        = partneraddresses    " BAPI Reference Structure for Addresses (Org./Company)
  .
