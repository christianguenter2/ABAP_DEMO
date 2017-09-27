*&---------------------------------------------------------------------*
*& Report  Z_TEST_CREATE_ORDER_FROM_OFFER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_create_order_from_offer.

PARAMETERS: p_vbeln TYPE vbak-vbeln OBLIGATORY VALUE CHECK.

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS: start.
  PRIVATE SECTION.
    METHODS: _select,
             _create_order.
    DATA: order_partners    TYPE STANDARD TABLE OF bapiparnr,
          order_header_in   TYPE bapisdhd1,
          order_header_inx  TYPE bapisdhd1x,
          order_items_in    TYPE STANDARD TABLE OF bapisditm,
          order_items_inx   TYPE STANDARD TABLE OF bapisditmx,
          order_schedules_in  TYPE  STANDARD TABLE OF bapischdl,
          order_schedules_inx	TYPE  STANDARD TABLE OF	bapischdlx.
ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD start.
    _select( ).
    _create_order( ).
  ENDMETHOD.                    "start

  METHOD _select.
    DATA: lv_offer TYPE vbak,
          lt_vbap TYPE STANDARD TABLE OF vbap,
          lt_vbep TYPE STANDARD TABLE OF vbep,
          order_item_in  LIKE LINE OF order_items_in ,
          order_item_inx LIKE LINE OF order_items_inx,
          order_schedule_in   TYPE  bapischdl,
          order_schedule_inx  TYPE  bapischdlx.

    FIELD-SYMBOLS: <order_partner> LIKE LINE OF order_partners,
                   <vbap> LIKE LINE OF lt_vbap,
                   <vbep> LIKE LINE OF lt_vbep.

    SELECT SINGLE *
        FROM vbak
        INTO lv_offer
        WHERE vbeln = p_vbeln
        AND   vbtyp = 'B'.

    IF sy-subrc <> 0.
      MESSAGE 'Angebot nicht gefunden' TYPE 'E'.
    ENDIF.

    SELECT *
      FROM vbap
      INTO TABLE lt_vbap
      WHERE vbeln = p_vbeln.

    SELECT *
      FROM vbep
      INTO TABLE lt_vbep
      WHERE vbeln = p_vbeln.

    order_header_in-doc_type      = 'ZL'.
    order_header_inx-doc_type     = abap_true.
    order_header_in-sales_org     = lv_offer-vkorg.
    order_header_inx-sales_org    = abap_true.
    order_header_in-distr_chan    = lv_offer-vtweg.
    order_header_inx-distr_chan   = abap_true.
    order_header_in-division      = lv_offer-spart.
    order_header_inx-division     = abap_true.

    order_header_in-ref_doc = p_vbeln.
    order_header_in-refdoc_cat = 'B'.

    APPEND INITIAL LINE TO order_partners ASSIGNING <order_partner>.
    IF sy-subrc = 0.
      <order_partner>-partn_role = 'AG'.
      <order_partner>-partn_numb = lv_offer-kunnr.
      <order_partner>-addr_link  = '0000000001'.
    ENDIF.

    LOOP AT lt_vbap ASSIGNING <vbap>.
      order_item_in-itm_number   = <vbap>-posnr.
      order_item_inx-itm_number  = <vbap>-posnr.
      order_item_in-material     = <vbap>-matnr.
      order_item_inx-material    = 'X'.
      order_item_in-ref_doc      = <vbap>-vbeln.  "quote number
      order_item_inx-ref_doc     = 'X'.  "quote number
      order_item_in-ref_doc_it   = <vbap>-posnr.   "quote line item #
      order_item_inx-ref_doc_it  = 'X'.   "quote line item #
      order_item_in-ref_doc_ca   = 'B'.
      order_item_inx-ref_doc_ca  = 'X'.
      APPEND order_item_in TO order_items_in.
      APPEND order_item_inx TO order_items_inx.
    ENDLOOP.

    LOOP AT lt_vbep ASSIGNING <vbep>.
      order_schedule_in-itm_number  = <vbep>-posnr.
      order_schedule_inx-itm_number = <vbep>-posnr.
      order_schedule_in-req_qty     = <vbep>-wmeng.
      order_schedule_inx-req_qty    = abap_true.
      INSERT order_schedule_in  INTO TABLE order_schedules_in.
      INSERT order_schedule_inx INTO TABLE order_schedules_inx.
    ENDLOOP.
  ENDMETHOD.                    "_select

  METHOD _create_order.
    DATA: lt_return TYPE bapiret2tab.

    CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
      EXPORTING
*        salesdocumentin         =     " Sales and Distribution Document Number
        order_header_in         = order_header_in    " Order Header
        order_header_inx        = order_header_inx    " Sales Order Check List
*        sender                  =     " Logical System - Sender
*        binary_relationshiptype =     " Binary Relationship Type (Private)
*        int_number_assignment   =     " Internal Item Number Assignment
*        behave_when_error       =     " Error Handling
*        logic_switch            =     " Internal Control Parameter
*        testrun                 =     " Test Run
*        convert                 = SPACE    " Conversion of Partner Function + Order Type
*      IMPORTING
*        salesdocument           =     " Number of Generated Document
      TABLES
        return                  = lt_return     " Return Messages
        order_items_in          = order_items_in     " Item Data
        order_items_inx         = order_items_inx    " Item Data Checkbox
        order_partners          = order_partners    " Document Partner
        order_schedules_in      = order_schedules_in    " Schedule Line Data
        order_schedules_inx     = order_schedules_inx    " Checkbox Schedule Line Data
*        order_conditions_in     =     " Conditions
*        order_conditions_inx    =     " Conditions Checkbox
*        order_cfgs_ref          =     " Configuration: Reference Data
*        order_cfgs_inst         =     " Configuration: Instances
*        order_cfgs_part_of      =     " Configuration: Part-of Specifications
*        order_cfgs_value        =     " Configuration: Characteristic Values
*        order_cfgs_blob         =     " Configuration: BLOB Internal Data (SCE)
*        order_cfgs_vk           =     " Configuration: Variant Condition Key
*        order_cfgs_refinst      =     " Configuration: Reference Item / Instance
*        order_ccard             =     " Credit Card Data
*        order_text              =     " Texts
*        order_keys              =     " Output Table of Reference Keys
*        extensionin             =     " Customer Enhancement for VBAK, VBAP, VBEP
*        partneraddresses        =     " BAPI Reference Structure for Addresses (Org./Company)
      .

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

    CALL FUNCTION 'ISH_BAPIRET2_DISPLAY'
*      EXPORTING
*        send_if_one   = SPACE    " Message sent directly if number = 1
*        object        = SPACE    " Object for title in message dialog box
*        show_linno    = 'X'    " Also show line numbers
*        amodal_window = ' '    " Also show line numbers
      TABLES
        ss_bapiret2   = lt_return    " Return Parameter(s)
      .

  ENDMETHOD.                    "_create_order
ENDCLASS.                    "lcl_application IMPLEMENTATION

START-OF-SELECTION.
  DATA: lo_application TYPE REF TO lcl_application.
  CREATE OBJECT lo_application.
  lo_application->start( ).
