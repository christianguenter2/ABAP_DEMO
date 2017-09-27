*&---------------------------------------------------------------------*
*& Report  Z_TEST_BOPF_DEMO
*& Created By:   James Wood (jwood@bowdark.com)                        *
*& Created On:   01/16/2013                                            *
*&---------------------------------------------------------------------*
*& Description:  This program demonstrates the BOPF API by showing how *
*&               to perform basic CRUD operations on a sample BO       *
*&               called /BOBF/DEMO_CUSTOMER.                           *
*&---------------------------------------------------------------------*


REPORT Z_TEST_BOPF_DEMO.

*&---------------------------------------------------------------------*
*& Selection Screen Definition                                         *
*&---------------------------------------------------------------------*
PARAMETERS:
  p_custid TYPE /bobf/demo_customer_id.

SELECTION-SCREEN SKIP.

PARAMETERS:
  p_create TYPE c DEFAULT 'X' RADIOBUTTON GROUP mode,
  p_change TYPE c RADIOBUTTON GROUP mode,
  p_view   TYPE c RADIOBUTTON GROUP mode.

*&---------------------------------------------------------------------*
*& Local Class Definitions                                             *
*&---------------------------------------------------------------------*
CLASS lcl_bopf_utils DEFINITION.
  PUBLIC SECTION.
    "Instance Attributes:
    DATA mo_txn_mngr TYPE REF TO /bobf/if_tra_transaction_mgr.
    DATA mo_svc_mngr TYPE REF TO /bobf/if_tra_service_manager.
    DATA mo_bo_conf  TYPE REF TO /bobf/if_frw_configuration.

    CLASS-METHODS:
      display_messages IMPORTING io_message
                            TYPE REF TO /bobf/if_frw_message.

    METHODS:
      constructor IMPORTING iv_bo_key TYPE /bobf/obm_bo_key
                    RAISING /bobf/cx_frw,

      get_node_table
        IMPORTING iv_key TYPE /bobf/conf_key
                  iv_node_key TYPE /bobf/obm_node_key
                  iv_edit_mode TYPE /bobf/conf_edit_mode
                    DEFAULT /bobf/if_conf_c=>sc_edit_read_only
        RETURNING VALUE(rr_data) TYPE REF TO data
          RAISING /bobf/cx_frw,

      get_node_row
        IMPORTING iv_key TYPE /bobf/conf_key
                  iv_node_key TYPE /bobf/obm_node_key
                  iv_edit_mode TYPE /bobf/conf_edit_mode
                    DEFAULT /bobf/if_conf_c=>sc_edit_read_only
                  iv_index TYPE i DEFAULT 1
        RETURNING VALUE(rr_data) TYPE REF TO data
          RAISING /bobf/cx_frw,

      get_node_table_by_assoc
        IMPORTING iv_key TYPE /bobf/conf_key
                  iv_node_key TYPE /bobf/obm_node_key
                  iv_assoc_key TYPE /bobf/obm_assoc_key
                  iv_edit_mode TYPE /bobf/conf_edit_mode
                    DEFAULT /bobf/if_conf_c=>sc_edit_read_only
        RETURNING VALUE(rr_data) TYPE REF TO data
          RAISING /bobf/cx_frw,

      get_node_row_by_assoc
        IMPORTING iv_key TYPE /bobf/conf_key
                  iv_node_key TYPE /bobf/obm_node_key
                  iv_assoc_key TYPE /bobf/obm_assoc_key
                  iv_edit_mode TYPE /bobf/conf_edit_mode
                    DEFAULT /bobf/if_conf_c=>sc_edit_read_only
                  iv_index TYPE i DEFAULT 1
        RETURNING VALUE(rr_data) TYPE REF TO data
          RAISING /bobf/cx_frw,

      get_do_node_table_by_assoc
        IMPORTING iv_key TYPE /bobf/conf_key
                  iv_node_key TYPE /bobf/obm_node_key
                  iv_assoc_name TYPE /bobf/obm_name
                  iv_edit_mode TYPE /bobf/conf_edit_mode
                    DEFAULT /bobf/if_conf_c=>sc_edit_read_only
        RETURNING VALUE(rr_data) TYPE REF TO data
          RAISING /bobf/cx_frw,

      get_do_node_row_by_assoc
        IMPORTING iv_key TYPE /bobf/conf_key
                  iv_node_key TYPE /bobf/obm_node_key
                  iv_assoc_name TYPE /bobf/obm_name
                  iv_edit_mode TYPE /bobf/conf_edit_mode
                    DEFAULT /bobf/if_conf_c=>sc_edit_read_only
                  iv_index TYPE i DEFAULT 1
        RETURNING VALUE(rr_data) TYPE REF TO data
          RAISING /bobf/cx_frw.
ENDCLASS.

CLASS lcl_bopf_utils IMPLEMENTATION.
  METHOD constructor.
    "Obtain a reference to the BOPF transaction manager:
    me->mo_txn_mngr =
      /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

    "Obtain a reference to the BOPF service manager:
    me->mo_svc_mngr =
      /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
        iv_bo_key ).

    "Access the metadata for the BO:
    me->mo_bo_conf =
      /bobf/cl_frw_factory=>get_configuration(
        iv_bo_key ).
  ENDMETHOD.

  METHOD get_node_table.
    "Method-Local Data Declarations:
    DATA lt_key       TYPE /bobf/t_frw_key.
    DATA ls_node_conf TYPE /bobf/s_confro_node.
    DATA lo_change    TYPE REF TO /bobf/if_tra_change.
    DATA lo_message   TYPE REF TO /bobf/if_frw_message.

    FIELD-SYMBOLS <ls_key> LIKE LINE OF lt_key.
    FIELD-SYMBOLS <lt_data> TYPE INDEX TABLE.

    "Lookup the node's configuration:
    CALL METHOD mo_bo_conf->get_node
      EXPORTING
        iv_node_key = iv_node_key
      IMPORTING
        es_node     = ls_node_conf.

    "Use the node configuration metadata to create the result table:
    CREATE DATA rr_data TYPE (ls_node_conf-data_table_type).
    ASSIGN rr_data->* TO <lt_data>.

    "Retrieve the target node:
    APPEND INITIAL LINE TO lt_key ASSIGNING <ls_key>.
    <ls_key>-key = iv_key.

    CALL METHOD mo_svc_mngr->retrieve
      EXPORTING
        iv_node_key = iv_node_key
        it_key      = lt_key
      IMPORTING
        eo_message  = lo_message
        eo_change   = lo_change
        et_data     = <lt_data>.

    "Check the results:
    IF lo_message IS BOUND.
      IF lo_message->check( ) EQ abap_true.
        display_messages( lo_message ).
        RAISE EXCEPTION TYPE /bobf/cx_dac.
      ENDIF.
    ENDIF.
  ENDMETHOD.                 " METHOD get_node_table

  METHOD get_node_row.
    "Method-Local Data Declarations:
    DATA lr_t_data TYPE REF TO data.

    FIELD-SYMBOLS <lt_data> TYPE INDEX TABLE.
    FIELD-SYMBOLS <ls_row> TYPE ANY.

    "Lookup the node data:
    lr_t_data =
      get_node_table( iv_key       = iv_key
                      iv_node_key  = iv_node_key
                      iv_edit_mode = iv_edit_mode ).

    IF lr_t_data IS NOT BOUND.
      RAISE EXCEPTION TYPE /bobf/cx_dac.
    ENDIF.

    "Try to pull the record at the specified index:
    ASSIGN lr_t_data->* TO <lt_data>.
    READ TABLE <lt_data> INDEX iv_index ASSIGNING <ls_row>.
    IF sy-subrc EQ 0.
      GET REFERENCE OF <ls_row> INTO rr_data.
    ELSE.
      RAISE EXCEPTION TYPE /bobf/cx_dac.
    ENDIF.
  ENDMETHOD.                 " METHOD get_node_row

  METHOD get_node_table_by_assoc.
    "Method-Local Data Declarations:
    DATA lt_key         TYPE /bobf/t_frw_key.
    DATA ls_node_conf   TYPE /bobf/s_confro_node.
    DATA ls_association TYPE /bobf/s_confro_assoc.
    DATA lo_change      TYPE REF TO /bobf/if_tra_change.
    DATA lo_message     TYPE REF TO /bobf/if_frw_message.

    FIELD-SYMBOLS <ls_key> LIKE LINE OF lt_key.
    FIELD-SYMBOLS <lt_data> TYPE INDEX TABLE.

    "Lookup the association metadata to find out more
    "information about the target sub-node:
    CALL METHOD mo_bo_conf->get_assoc
      EXPORTING
        iv_assoc_key = iv_assoc_key
        iv_node_key  = iv_node_key
      IMPORTING
        es_assoc     = ls_association.

    IF ls_association-target_node IS NOT BOUND.
      RAISE EXCEPTION TYPE /bobf/cx_dac.
    ENDIF.

    ls_node_conf = ls_association-target_node->*.

    "Use the node configuration metadata to create the result table:
    CREATE DATA rr_data TYPE (ls_node_conf-data_table_type).
    ASSIGN rr_data->* TO <lt_data>.

    "Retrieve the target node:
    APPEND INITIAL LINE TO lt_key ASSIGNING <ls_key>.
    <ls_key>-key = iv_key.

    CALL METHOD mo_svc_mngr->retrieve_by_association
      EXPORTING
        iv_node_key    = iv_node_key
        it_key         = lt_key
        iv_association = iv_assoc_key
        iv_fill_data   = abap_true
      IMPORTING
        eo_message     = lo_message
        eo_change      = lo_change
        et_data        = <lt_data>.

    "Check the results:
    IF lo_message IS BOUND.
      IF lo_message->check( ) EQ abap_true.
        display_messages( lo_message ).
        RAISE EXCEPTION TYPE /bobf/cx_dac.
      ENDIF.
    ENDIF.
  ENDMETHOD.                 " METHOD get_node_table_by_assoc

  METHOD get_node_row_by_assoc.
    "Method-Local Data Declarations:
    DATA lr_t_data TYPE REF TO data.

    FIELD-SYMBOLS <lt_data> TYPE INDEX TABLE.
    FIELD-SYMBOLS <ls_row> TYPE ANY.

    "Lookup the node data:
    lr_t_data =
      get_node_table_by_assoc( iv_key       = iv_key
                               iv_node_key  = iv_node_key
                               iv_assoc_key = iv_assoc_key
                               iv_edit_mode = iv_edit_mode ).

    IF lr_t_data IS NOT BOUND.
      RAISE EXCEPTION TYPE /bobf/cx_dac.
    ENDIF.

    "Try to pull the record at the specified index:
    ASSIGN lr_t_data->* TO <lt_data>.
    READ TABLE <lt_data> INDEX iv_index ASSIGNING <ls_row>.
    IF sy-subrc EQ 0.
      GET REFERENCE OF <ls_row> INTO rr_data.
    ELSE.
      RAISE EXCEPTION TYPE /bobf/cx_dac.
    ENDIF.
  ENDMETHOD.                 " METHOD get_node_row_by_assoc

  METHOD get_do_node_table_by_assoc.
    "Method-Local Data Declarations:
    DATA lv_assoc_key   TYPE /bobf/obm_assoc_key.
    DATA lt_key         TYPE /bobf/t_frw_key.
    DATA ls_node_conf   TYPE /bobf/s_confro_node.
    DATA ls_association TYPE /bobf/s_confro_assoc.
    DATA lo_change      TYPE REF TO /bobf/if_tra_change.
    DATA lo_message     TYPE REF TO /bobf/if_frw_message.

    FIELD-SYMBOLS <ls_key> LIKE LINE OF lt_key.
    FIELD-SYMBOLS <lt_data> TYPE INDEX TABLE.

    "Lookup the association metadata to find out more
    "information about the target sub-node:
    lv_assoc_key =
      me->mo_bo_conf->query_assoc(
        iv_node_key   = iv_node_key
        iv_assoc_name = iv_assoc_name ).

    CALL METHOD me->mo_bo_conf->get_assoc
      EXPORTING
        iv_assoc_key = lv_assoc_key
        iv_node_key  = iv_node_key
      IMPORTING
        es_assoc     = ls_association.

    IF ls_association-target_node IS NOT BOUND.
      RAISE EXCEPTION TYPE /bobf/cx_dac.
    ENDIF.

    ls_node_conf = ls_association-target_node->*.

    "Use the node configuration metadata to create the result table:
    CREATE DATA rr_data TYPE (ls_node_conf-data_table_type).
    ASSIGN rr_data->* TO <lt_data>.

    "Retrieve the target node:
    APPEND INITIAL LINE TO lt_key ASSIGNING <ls_key>.
    <ls_key>-key = iv_key.

    CALL METHOD mo_svc_mngr->retrieve_by_association
      EXPORTING
        iv_node_key    = iv_node_key
        iv_association = ls_association-assoc_key
        it_key         = lt_key
        iv_fill_data   = abap_true
      IMPORTING
        eo_message     = lo_message
        eo_change      = lo_change
        et_data        = <lt_data>.

    "Check the results:
    IF lo_message IS BOUND.
      IF lo_message->check( ) EQ abap_true.
        display_messages( lo_message ).
        RAISE EXCEPTION TYPE /bobf/cx_dac.
      ENDIF.
    ENDIF.
  ENDMETHOD.                 " METHOD get_do_node_table_by_assoc

  METHOD get_do_node_row_by_assoc.
    "Method-Local Data Declarations:
    DATA lr_t_data TYPE REF TO data.

    FIELD-SYMBOLS <lt_data> TYPE INDEX TABLE.
    FIELD-SYMBOLS <ls_row> TYPE ANY.

    "Lookup the node data:
    lr_t_data =
      get_do_node_table_by_assoc( iv_key       = iv_key
                                  iv_node_key  = iv_node_key
                                  iv_assoc_name = iv_assoc_name
                                  iv_edit_mode = iv_edit_mode ).

    IF lr_t_data IS NOT BOUND.
      RAISE EXCEPTION TYPE /bobf/cx_dac.
    ENDIF.

    "Try to pull the record at the specified index:
    ASSIGN lr_t_data->* TO <lt_data>.
    READ TABLE <lt_data> INDEX iv_index ASSIGNING <ls_row>.
    IF sy-subrc EQ 0.
      GET REFERENCE OF <ls_row> INTO rr_data.
    ELSE.
      RAISE EXCEPTION TYPE /bobf/cx_dac.
    ENDIF.
  ENDMETHOD.                 " METHOD get_node_row_by_assoc

  METHOD display_messages.
    "Method-Local Data Declarations:
    DATA lt_messages TYPE /bobf/t_frw_message_k.
    DATA lv_msg_text TYPE string.
    FIELD-SYMBOLS <ls_message> LIKE LINE OF lt_messages.

    "Sanity check:
    CHECK io_message IS BOUND.

    "Output each of the messages in the collection:
    io_message->get_messages( IMPORTING et_message = lt_messages ).
    LOOP AT lt_messages ASSIGNING <ls_message>.
      lv_msg_text = <ls_message>-message->get_text( ).
      WRITE: / lv_msg_text.
    ENDLOOP.
  ENDMETHOD.                 " METHOD display_messages
ENDCLASS.

CLASS lcl_customer_utils DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      create_customer IMPORTING iv_customer_id
                           TYPE /bobf/demo_customer_id,

      change_customer IMPORTING iv_customer_id
                           TYPE /bobf/demo_customer_id,

      display_customer IMPORTING iv_customer_id
                            TYPE /bobf/demo_customer_id.

  PRIVATE SECTION.
    DATA mo_cust_bo TYPE REF TO lcl_bopf_utils.

    METHODS:
      constructor RAISING /bobf/cx_frw,

      create IMPORTING iv_customer_id
                  TYPE /bobf/demo_customer_id,

      change IMPORTING iv_customer_id
                  TYPE /bobf/demo_customer_id,

      display IMPORTING iv_customer_id
                   TYPE /bobf/demo_customer_id,

      get_customer_for_id IMPORTING iv_customer_id
                               TYPE /bobf/demo_customer_id
                          RETURNING VALUE(rv_customer_key)
                               TYPE /bobf/conf_key
                            RAISING /bobf/cx_frw,

      get_customer_long_text
        IMPORTING iv_key TYPE /bobf/conf_key
        RETURNING VALUE(rv_text) TYPE string
          RAISING /bobf/cx_frw.
ENDCLASS.

CLASS lcl_customer_utils IMPLEMENTATION.
  METHOD constructor.
    "Create a customer BO wrapper instance:
    CREATE OBJECT me->mo_cust_bo
      EXPORTING
        iv_bo_key = /bobf/if_demo_customer_c=>sc_bo_key.
  ENDMETHOD.                 " METHOD constructor

  METHOD create_customer.
    "Method-Local Data Declarations:
    DATA lo_customer TYPE REF TO lcl_customer_utils.
    DATA lo_ex TYPE REF TO /bobf/cx_frw.
    DATA lv_message TYPE string.

    TRY.
      "Instantiate the customer BO wrapper class:
      CREATE OBJECT lo_customer.

      "Create the customer record:
      lo_customer->create( iv_customer_id ).
    CATCH /bobf/cx_frw INTO lo_ex.
      lv_message = lo_ex->get_text( ).
      MESSAGE lv_message TYPE 'I'.
    ENDTRY.
  ENDMETHOD.                 " METHOD create_customer

  METHOD change_customer.
    "Method-Local Data Declarations:
    DATA lo_customer TYPE REF TO lcl_customer_utils.
    DATA lo_ex TYPE REF TO /bobf/cx_frw.
    DATA lv_message TYPE string.

    TRY.
      "Instantiate the customer BO wrapper class:
      CREATE OBJECT lo_customer.

      "Update the customer record:
      lo_customer->change( iv_customer_id ).
    CATCH /bobf/cx_frw INTO lo_ex.
      lv_message = lo_ex->get_text( ).
      MESSAGE lv_message TYPE 'I'.
    ENDTRY.
  ENDMETHOD.                 " METHOD change_customer

  METHOD display_customer.
    "Method-Local Data Declarations:
    DATA lo_customer TYPE REF TO lcl_customer_utils.
    DATA lo_ex TYPE REF TO /bobf/cx_frw.
    DATA lv_message TYPE string.

    TRY.
      "Instantiate the customer BO wrapper class:
      CREATE OBJECT lo_customer.

      "Display the customer record:
      lo_customer->display( iv_customer_id ).
    CATCH /bobf/cx_frw INTO lo_ex.
      lv_message = lo_ex->get_text( ).
      MESSAGE lv_message TYPE 'I'.
    ENDTRY.
  ENDMETHOD.                 " METHOD display_customer

  METHOD get_customer_for_id.
    "Method-Local Data Declarations:
    DATA lt_parameters    TYPE /bobf/t_frw_query_selparam.
    DATA lt_customer_keys TYPE /bobf/t_frw_key.
    DATA lx_bopf_ex       TYPE REF TO /bobf/cx_frw.
    DATA lv_err_msg       TYPE string.

    FIELD-SYMBOLS <ls_parameter> LIKE LINE OF lt_parameters.
    FIELD-SYMBOLS <ls_customer_key> LIKE LINE OF lt_customer_keys.

    "Though we could conceivably lookup the customer using an SQL query,
    "the preferred method of selection is a BOPF query:
    APPEND INITIAL LINE TO lt_parameters ASSIGNING <ls_parameter>.
    <ls_parameter>-attribute_name =
      /bobf/if_demo_customer_c=>sc_query_attribute-root-select_by_attributes-customer_id.
    <ls_parameter>-sign           = 'I'.
    <ls_parameter>-option         = 'EQ'.
    <ls_parameter>-low            = iv_customer_id.

    CALL METHOD mo_cust_bo->mo_svc_mngr->query
      EXPORTING
        iv_query_key            =
          /bobf/if_demo_customer_c=>sc_query-root-select_by_attributes
        it_selection_parameters = lt_parameters
      IMPORTING
        et_key                  = lt_customer_keys.

    "Return the matching customer's KEY value:
    READ TABLE lt_customer_keys INDEX 1 ASSIGNING <ls_customer_key>.
    IF sy-subrc EQ 0.
      rv_customer_key = <ls_customer_key>-key.
    ENDIF.
  ENDMETHOD.                 " METHOD get_customer_for_id

  METHOD create.
    "Method-Local Data Declarations:
    DATA lt_mod      TYPE /bobf/t_frw_modification.
    DATA lo_change   TYPE REF TO /bobf/if_tra_change.
    DATA lo_message  TYPE REF TO /bobf/if_frw_message.
    DATA lv_rejected TYPE boole_d.
    DATA lx_bopf_ex  TYPE REF TO /bobf/cx_frw.
    DATA lv_err_msg  TYPE string.

    DATA lr_s_root     TYPE REF TO /bobf/s_demo_customer_hdr_k.
    DATA lr_s_txt      TYPE REF TO /bobf/s_demo_short_text_k.
    DATA lr_s_txt_hdr  TYPE REF TO /bobf/s_demo_longtext_hdr_k.
    DATA lr_s_txt_cont TYPE REF TO /bobf/s_demo_longtext_item_k.

    FIELD-SYMBOLS:
      <ls_mod> LIKE LINE OF lt_mod.

    "Use the BOPF API to create a new customer record:
    TRY.
      "Build the ROOT node:
      CREATE DATA lr_s_root.
      lr_s_root->key = /bobf/cl_frw_factory=>get_new_key( ).
      lr_s_root->customer_id    = iv_customer_id.
      lr_s_root->sales_org      = 'AMER'.
      lr_s_root->cust_curr      = 'USD'.
      lr_s_root->address_contry = 'US'.
      lr_s_root->address        = '1234 Any Street'.

      APPEND INITIAL LINE TO lt_mod ASSIGNING <ls_mod>.
      <ls_mod>-node        = /bobf/if_demo_customer_c=>sc_node-root.
      <ls_mod>-change_mode = /bobf/if_frw_c=>sc_modify_create.
      <ls_mod>-key         = lr_s_root->key.
      <ls_mod>-data        = lr_s_root.

      "Build the ROOT_TEXT node:
      CREATE DATA lr_s_txt.
      lr_s_txt->key      = /bobf/cl_frw_factory=>get_new_key( ).
      lr_s_txt->text     = 'Sample Customer Record'.
      lr_s_txt->language = sy-langu.

      APPEND INITIAL LINE TO lt_mod ASSIGNING <ls_mod>.
      <ls_mod>-node        = /bobf/if_demo_customer_c=>sc_node-root_text.
      <ls_mod>-change_mode = /bobf/if_frw_c=>sc_modify_create.
      <ls_mod>-source_node = /bobf/if_demo_customer_c=>sc_node-root.
      <ls_mod>-association =
        /bobf/if_demo_customer_c=>sc_association-root-root_text.
      <ls_mod>-source_key  = lr_s_root->key.
      <ls_mod>-key         = lr_s_txt->key.
      <ls_mod>-data        = lr_s_txt.

      "Build the ROOT_LONG_TEXT node:
      "If you look at the node type for this node, you'll notice that
      "it's a "Delegated Node". In other words, it is defined in terms
      "of the /BOBF/DEMO_TEXT_COLLECTION business object. The following
      "code accounts for this indirection.
      CREATE DATA lr_s_txt_hdr.
      lr_s_txt_hdr->key = /bobf/cl_frw_factory=>get_new_key( ).

      APPEND INITIAL LINE TO lt_mod ASSIGNING <ls_mod>.
      <ls_mod>-node        =
        /bobf/if_demo_customer_c=>sc_node-root_long_text.
      <ls_mod>-change_mode = /bobf/if_frw_c=>sc_modify_create.
      <ls_mod>-source_node = /bobf/if_demo_customer_c=>sc_node-root.
      <ls_mod>-association =
        /bobf/if_demo_customer_c=>sc_association-root-root_long_text.
      <ls_mod>-source_key  = lr_s_root->key.
      <ls_mod>-key         = lr_s_txt_hdr->key.
      <ls_mod>-data        = lr_s_txt_hdr.

      "Create the CONTENT node:
      CREATE DATA lr_s_txt_cont.
      lr_s_txt_cont->key          = /bobf/cl_frw_factory=>get_new_key( ).
      lr_s_txt_cont->language     = sy-langu.
      lr_s_txt_cont->text_type    = 'MEMO'.
      lr_s_txt_cont->text_content = 'Demo customer created via BOPF API.'.

      APPEND INITIAL LINE TO lt_mod ASSIGNING <ls_mod>.
      <ls_mod>-node =
        me->mo_cust_bo->mo_bo_conf->query_node(
          iv_proxy_node_name = 'ROOT_LONG_TXT.CONTENT' ).
      <ls_mod>-change_mode = /bobf/if_frw_c=>sc_modify_create.
      <ls_mod>-source_node =
        /bobf/if_demo_customer_c=>sc_node-root_long_text.
      <ls_mod>-source_key  = lr_s_txt_hdr->key.
      <ls_mod>-key         = lr_s_txt_cont->key.
      <ls_mod>-data        = lr_s_txt_cont.

      <ls_mod>-association =
        me->mo_cust_bo->mo_bo_conf->query_assoc(
          iv_node_key   = /bobf/if_demo_customer_c=>sc_node-root_long_text
          iv_assoc_name = 'CONTENT' ).

      "Create the customer record:
      CALL METHOD me->mo_cust_bo->mo_svc_mngr->modify
        EXPORTING
          it_modification = lt_mod
        IMPORTING
          eo_change       = lo_change
          eo_message      = lo_message.

      "Check for errors:
      IF lo_message IS BOUND.
        IF lo_message->check( ) EQ abap_true.
          lcl_bopf_utils=>display_messages( lo_message ).
          RETURN.
        ENDIF.
      ENDIF.

      "Apply the transactional changes:
      CALL METHOD me->mo_cust_bo->mo_txn_mngr->save
        IMPORTING
          eo_message  = lo_message
          ev_rejected = lv_rejected.

      IF lv_rejected EQ abap_true.
        lcl_bopf_utils=>display_messages( lo_message ).
        RETURN.
      ENDIF.

      "If we get to here, then the operation was successful:
      WRITE: / 'Customer', iv_customer_id, 'created successfully.'.
    CATCH /bobf/cx_frw INTO lx_bopf_ex.
      lv_err_msg = lx_bopf_ex->get_text( ).
      WRITE: / lv_err_msg.
    ENDTRY.
  ENDMETHOD.                 " METHOD create

  METHOD change.
    "Method-Local Data Declarations:
    DATA lv_customer_key TYPE /bobf/conf_key.
    DATA lt_mod          TYPE /bobf/t_frw_modification.
    DATA lo_change       TYPE REF TO /bobf/if_tra_change.
    DATA lo_message      TYPE REF TO /bobf/if_frw_message.
    DATA lv_rejected     TYPE boole_d.
    DATA lx_bopf_ex      TYPE REF TO /bobf/cx_frw.
    DATA lv_err_msg      TYPE string.

    FIELD-SYMBOLS:
      <ls_mod> LIKE LINE OF lt_mod.

    DATA lr_s_root TYPE REF TO /bobf/s_demo_customer_hdr_k.

    "Try to change the address on the selected customer:
    TRY.
      "Access the customer ROOT node:
      lv_customer_key = get_customer_for_id( iv_customer_id ).

      lr_s_root ?=
        me->mo_cust_bo->get_node_row(
          iv_key = lv_customer_key
          iv_node_key = /bobf/if_demo_customer_c=>sc_node-root
          iv_edit_mode = /bobf/if_conf_c=>sc_edit_exclusive ).

      "Change the address string on the customer:
      lr_s_root->address = '1234 Boardwalk Ave.'.

      APPEND INITIAL LINE TO lt_mod ASSIGNING <ls_mod>.
      <ls_mod>-node        = /bobf/if_demo_customer_c=>sc_node-root.
      <ls_mod>-change_mode = /bobf/if_frw_c=>sc_modify_update.
      <ls_mod>-key         = lr_s_root->key.
      <ls_mod>-data        = lr_s_root.

      "Create the customer record:
      CALL METHOD me->mo_cust_bo->mo_svc_mngr->modify
        EXPORTING
          it_modification = lt_mod
        IMPORTING
          eo_change       = lo_change
          eo_message      = lo_message.

      "Check for errors:
      IF lo_message IS BOUND.
        IF lo_message->check( ) EQ abap_true.
          lcl_bopf_utils=>display_messages( lo_message ).
          RETURN.
        ENDIF.
      ENDIF.

      "Apply the transactional changes:
      CALL METHOD me->mo_cust_bo->mo_txn_mngr->save
        IMPORTING
          eo_message  = lo_message
          ev_rejected = lv_rejected.

      IF lv_rejected EQ abap_true.
        lcl_bopf_utils=>display_messages( lo_message ).
        RETURN.
      ENDIF.

      "If we get to here, then the operation was successful:
      WRITE: / 'Customer', iv_customer_id, 'updated successfully.'.
    CATCH /bobf/cx_frw INTO lx_bopf_ex.
      lv_err_msg = lx_bopf_ex->get_text( ).
      WRITE: / lv_err_msg.
    ENDTRY.
  ENDMETHOD.                 " METHOD change

  METHOD display.
    "Method-Local Data Declarations:
    DATA lv_customer_key TYPE /bobf/conf_key.
    DATA lx_bopf_ex      TYPE REF TO /bobf/cx_frw.
    DATA lv_err_msg      TYPE string.

    DATA lr_s_root TYPE REF TO /bobf/s_demo_customer_hdr_k.
    DATA lr_s_text TYPE REF TO /bobf/s_demo_short_text_k.
    DATA lv_long_text TYPE string.

    "Try to display the selected customer:
    TRY.
      "Lookup the customer's key attribute using a query:
      lv_customer_key = get_customer_for_id( iv_customer_id ).

      "Display the header-level details for the customer:
      lr_s_root ?=
        me->mo_cust_bo->get_node_row(
          iv_key = lv_customer_key
          iv_node_key = /bobf/if_demo_customer_c=>sc_node-root ).

      WRITE: / 'Display Customer', lr_s_root->customer_id.
      ULINE.
      WRITE: / 'Sales Organization:', lr_s_root->sales_org.
      WRITE: / 'Address:', lr_s_root->address.
      SKIP.

      "Traverse to the ROOT_TEXT node to display the customer short text:
      lr_s_text ?=
        me->mo_cust_bo->get_node_row_by_assoc(
          iv_key = lv_customer_key
          iv_node_key = /bobf/if_demo_customer_c=>sc_node-root
          iv_assoc_key =
            /bobf/if_demo_customer_c=>sc_association-root-root_text ).
      WRITE: / 'Short Text:', lr_s_text->text.

      "Read the long text from the ROOT_LONG_TEXT node:
      lv_long_text = get_customer_long_text( lv_customer_key ).
      WRITE: / 'Long Text:', lv_long_text.
    CATCH /bobf/cx_frw INTO lx_bopf_ex.
      lv_err_msg = lx_bopf_ex->get_text( ).
      WRITE: / lv_err_msg.
    ENDTRY.
  ENDMETHOD.                 " METHOD display

  METHOD get_customer_long_text.
    "Method-Local Data Declarations:
    DATA lt_key         TYPE /bobf/t_frw_key.
    DATA lo_message     TYPE REF TO /bobf/if_frw_message.
    DATA lt_key_link    TYPE /bobf/t_frw_key_link.
    DATA lt_target_key  TYPE /bobf/t_frw_key.
    DATA lt_failed_key  TYPE /bobf/t_frw_key.
    DATA ls_node_conf   TYPE /bobf/s_confro_node.
    DATA ls_association TYPE /bobf/s_confro_assoc.
    DATA lr_data        TYPE REF TO data.
    DATA lr_s_text      TYPE REF TO /bobf/s_demo_longtext_item_k.

    FIELD-SYMBOLS <lt_data> TYPE INDEX TABLE.
    FIELD-SYMBOLS <ls_key> LIKE LINE OF lt_key.
    FIELD-SYMBOLS <ls_key_link> LIKE LINE OF lt_key_link.

    "Lookup the association metadata to find out more
    "information about the target sub-node:
    CALL METHOD me->mo_cust_bo->mo_bo_conf->get_assoc
      EXPORTING
        iv_assoc_key =
          /bobf/if_demo_customer_c=>sc_association-root-root_long_text
        iv_node_key  = /bobf/if_demo_customer_c=>sc_node-root
      IMPORTING
        es_assoc     = ls_association.

    IF ls_association-target_node IS NOT BOUND.
      RAISE EXCEPTION TYPE /bobf/cx_dac.
    ENDIF.

    ls_node_conf = ls_association-target_node->*.

    "Use the node configuration metadata to create the result table:
    CREATE DATA lr_data TYPE (ls_node_conf-data_table_type).
    ASSIGN lr_data->* TO <lt_data>.

    "Retrieve the target node:
    APPEND INITIAL LINE TO lt_key ASSIGNING <ls_key>.
    <ls_key>-key = iv_key.

    CALL METHOD me->mo_cust_bo->mo_svc_mngr->retrieve_by_association
      EXPORTING
        iv_node_key    = /bobf/if_demo_customer_c=>sc_node-root
        it_key         = lt_key
        iv_association =
          /bobf/if_demo_customer_c=>sc_association-root-root_long_text
        iv_fill_data   = abap_true
      IMPORTING
        eo_message     = lo_message
        et_data        = <lt_data>
        et_key_link    = lt_key_link
        et_target_key  = lt_target_key
        et_failed_key  = lt_failed_key.

    "Check the results:
    IF lo_message IS BOUND.
      IF lo_message->check( ) EQ abap_true.
        lcl_bopf_utils=>display_messages( lo_message ).
        RAISE EXCEPTION TYPE /bobf/cx_dac.
      ENDIF.
    ENDIF.

    "Read the long text from the /BOBF/DEMO_TEXT_COLLECTION business
    "object:
    READ TABLE lt_key_link INDEX 1 ASSIGNING <ls_key_link>.
    IF sy-subrc NE 0.
      "No text was found...
      RETURN.
    ENDIF.

    lr_s_text ?=
      me->mo_cust_bo->get_do_node_row_by_assoc(
        iv_key = <ls_key_link>-target_key
        iv_node_key =
          /bobf/if_demo_customer_c=>sc_node-root_long_text
        iv_assoc_name = 'CONTENT' ).

    IF lr_s_text IS BOUND.
      rv_text = lr_s_text->text_content.
    ENDIF.
  ENDMETHOD.                 " METHOD get_customer_long_text
ENDCLASS.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION Event Module                                     *
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  "Run the demo program:
  IF p_create EQ abap_true.
    lcl_customer_utils=>create_customer( iv_customer_id = p_custid ).
  ELSEIF p_change EQ abap_true.
    lcl_customer_utils=>change_customer( iv_customer_id = p_custid ).
  ELSE.
    lcl_customer_utils=>display_customer( iv_customer_id = p_custid ).
  ENDIF.
