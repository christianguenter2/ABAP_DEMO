*----------------------------------------------------------------------*
*       CLASS ZCL_BOPF_MATERIAL_MODEL DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_BOPF_MATERIAL_MODEL definition
  public
  create public .

public section.

  data ROOT type ZMAT_S_ROOT read-only .
  data LT_MATERIAL_LINK type ZMAT_T_MATERIAL_LINK read-only .
  data EDIT_MODE type /BOBF/CONF_EDIT_MODE read-only .

  methods CONSTRUCTOR
    raising
      /BOBF/CX_FRW .
  methods CREATE
    importing
      !I_MAT type ZMAT_S_ROOT_D
    raising
      /BOBF/CX_FRW .
  methods GET_MATERIAL_FOR_MAT_NO
    importing
      !I_MAT_NO type ZMAT_D_ROOT-MAT_NO
      !I_MAT_VERSION type ZMAT_D_ROOT-MAT_VERSION
    returning
      value(R_KEY) type /BOBF/CONF_KEY
    raising
      /BOBF/CX_FRW .
  interface /BOBF/IF_CONF_C load .
  methods LOAD
    importing
      !I_MAT_NO type ZMAT_D_ROOT-MAT_NO
      !I_MAT_VERSION type ZMAT_D_ROOT-MAT_VERSION
      !I_EDIT_MODE type /BOBF/CONF_EDIT_MODE default /BOBF/IF_CONF_C=>SC_EDIT_READ_ONLY
    raising
      /BOBF/CX_FRW .
  methods DISPLAY_MESSAGES
    importing
      !IO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE .
  methods SAVE
    raising
      /BOBF/CX_FRW .
  methods INIT .
  methods HAS_CHANGES
    returning
      value(R_HAS_CHANGES) type ABAP_BOOL .
  methods SET_HEAD
    importing
      !I_HEAD type ZMAT_S_ROOT
    raising
      /BOBF/CX_FRW .
  methods GET_OBLIGATORY_FIELDS
    returning
      value(R_OBLIGATORY_FIELDS) type STRINGTAB .
  methods ADD_MATERIAL_LINK
    importing
      !I_MATNR type MATNR
    raising
      /BOBF/CX_FRW .
  methods DELETE_MATERIAL_LINK
    importing
      !I_MATERIAL_LINK type ZMAT_S_MATERIAL_LINK
    raising
      /BOBF/CX_FRW .
  methods DOCS_CHANGED .
  PROTECTED SECTION.
private section.

  data TRANSACTION_MANAGER type ref to /BOBF/IF_TRA_TRANSACTION_MGR .
  data SERVICE_MANAGER type ref to /BOBF/IF_TRA_SERVICE_MANAGER .
  data CONFIGURATION type ref to /BOBF/IF_FRW_CONFIGURATION .
  data _DIRTY type ABAP_BOOL .
  data _DB_KEY type /BOBF/CONF_KEY .

  methods _LOAD_MATERIAL_LINKS_BY_KEY
    importing
      !I_KEY type /BOBF/CONF_KEY
    raising
      /BOBF/CX_FRW .
  methods _LOAD_BY_KEY
    importing
      !I_KEY type /BOBF/CONF_KEY
    raising
      /BOBF/CX_FRW .
  methods _MODIFY
    raising
      /BOBF/CX_FRW .
ENDCLASS.



CLASS ZCL_BOPF_MATERIAL_MODEL IMPLEMENTATION.


METHOD add_material_link.
  DATA: lt_mod             TYPE /bobf/t_frw_modification,
        ls_mod             LIKE LINE OF lt_mod,
        lo_change          TYPE REF TO /bobf/if_tra_change,
        lo_message         TYPE REF TO /bobf/if_frw_message,
        lr_s_material_link TYPE REF TO zmat_d_material.

  CHECK edit_mode <> /bobf/if_conf_c=>sc_edit_read_only.

  CREATE DATA lr_s_material_link.
  lr_s_material_link->db_key     = /bobf/cl_frw_factory=>get_new_key( ).
  lr_s_material_link->parent_key = _db_key.
  lr_s_material_link->matnr      = i_matnr.

  ls_mod-node        = zif_mat_z_bob_material1_c=>sc_node-material_link.
  ls_mod-source_node = zif_mat_z_bob_material1_c=>sc_node-root.
  ls_mod-source_key  = _db_key.
  ls_mod-association = zif_mat_z_bob_material1_c=>sc_association-root-material_link.
  ls_mod-change_mode = /bobf/if_frw_c=>sc_modify_create.
  ls_mod-key         = lr_s_material_link->db_key.
  ls_mod-data        = lr_s_material_link.
  INSERT ls_mod INTO TABLE lt_mod.
  CLEAR ls_mod.

  service_manager->modify(
    EXPORTING
      it_modification = lt_mod
    IMPORTING
      eo_change       = lo_change
      eo_message      = lo_message ).

  IF lo_message IS BOUND.
    IF lo_message->check( ) = abap_true.
      RAISE EXCEPTION TYPE /bobf/cx_frw_core
        EXPORTING
          mo_message = lo_message.
    ENDIF.
  ENDIF.

  _load_material_links_by_key( _db_key ).

  _dirty = abap_true.
ENDMETHOD.


  METHOD constructor.
    transaction_manager = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).
    service_manager     = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( zif_mat_z_bob_material1_c=>sc_bo_key ).
    configuration       = /bobf/cl_frw_factory=>get_configuration( zif_mat_z_bob_material1_c=>sc_bo_key ).

    transaction_manager->cleanup( ).
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD create.
    DATA: lt_mod             TYPE /bobf/t_frw_modification,
          ls_mod             LIKE LINE OF lt_mod,
          lo_change          TYPE REF TO /bobf/if_tra_change,
          lo_message         TYPE REF TO /bobf/if_frw_message,
          lr_s_root          TYPE REF TO zmat_d_root,
          lr_s_material_link TYPE REF TO zmat_d_material,
          rejected           TYPE boole_d.

    CREATE DATA lr_s_root.
    lr_s_root->db_key = /bobf/cl_frw_factory=>get_new_key( ).
    MOVE-CORRESPONDING i_mat TO lr_s_root->*.

    lr_s_root->mat_version = '01'.

    ls_mod-node        = zif_mat_z_bob_material1_c=>sc_node-root.
    ls_mod-change_mode = /bobf/if_frw_c=>sc_modify_create.
    ls_mod-key         = lr_s_root->db_key.
    ls_mod-data        = lr_s_root.
    INSERT ls_mod INTO TABLE lt_mod.

    service_manager->modify(
      EXPORTING
        it_modification = lt_mod
      IMPORTING
        eo_change       = lo_change
        eo_message      = lo_message ).

    IF lo_message IS BOUND.
      IF lo_message->check( ) = abap_true.
        RAISE EXCEPTION TYPE /bobf/cx_frw_core
          EXPORTING
            mo_message = lo_message.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING lr_s_root->* TO root.

    _db_key = lr_s_root->db_key.

    _dirty = abap_true.
  ENDMETHOD.                    "CREATE


METHOD delete_material_link.
  DATA: lt_mod             TYPE /bobf/t_frw_modification,
        ls_mod             LIKE LINE OF lt_mod,
        lo_change          TYPE REF TO /bobf/if_tra_change,
        lo_message         TYPE REF TO /bobf/if_frw_message,
        lr_s_material_link TYPE REF TO zmat_d_material.

  CHECK edit_mode <> /bobf/if_conf_c=>sc_edit_read_only.

  CREATE DATA lr_s_material_link.
  MOVE-CORRESPONDING i_material_link TO lr_s_material_link->*.
  lr_s_material_link->db_key = i_material_link-key.

  ls_mod-node        = zif_mat_z_bob_material1_c=>sc_node-material_link.
  ls_mod-source_node = zif_mat_z_bob_material1_c=>sc_node-root.
  ls_mod-source_key  = _db_key.
  ls_mod-association = zif_mat_z_bob_material1_c=>sc_association-root-material_link.
  ls_mod-change_mode = /bobf/if_frw_c=>sc_modify_delete.
  ls_mod-key         = lr_s_material_link->db_key.
  ls_mod-data        = lr_s_material_link.
  INSERT ls_mod INTO TABLE lt_mod.
  CLEAR ls_mod.

  service_manager->modify(
    EXPORTING
      it_modification = lt_mod
    IMPORTING
      eo_change       = lo_change
      eo_message      = lo_message ).

  IF lo_message IS BOUND.
    IF lo_message->check( ) = abap_true.
      RAISE EXCEPTION TYPE /bobf/cx_frw_core
        EXPORTING
          mo_message = lo_message.
    ENDIF.
  ENDIF.

  _load_material_links_by_key( _db_key ).

  _dirty = abap_true.
ENDMETHOD.


  METHOD display_messages.

    DATA  lt_messages TYPE /bobf/t_frw_message_k.
    DATA  lv_msg_text TYPE string.
    DATA: text        TYPE string.
    DATA: text2       TYPE string.
    FIELD-SYMBOLS <ls_message> LIKE LINE OF lt_messages.

    CHECK io_message IS BOUND.

    io_message->get_messages( IMPORTING et_message = lt_messages ).
    LOOP AT lt_messages ASSIGNING <ls_message>.
      lv_msg_text = <ls_message>-message->get_text( ).
      IF sy-tabix = 1.
        text = lv_msg_text.
      ELSE.
        text2 = lv_msg_text.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Fehler'
        txt1  = text
        txt2  = text2.

  ENDMETHOD.                    "DISPLAY_MESSAGES


METHOD docs_changed.
  _dirty = abap_true.
ENDMETHOD.


  METHOD get_material_for_mat_no.

    DATA: lt_data       TYPE zmat_t_root,
          lt_parameters TYPE /bobf/t_frw_query_selparam,
          lo_message    TYPE REF TO /bobf/if_frw_message.

    FIELD-SYMBOLS: <data>  LIKE LINE OF lt_data,
                   <param> LIKE LINE OF lt_parameters.

    APPEND INITIAL LINE TO lt_parameters ASSIGNING <param>.
    <param>-attribute_name = zif_mat_z_bob_material1_c=>sc_query_attribute-root-select_by_mat_no-mat_no.
    <param>-sign           = 'I'.
    <param>-option         = 'EQ'.
    <param>-low            = i_mat_no.

    APPEND INITIAL LINE TO lt_parameters ASSIGNING <param>.
    <param>-attribute_name = zif_mat_z_bob_material1_c=>sc_query_attribute-root-select_by_mat_no-mat_version.
    <param>-sign           = 'I'.
    <param>-option         = 'EQ'.
    <param>-low            = i_mat_version.

    service_manager->query(
      EXPORTING
        iv_query_key            = zif_mat_z_bob_material1_c=>sc_query-root-select_by_mat_no
        iv_fill_data            = abap_true
        it_selection_parameters = lt_parameters
      IMPORTING
        et_data                 = lt_data
        eo_message              = lo_message ).

    IF lo_message IS BOUND.
      IF lo_message->check( ) = abap_true.
        RAISE EXCEPTION TYPE /bobf/cx_frw_core
          EXPORTING
            mo_message = lo_message.
      ENDIF.
    ENDIF.

    READ TABLE lt_data ASSIGNING <data> INDEX 1.
    IF sy-subrc = 0.
      r_key = <data>-key.
    ENDIF.

  ENDMETHOD.                    "GET_MATERIAL_FOR_MAT_NO


METHOD get_obligatory_fields.
  DATA: lo_bopf     TYPE REF TO /bobf/if_frw_service_layer,
        lt_key      TYPE /bobf/t_frw_key,
        lo_property	TYPE REF TO	/bobf/if_frw_property,
        lo_message  TYPE REF TO /bobf/if_frw_message,
        lt_property TYPE /bobf/t_confro_property_d.

  FIELD-SYMBOLS: <property> LIKE LINE OF lt_property.

  lo_bopf = /bobf/cl_frw_factory=>get_bopf( zif_mat_z_bob_material1_c=>sc_bo_key ).

  configuration->get_property(
    EXPORTING
      iv_node_cat_key = zif_mat_z_bob_material1_c=>sc_node_category-root-root
    IMPORTING
      et_property     = lt_property ).

  DELETE lt_property
    WHERE property_name <> /bobf/if_conf_c=>sc_property_name_mandatory
       OR value         = abap_false.                    "#EC CI_STDSEQ

  LOOP AT lt_property ASSIGNING <property>.
    INSERT <property>-attribute_name INTO TABLE r_obligatory_fields.
  ENDLOOP.
ENDMETHOD.


  METHOD has_changes.
    r_has_changes = _dirty.
  ENDMETHOD.                    "has_changes


  METHOD init.
    transaction_manager->cleanup( ).

    CLEAR: service_manager,
           transaction_manager,
           configuration,
           root,
           _db_key,
           _dirty.
  ENDMETHOD.                    "init


  METHOD load.
    _db_key = get_material_for_mat_no( i_mat_no      = i_mat_no
                                       i_mat_version = i_mat_version ).

    edit_mode = i_edit_mode.

    _load_by_key( _db_key ).
  ENDMETHOD.                    "LOAD


  METHOD save.
    DATA: lo_message TYPE REF TO /bobf/if_frw_message,
          lo_change  TYPE REF TO /bobf/if_tra_change,
          rejected   TYPE boole_d.

    transaction_manager->save(
      IMPORTING
        ev_rejected            = rejected
        eo_message             = lo_message
        eo_change              = lo_change ).

    IF rejected = abap_true.
      RAISE EXCEPTION TYPE /bobf/cx_frw_core
        EXPORTING
          mo_message = lo_message.
    ENDIF.

    _load_by_key( _db_key ).

    CLEAR _dirty.
  ENDMETHOD.                    "save


METHOD set_head.
  DATA: something_changed TYPE abap_bool.

  _set_data: mat_type,
             mat_subtype,
             mat_expression,
             mat_txt.

  IF something_changed = abap_true.
    _modify( ).
  ENDIF.
ENDMETHOD.                    "set_head


METHOD _load_by_key.
  DATA: lt_key        TYPE /bobf/t_frw_key,
        lt_failed_key TYPE /bobf/t_frw_key,
        lv_key        LIKE LINE OF lt_key,
        lo_message    TYPE REF TO /bobf/if_frw_message,
        lt_data       TYPE zmat_t_root.

  lv_key-key = i_key.
  INSERT lv_key INTO TABLE lt_key.

  service_manager->retrieve(
    EXPORTING
      iv_node_key             = zif_mat_z_bob_material1_c=>sc_node-root
      it_key                  = lt_key
      iv_edit_mode            = edit_mode
    IMPORTING
      eo_message              = lo_message
      et_data                 = lt_data
      et_failed_key           = lt_failed_key ).

  IF lo_message IS BOUND.
    IF lo_message->check( ) = abap_true.
      RAISE EXCEPTION TYPE /bobf/cx_frw_core
        EXPORTING
          mo_message = lo_message.
    ENDIF.
  ENDIF.

  IF lines( lt_failed_key ) > 0.
    DATA: msg TYPE symsg.
    msg-msgty = 'E'.
    msg-msgno = '002'.
    msg-msgid = '/BOBF/FRW'.
    msg-msgv1 = 'Werkstoff nicht vorhanden'.

    lo_message->add_message(
      EXPORTING
        is_msg       = msg ).

    RAISE EXCEPTION TYPE /bobf/cx_frw_core
      EXPORTING
        mo_message = lo_message.
  ENDIF.

  READ TABLE lt_data INTO root INDEX 1.

  _load_material_links_by_key( i_key ).
ENDMETHOD.


METHOD _load_material_links_by_key.
  DATA: lt_key         TYPE /bobf/t_frw_key,
        lt_failed_key  TYPE /bobf/t_frw_key,
        lv_key         LIKE LINE OF lt_key,
        lo_message     TYPE REF TO /bobf/if_frw_message,
        lv_association TYPE /bobf/obm_assoc_key.

  lv_key-key = i_key.
  INSERT lv_key INTO TABLE lt_key.

  service_manager->retrieve_by_association(
    EXPORTING
      iv_node_key             = zif_mat_z_bob_material1_c=>sc_node-root
      it_key                  = lt_key
      iv_association          = zif_mat_z_bob_material1_c=>sc_association-root-material_link
      iv_fill_data            = abap_true
      iv_edit_mode            = edit_mode
    IMPORTING
      eo_message              = lo_message
      et_data                 = lt_material_link
      et_failed_key           = lt_failed_key ).

  IF lo_message IS BOUND.
    IF lo_message->check( ) = abap_true.
      RAISE EXCEPTION TYPE /bobf/cx_frw_core
        EXPORTING
          mo_message = lo_message.
    ENDIF.
  ENDIF.
ENDMETHOD.


  METHOD _modify.
    DATA: lt_mod             TYPE /bobf/t_frw_modification,
          ls_mod             LIKE LINE OF lt_mod,
          lo_change          TYPE REF TO /bobf/if_tra_change,
          lo_message         TYPE REF TO /bobf/if_frw_message,
          lr_s_root          TYPE REF TO zmat_d_root,
          lr_s_material_link TYPE REF TO zmat_d_material.

    _dirty = abap_true.

    CREATE DATA lr_s_root.
    MOVE-CORRESPONDING root TO lr_s_root->*.
    lr_s_root->db_key = root-key.

    ls_mod-node        = zif_mat_z_bob_material1_c=>sc_node-root.
    ls_mod-change_mode = /bobf/if_frw_c=>sc_modify_update.
    ls_mod-key         = lr_s_root->db_key.
    ls_mod-data        = lr_s_root.
    INSERT ls_mod INTO TABLE lt_mod.

    service_manager->modify(
      EXPORTING
        it_modification = lt_mod
      IMPORTING
        eo_change       = lo_change
        eo_message      = lo_message ).

    IF lo_message IS BOUND.
      IF lo_message->check( ) = abap_true.
        RAISE EXCEPTION TYPE /bobf/cx_frw_core
          EXPORTING
            mo_message = lo_message.
      ENDIF.
    ENDIF.

    IF lines( lo_change->get_changes( ) ) > 0.
      load(
        EXPORTING
          i_mat_no      = root-mat_no    " I_MAT_NO
          i_mat_version = root-mat_version ).
    ENDIF.
  ENDMETHOD.                    "_modify
ENDCLASS.
