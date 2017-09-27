class ZCL_MAT_V_DOMAIN_FIX_VALUES definition
  public
  inheriting from /BOBF/CL_LIB_V_SUPERCLASS
  final
  create public .

public section.

  methods /BOBF/IF_FRW_VALIDATION~EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MAT_V_DOMAIN_FIX_VALUES IMPLEMENTATION.


METHOD /bobf/if_frw_validation~execute.
  DATA: table         TYPE zmat_t_root,
        message       TYPE symsg,
        lv_failed_key LIKE LINE OF it_key.

  FIELD-SYMBOLS: <line> LIKE LINE OF table.

  io_read->retrieve(
    EXPORTING
      iv_node                 = is_ctx-node_key
      it_key                  = it_key
    IMPORTING
      eo_message              = eo_message
      et_data                 = table
      et_failed_key           = et_failed_key ).

  LOOP AT table ASSIGNING <line>.
    cl_reca_ddic_doma=>check_value(
      EXPORTING
        id_value  = <line>-mat_type
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2 ).

    IF sy-subrc <> 0.
      lv_failed_key-key = <line>-key.
      APPEND lv_failed_key TO et_failed_key.

      message-msgty = 'E'.
      message-msgid = 'SD'.
      message-msgno = '024'.
      message-msgv1 = 'Bitte gültige Werte eingeben'.

      IF eo_message IS NOT BOUND.
        eo_message = /bobf/cl_frw_factory=>get_message( ).
      ENDIF.

      eo_message->add_message(
        EXPORTING
          is_msg       = message
          iv_node      = is_ctx-node_key
          iv_key       = <line>-key ).
    ENDIF.
  ENDLOOP.
ENDMETHOD.
ENDCLASS.
