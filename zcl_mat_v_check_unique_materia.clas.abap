class ZCL_MAT_V_CHECK_UNIQUE_MATERIA definition
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



CLASS ZCL_MAT_V_CHECK_UNIQUE_MATERIA IMPLEMENTATION.


METHOD /bobf/if_frw_validation~execute.
  DATA: lo_message    TYPE REF TO /bobf/if_frw_message,
        lt_data       TYPE zmat_t_material_link,
        lt_key_link   TYPE /bobf/t_frw_key_link,
        lv_failed_key LIKE LINE OF et_failed_key,
        message       TYPE symsg,
        counter       TYPE i.

  FIELD-SYMBOLS: <data>  LIKE LINE OF lt_data,
                 <data2> LIKE LINE OF lt_data.

  io_read->retrieve_by_association(
    EXPORTING
      iv_node                 = is_ctx-node_key
      it_key                  = it_key
      iv_association          = zif_mat_z_bob_material1_c=>sc_association-root-material_link
      iv_fill_data            = abap_true
    IMPORTING
      eo_message              = lo_message
      et_data                 = lt_data
      et_key_link             = lt_key_link ).

  LOOP AT lt_data ASSIGNING <data>.
    counter = 0.
    LOOP AT lt_data ASSIGNING <data2>
                    WHERE matnr = <data>-matnr.
      counter = counter + 1.
      IF counter > 1.
        lv_failed_key-key = <data2>-key.
        APPEND lv_failed_key TO et_failed_key.

        message-msgty = 'E'.
        message-msgid = 'SD'.
        message-msgno = '024'.
        message-msgv1 = `Material ` && <data2>-matnr && ` bereits verknüpft`.

        IF eo_message IS NOT BOUND.
          eo_message = /bobf/cl_frw_factory=>get_message( ).
        ENDIF.

        IF eo_message IS NOT BOUND.
          eo_message = /bobf/cl_frw_factory=>get_message( ).
        ENDIF.

        eo_message->add_message(
          EXPORTING
            is_msg       = message
            iv_node      = is_ctx-node_key
            iv_key       = <data2>-key ).

        RETURN.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDMETHOD.
ENDCLASS.
