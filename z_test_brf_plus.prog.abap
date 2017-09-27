*&---------------------------------------------------------------------*
*& Report  Z_TEST_BRF_PLUS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_brf_plus.

CONSTANTS: co_function_id TYPE if_fdt_types=>id VALUE '539A1834EE751490E10080000A060165'.

DATA: lo_function         TYPE REF TO if_fdt_function,
      lo_context          TYPE REF TO if_fdt_context,
      lo_result           TYPE REF TO if_fdt_result,
      lx_fdt              TYPE REF TO cx_fdt,
      lv_stat_chg_allowed TYPE abap_bool.

lo_function = cl_fdt_factory=>if_fdt_factory~get_instance( )->get_function( co_function_id ).

TRY.
    lo_context = lo_function->get_process_context( ).
    lo_context->set_value( iv_name  = 'DOKAR'
                           ia_value = 'PDR' ).
    lo_context->set_value( iv_name  = 'DOKST'
                           ia_value = 'IA' ).
    lo_context->set_value( iv_name = 'Z_EHS_MAT_STATUS'
                           ia_value = 'FR' ).
    lo_context->set_value( iv_name = 'Z_EHS_MAT_USAGE'
                           ia_value = '01' ).
    lo_function->process(
      EXPORTING
        io_context    = lo_context

      IMPORTING
        eo_result     = lo_result ).

    lo_result->get_value(
      IMPORTING
        ea_value = lv_stat_chg_allowed ).

  CATCH cx_fdt INTO lx_fdt.
    MESSAGE lx_fdt TYPE 'E'.
ENDTRY.

WRITE: / 'Statuswechsel erlaubt: ', lv_stat_chg_allowed.

*cl_fdt_wd_factory=>if_fdt_wd_factory~get_instance( )->get_ui_execution( )->execute_object_manager(
*    iv_id           = '539A021BF7F21DC0E10080000A060165'
**    iv_timestamp    = iv_timestamp
*    iv_display_mode = if_fdt_wd_constants=>gc_edit_mode
*).
