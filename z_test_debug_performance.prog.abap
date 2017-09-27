*&---------------------------------------------------------------------*
*& Report  Z_TEST_DEBUG_PERFORMANCE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_debug_performance.

DATA: source_info TYPE tpda_curr_source_pos,
      source_tab  TYPE tpda_ast_src_it.

source_info-include = 'MV45AFZZ'.
source_info-line    = '758'.

DO 1000 TIMES.
  zcl_bc_debugger_scripts=>get_source_info_extended(
    EXPORTING
      i_source_info  = source_info
    CHANGING
      ct_source      = source_tab ).
ENDDO.
