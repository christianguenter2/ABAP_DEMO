REPORT z_test_script.

DATA: source_tab TYPE tpda_ast_src_it.

zcl_bc_debugger_scripts=>get_source_info_extended(
  EXPORTING
    i_source_info  = VALUE #( include = 'Z_TEST_DEBUG'
                              line    = '78' )
    i_filter       = VALUE #( ( low = 'sel' ) )
  CHANGING
    ct_source      = source_tab ).

cl_demo_output=>display( source_tab ).
