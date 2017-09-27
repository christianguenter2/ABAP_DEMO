*&---------------------------------------------------------------------*
*& Report z_test_read_report
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_read_report.

START-OF-SELECTION.

  DATA(parser) = NEW zcl_bc_abap_parser( 'Z_TEST_READ_REPORT2' ).

  DATA(type_name) = parser->get_type( i_structure_name = 'TY_DATA'
                                      i_field_name     = 'NETWR' ).

  DATA(ref_info) = parser->get_reference_information( type_name ).

  DATA(field) = parser->get_field_for_type( i_structure_name = 'TY_DATA'
                                            i_type           = ref_info-tabname && '-' && ref_info-fieldname ).

  cl_demo_output=>write( ref_info ).
  cl_demo_output=>write( field ).
  cl_demo_output=>display( ).
