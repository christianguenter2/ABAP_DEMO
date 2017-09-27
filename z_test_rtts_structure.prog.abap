*&---------------------------------------------------------------------*
*& Report z_test_rtts_structure
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_rtts_structure.

DATA(lo_struct_descr) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( 'PA0002' ) ).

DATA(fieldlist) = lo_struct_descr->get_ddic_field_list( p_including_substructres = abap_true ).
DATA(components) = lo_struct_descr->get_components( ).
DATA(components2) = lo_struct_descr->components.

cl_demo_output=>write( fieldlist ).
cl_demo_output=>write( components ).
cl_demo_output=>write( components2 ).
cl_demo_output=>display( ).
