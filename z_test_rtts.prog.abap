REPORT z_test_rtts.

DATA: tabname TYPE string VALUE 'T100',
      ltype   TYPE REF TO data.

CREATE DATA ltype TYPE (tabname).
ASSIGN ltype->* TO FIELD-SYMBOL(<ltype>).
DATA(typedescr) = cl_abap_typedescr=>describe_by_data( <ltype> ).
tabname = typedescr->get_relative_name( ).

cl_demo_output=>display_data( tabname ).
