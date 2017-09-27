REPORT z_test_type_c.

DATA: c TYPE c,
      s TYPE string.

FIELD-SYMBOLS: <c> TYPE clike.

s = `Dies ist ein Test...`.
c = `Dies ist ein Test...`.

ASSIGN s TO <c>.

cl_demo_output=>write_data( s ).
cl_demo_output=>write_data( c ).
cl_demo_output=>write_data( <c> ).
cl_demo_output=>display( ).
