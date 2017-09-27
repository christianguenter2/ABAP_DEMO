REPORT z_test_demo_out.

DATA: answer TYPE i VALUE 42,
      name   TYPE string VALUE 'Christian',
      text   TYPE string.

text = |Hallo { name },\nAntwort auf alle Fragen ist\n\t{ answer }.|.

cl_demo_output=>display( text ).
