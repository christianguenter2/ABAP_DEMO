REPORT z_test_conversion_exit.

DATA: text TYPE string.

text = |Dies ist ein Test { zcl_conversion_exit=>out( `0000123456`) } hier gehts weiter.\t| &&
       |Dies ist ein Test { zcl_conversion_exit=>out( `00000000000000000123456`) } hier gehts weiter.|.

DATA: lo_text TYPE REF TO cl_demo_text.

lo_text = cl_demo_text=>get_handle( ).

lo_text->add_line( |{ text }| ).
lo_text->display( ).
