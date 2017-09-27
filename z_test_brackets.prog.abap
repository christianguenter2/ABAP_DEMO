REPORT z_test_brackets.

CLASS lcl_test_brackets DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: start.
ENDCLASS.

CLASS lcl_test_brackets IMPLEMENTATION.
  METHOD start.
    DATA(text) = 'Test'.

    text = |text { 5 }|.

    cl_demo_output=>display( text ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_test_brackets( )->start( ).
