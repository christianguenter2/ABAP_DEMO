REPORT z_test_livecoding.

CLASS lcl_test_livecoding DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

ENDCLASS.

CLASS lcl_test_livecoding IMPLEMENTATION.

  METHOD start.
    cl_demo_output=>display( |Hello World!| ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_test_livecoding( )->start( ).
