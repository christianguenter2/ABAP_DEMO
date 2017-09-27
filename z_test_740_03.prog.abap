REPORT z_test_740_03.

CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS: start.
ENDCLASS.

CLASS lcl_application IMPLEMENTATION.
  METHOD start.
    cl_demo_output=>display_data( `Dies ist ein test` ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_application( )->start( ).
