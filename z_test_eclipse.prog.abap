REPORT z_test_eclipse.

CLASS lcl_test DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: start.
  PRIVATE SECTION.

    METHODS _select.
    METHODS _process.
    METHODS _display.
ENDCLASS.

CLASS lcl_test IMPLEMENTATION.
  METHOD start.
    _select( ).
    _process( ).
    _display( ).
  ENDMETHOD.

  METHOD _select.

  ENDMETHOD.

  METHOD _process.


  ENDMETHOD.

  METHOD _display.
    cl_demo_output=>display( `Dies ist ein Test`).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_test( )->start( ).
