REPORT z_test_importing.

CLASS lcl_test DEFINITION.
  PUBLIC SECTION.
    METHODS: start.

  PRIVATE SECTION.
    METHODS: importing EXPORTING exporting TYPE string
                                 importing TYPE string.
ENDCLASS.

CLASS lcl_test IMPLEMENTATION.
  METHOD start.
    DATA: test TYPE string.

    importing(
      EXPORTING
        exporting = 'Test'
      IMPORTING
        importing = test ).
  ENDMETHOD.

  METHOD importing.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_test( )->start( ).
