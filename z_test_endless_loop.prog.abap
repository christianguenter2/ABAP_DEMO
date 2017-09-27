REPORT z_test_endless_loop.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      run.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD run.

    WHILE 1 = 1.

    ENDWHILE.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  controller=>run( ).
