CLASS zcl_test_where_used DEFINITION PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

ENDCLASS.

CLASS zcl_test_where_used IMPLEMENTATION.

  METHOD start.

    NEW lcl_test_1( )->lif_test~test( ).

  ENDMETHOD.

ENDCLASS.
