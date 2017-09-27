REPORT z_test_reduce.

CLASS lcl_test DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: start.
  PRIVATE SECTION.
    METHODS get_some_ints
      RETURNING
        VALUE(ints) TYPE int4_table.
ENDCLASS.

CLASS lcl_test IMPLEMENTATION.
  METHOD start.
    DATA(sum) = REDUCE i( INIT result = 0
                          FOR wa IN get_some_ints( )
                          NEXT result = result + wa ).
    cl_demo_output=>display( sum ).
  ENDMETHOD.

  METHOD get_some_ints.
    ints = VALUE int4_table( ( 1 ) ( 2 ) ( 3 ) ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_test( )->start( ).
