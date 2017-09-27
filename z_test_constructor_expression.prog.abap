REPORT z_test_constructor_expression.

CLASS test_expression DEFINITION.
  PUBLIC SECTION.
    METHODS: test IMPORTING table TYPE ANY TABLE.
ENDCLASS.

CLASS test_expression IMPLEMENTATION.
  METHOD test.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  TYPES: BEGIN OF ty_data,
           i TYPE i,
           s TYPE string,
         END OF ty_data,
         tty_data TYPE HASHED TABLE OF ty_data
                  WITH UNIQUE KEY i.

  DATA(items) = VALUE tty_data( ( i = 1 s = 'Test' )
                                ( i = 2 s = 'Hello World' )
                                ( i = 3 s = 'xyz') ).

  " Syntax error here: Unexpected operator "FILTER"
  IF lines( FILTER #( items EXCEPT WHERE i = 1 ) ) > 0."

  ENDIF.

  " Syntax error here: "FILTER ANY( )" is not an internal table
  DATA(sum) = REDUCE i( INIT result = 0
                        FOR item IN FILTER #( items EXCEPT WHERE i = 1 )
                        NEXT result = result && item-i ).

  " No syntax error here
  NEW test_expression( )->test( FILTER #( items EXCEPT WHERE i = 1 ) ).

  IF lines( VALUE tty_data( FOR item IN items ( item ) ) ) > 0.

  ENDIF.

  IF REDUCE i( INIT result = 0
               FOR item IN items
               NEXT result = result + item-i ) = 0.

  ENDIF.
