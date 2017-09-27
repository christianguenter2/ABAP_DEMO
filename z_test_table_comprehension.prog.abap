REPORT z_test_table_comprehension.

TYPES t_itab TYPE TABLE OF i WITH EMPTY KEY.

CLASS cls DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS
      meth IMPORTING p        TYPE t_itab
           RETURNING VALUE(r) TYPE LINE OF t_itab.
ENDCLASS.

CLASS cls IMPLEMENTATION.
  METHOD meth.
    r = p[ 1 ].
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(itab) = VALUE t_itab( ( 1 ) ( 2 ) ( 3 ) ).

  DATA(result1) = VALUE t_itab(
      FOR wa IN itab FROM 2
      WHERE ( table_line >= itab[ 1 ] )
        ( wa ) ).
  cl_demo_output=>write( result1 ).

  DATA(result2) = VALUE t_itab(
      FOR wa IN itab FROM 2
      WHERE ( table_line >= cls=>meth( itab ) )
        ( wa ) ).
  cl_demo_output=>write( result2 ).

  DATA(result3) = VALUE t_itab(
      FOR wa IN itab FROM 2
      WHERE ( table_line >= 1 )
        ( wa ) ).
  cl_demo_output=>write( result3 ).

  DATA result4 TYPE t_itab.
  LOOP AT itab INTO DATA(line)
               FROM 2
               WHERE ( table_line >= itab[ 1 ] ).
    result4 = VALUE #( BASE result4 ( line ) ).
  ENDLOOP.
  cl_demo_output=>write( result4 ).
  cl_demo_output=>display( ).
