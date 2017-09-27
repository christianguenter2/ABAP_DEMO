REPORT z_test_table_comprehension_2.

TYPES: BEGIN OF t_struct1,
         field1 TYPE i,
         field2 TYPE string,
       END OF t_struct1,
       tt_struct1 TYPE STANDARD TABLE OF t_struct1
                       WITH NON-UNIQUE EMPTY KEY,
       BEGIN OF t_struct2,
         field1 TYPE i,
         field2 TYPE string,
         field3 TYPE i,
       END OF t_struct2,
       tt_struct2 TYPE STANDARD TABLE OF t_struct2
                       WITH NON-UNIQUE EMPTY KEY.

cl_demo_output=>display( VALUE tt_struct2( FOR wa IN VALUE tt_struct1( ( field1 = 1 field2 = 'A' )
                                                                       ( field1 = 2 field2 = 'B' ) )
                                           INDEX INTO index
                                           LET base = VALUE t_struct2( field3 = index )
                                           IN ( CORRESPONDING #( BASE ( base ) wa ) ) ) ).
