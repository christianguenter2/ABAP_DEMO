REPORT z_test_itab_join.

DATA(tab_a) = VALUE stringtab( ( `Hallo` )
                               ( `Welt` )
                               ( `Dies ist ein Test` ) ).

DATA(tab_b) = VALUE stringtab( ( `Another Test` )
                               ( `Hello World again` ) ).

DATA: joined_itab TYPE stringtab.

joined_itab = VALUE #( ( LINES OF tab_a ) ( LINES OF tab_b ) ).

cl_demo_output=>display( joined_itab ).
