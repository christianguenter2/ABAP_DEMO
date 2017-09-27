REPORT z_test_delete_in.

TYPES: BEGIN OF ty_data,
         i TYPE i,
         s TYPE string,
       END OF ty_data,
       tty_data TYPE HASHED TABLE OF ty_data
                     WITH UNIQUE KEY i.

TYPES: tty_filter_range TYPE RANGE OF i.

DATA(lt_data) = VALUE tty_data( ( i = 1 s = `Test` )
                                ( i = 2 s = `Hallo Welt!`  ) ).

DATA(filter) = VALUE tty_filter_range( ( sign   = 'I'
                                         option = 'EQ'
                                         low    = 1 ) ).

CLEAR filter.

DELETE lt_data WHERE i NOT IN filter.

cl_demo_output=>display( lt_data ).
