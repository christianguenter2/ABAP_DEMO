REPORT z_test_table_expression.

TYPES: BEGIN OF ty_data,
         partn_numb TYPE kunnr,
       END OF ty_data,
       tty_data TYPE STANDARD TABLE OF ty_data
                WITH NON-UNIQUE DEFAULT KEY.

DATA: ship_to_party TYPE kunnr VALUE '1234'.
DATA(partner) = VALUE tty_data( ( partn_numb = '0815' ) ).

partner[ partn_numb = space ] = ship_to_party.

cl_demo_output=>display( partner ).
