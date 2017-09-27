*&---------------------------------------------------------------------*
*& Report z_test_reference
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_reference.

DATA itab TYPE STANDARD TABLE OF REF TO string.

DATA(test) = |Hallo Welt!|.

APPEND INITIAL LINE TO itab ASSIGNING FIELD-SYMBOL(<line>).
CREATE DATA <line>.
<line>->* = test.

test = '1324'.
INSERT REF #( test ) INTO TABLE itab.

cl_demo_output=>write( REDUCE string( INIT result = ||
                                      FOR line IN itab
                                      NEXT result = result && line->* ) ).

*cl_demo_output=>write( REDUCE string( INIT result = ||
*                                      FOR line IN itab2
*                                      NEXT result = result && line->* ) ).

cl_demo_output=>display(  ).
