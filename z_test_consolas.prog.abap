REPORT z_test_consolas.

DATA: text TYPE string.

text = '/TYPE=MARA'.

" submatches MARA in /TYPE=MARA
*FIND FIRST OCCURRENCE OF REGEX '\/.*=(.*)' IN text
*           SUBMATCHES DATA(type_string).

DATA(type_string) = substring_after( val = text regex = '=' ).

cl_demo_output=>display_data( type_string ).
