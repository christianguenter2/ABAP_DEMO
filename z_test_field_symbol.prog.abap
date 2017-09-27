REPORT z_test_field_symbol.

DATA: text  TYPE string,
      tkomv TYPE STANDARD TABLE OF komv
                 WITH NON-UNIQUE DEFAULT KEY.

CALL FUNCTION 'Z_TEST_FM_1'
  EXPORTING
    text  = text
  TABLES
    tkomv = tkomv.    " Preisfindung Kommunikations-Konditionssatz

CALL FUNCTION 'Z_TEST_FM_2'.

cl_demo_output=>display_data( text ).
