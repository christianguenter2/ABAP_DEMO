REPORT z_test_regex.

DATA: regex      TYPE string VALUE `(\+|\*|\?)`,
      value      TYPE string VALUE `216825052*.EML`,
      lo_regex   TYPE REF TO cl_abap_regex,
      lo_matcher TYPE REF TO cl_abap_matcher.

cl_demo_output=>write_text( |1:  { boolc( matches( regex = regex
                                                   val   = value ) ) }| ).

FIND FIRST OCCURRENCE OF REGEX regex
           IN value.
IF sy-subrc = 0.
  cl_demo_output=>write_text( `2: X` ).
ELSE.
  cl_demo_output=>write_text( `2: ` ).
ENDIF.

CREATE OBJECT lo_regex
  EXPORTING
    pattern = regex.

CREATE OBJECT lo_matcher
  EXPORTING
    regex = lo_regex
    text  = value.

cl_demo_output=>write_text( |3: { lo_matcher->match( ) }| ).

cl_demo_output=>write_text( |4: { lo_matcher->contains( pattern = regex
                                                        text    = value ) }| ).

cl_demo_output=>write_text( |5: { boolc( contains( regex = regex
                                                   val   = value ) ) }| ).

cl_demo_output=>display(  ).
