REPORT z_test_corresponding.

TYPES:
  BEGIN OF source,
    dummy TYPE char01,
  END OF source,

  BEGIN OF target,
    dummy TYPE char01,
    date  TYPE d,
  END OF target,

  sourcetab TYPE STANDARD TABLE OF source
                 WITH NON-UNIQUE DEFAULT KEY,
  targettab TYPE STANDARD TABLE OF target
                 WITH NON-UNIQUE DEFAULT KEY.

DATA(sourcetab) = VALUE sourcetab( ( dummy = 'X' )
                                   ( dummy = 'Y' )
                                   ( dummy = 'Z' ) ).

DATA(targettab) = VALUE targettab(
                    FOR wa IN sourcetab (
                           CORRESPONDING #(
                             base ( value #( date = sy-datum ) )
                             wa ) ) ).

cl_demo_output=>display( targettab ).
