*&---------------------------------------------------------------------*
*& Report z_test_regex_2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_regex_2.

DATA(text) = `| |  |X|  | |  |X|  |X|`.

FIND ALL OCCURRENCES OF REGEX `\|(.{1})\|`
     IN text
     RESULTS DATA(results).

DATA(strings) = VALUE stringtab( FOR result IN results
                                 ( substring( val = text
                                              off = result-submatches[ 1 ]-offset
                                              len = result-submatches[ 1 ]-length ) ) ).

cl_demo_output=>display( strings ).
