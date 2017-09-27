*&---------------------------------------------------------------------*
*& Report  Z_TEST_INSERT_REPORT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_insert_report.

CONSTANTS: lf LIKE cl_abap_char_utilities=>cr_lf VALUE cl_abap_char_utilities=>cr_lf.
DATA: prog_lines TYPE stringtab,
      line LIKE LINE OF prog_lines,
      mess TYPE string,
      l TYPE i,
      w TYPE string,
      text TYPE string,
      dir TYPE trdir.

READ REPORT 'ZCSCG002' INTO prog_lines.

line = | | .
APPEND line TO prog_lines.
line = | SELECTION-SCREEN BEGIN OF SCREEN 1040 AS SUBSCREEN. |.
APPEND line TO prog_lines.
line = | SELECTION-SCREEN END OF SCREEN 1040. |.
APPEND line TO prog_lines.

SELECT SINGLE *
       FROM trdir
       INTO dir
       WHERE name = sy-repid.

SYNTAX-CHECK FOR prog_lines MESSAGE mess LINE line WORD w
             DIRECTORY ENTRY dir.
if sy-subrc <> 0.
  text = | 'Syntaxfehler' { lf } | &&
         | { mess } { lf } | &&
         | Zeile:   { l } | &&
         | Word: { w } |.
  write: text.
  return.
endif.

INSERT REPORT 'ZCSCG002' FROM prog_lines.
if sy-subrc = 0.
  write: 'Insert erfolgreich'.
endif.
