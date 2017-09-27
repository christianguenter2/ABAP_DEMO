*&---------------------------------------------------------------------*
*& Report  Z_TEST_TEXT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_text.

DATA: lt_lines TYPE STANDARD TABLE OF tline,
      line     LIKE LINE OF lt_lines.

line-tdformat = '*'.
line-tdline   = 'Test'.
INSERT line INTO TABLE lt_lines.

CALL FUNCTION 'TERM_CONTROL_EDIT'
*  EXPORTING
*    titel          = titel
*    langu          = langu
  TABLES
    textlines      = lt_lines
  EXCEPTIONS
    user_cancelled = 1
    OTHERS         = 2
  .
