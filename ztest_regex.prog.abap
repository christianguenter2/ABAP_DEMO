*&---------------------------------------------------------------------*
*& Report  ZTEST_REGEX
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZTEST_REGEX.

PARAMETERS: p_test TYPE string.

START-OF-SELECTION.


at SELECTION-SCREEN.
  REPLACE all OCCURRENCES OF REGEX '[^[[:word:]|[:space:]]]*' IN p_test WITH ` `.
