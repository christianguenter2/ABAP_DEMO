*&---------------------------------------------------------------------*
*& Report  Z_TEST_CHAR30
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_char30.

DATA: text TYPE string VALUE 'test',
      char30 TYPE char30.

char30 = text.

WRITE: / text.
WRITE: / text.