*&---------------------------------------------------------------------*
*& Report  Z_TEST_STRING_LENGTH
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_string_length.

DATA: test   TYPE c LENGTH 10,
      string TYPE string VALUE `Hallo Welt! 123456`.

test+3(*) = string.

cl_demo_output=>display_data( test ).
