*&---------------------------------------------------------------------*
*& Report  Z_TEST_SHIFT_LEFT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_shift_left.

DATA: text TYPE string VALUE 'Dies ist ein Test'.

cl_demo_output=>display_data( shift_left( val    = text ) ).
