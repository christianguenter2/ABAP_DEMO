*&---------------------------------------------------------------------*
*& Report  Z_TEST_FIND
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT Z_TEST_FIND.

DATA: offset TYPE i.

offset = find( val   = 'TSC_000300999999_00_000_999_15721000'
               regex = `([\d]{12})_([\d]{2})_([\d]{3})_([\d]{3})_([\d]{8})` ).

WRITE offset.
