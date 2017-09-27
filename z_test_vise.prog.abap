*&---------------------------------------------------------------------*
*& Report  Z_TEST_VISE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT Z_TEST_VISE.

TYPES: BEGIN OF ty_data,
         i TYPE i,
         s TYPE string,
       END OF ty_data.

data: itab TYPE SORTED TABLE OF ty_data
                with UNIQUE KEY i,
      line like LINE OF itab,
      text TYPE string.

line-i = 3.
line-s = 'Test1'.
insert line INto TABLE itab.

zcl_vise=>grip(
  EXPORTING
    i_data               =  itab ).

text = | { line-i } { line-s } |.
write: text.
