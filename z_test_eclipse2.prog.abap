*&---------------------------------------------------------------------*
*& Report z_test_eclipse2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_eclipse2.

TYPES: BEGIN OF ty_data,
         i TYPE i,
         s TYPE string,
         x TYPE xstring,
       END OF ty_data,
       tty_data TYPE HASHED TABLE OF ty_data
                     WITH UNIQUE KEY i.

DATA: itab TYPE tty_data,
      line TYPE ty_data.
