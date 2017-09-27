*&---------------------------------------------------------------------*
*& Report  ZTEST_SECONDARY_KEY
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ztest_secondary_key.

TYPES: BEGIN OF ty_data,
         i TYPE i,
         s TYPE string,
       END OF ty_data,
       tty_data TYPE STANDARD TABLE OF ty_data WITH NON-UNIQUE DEFAULT KEY
                     WITH UNIQUE SORTED KEY secondary_key
                     COMPONENTS i.

START-OF-SELECTION.
