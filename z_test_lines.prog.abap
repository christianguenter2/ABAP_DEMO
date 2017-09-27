*&---------------------------------------------------------------------*
*& Report  Z_TEST_LINES
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT Z_TEST_LINES.

TYPES: BEGIN OF ty_data,
         i TYPE i,
         s TYPE string,
       END OF ty_data,
       tty_data TYPE SORTED TABLE OF ty_data
                     WITH UNIQUE KEY i.

data: lt_data TYPE tty_data,
      lv_data LIKE LINE OF lt_data,
      text TYPE string.

DEFINE _insert_line.
  CLEAR lv_data.
  lv_data-i = &1.
  lv_data-s = &2.
  insert lv_data INTO TABLE lt_data.
END-OF-DEFINITION.
*
*_insert_line: 11 'Test',
*              12 'Test',
*              13 '1323fads',
*              15 'fsa',
*              37 'dfas'.

CLEAR lv_data.

READ TABLE lt_data INTO lv_data
                   INDEX lines( lt_data ).
if sy-subrc = 0.
  text = |{ lv_data-i } { lv_data-s }|.
  WRITE: text.
else.
  write: sy-subrc.
endif.
