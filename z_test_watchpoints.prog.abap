*&---------------------------------------------------------------------*
*& Report  Z_TEST_WATCHPOINTS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_watchpoints.


TYPES: BEGIN OF ty_data,
         i TYPE i,
         s TYPE string,
       END OF ty_data.

DATA: lt_data TYPE STANDARD TABLE OF ty_data,
      lv_data LIKE LINE OF lt_data,
      text    TYPE string.

FIELD-SYMBOLS: <data> LIKE LINE OF lt_data.

lv_data-i = 1234.
lv_data-s = 'Test'.
INSERT lv_data INTO TABLE lt_data.

lv_data-i = 815.
lv_data-s = 'Hallo Welt'.
INSERT lv_data INTO TABLE lt_data.

LOOP AT lt_data ASSIGNING <data>.
  text = |{ <data>-i } { <data>-s }|.
  WRITE: / text.
ENDLOOP.

IF sy-subrc = 0.

ENDIF.
