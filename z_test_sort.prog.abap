*&---------------------------------------------------------------------*
*& Report  Z_TEST_SORT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT Z_TEST_SORT.

data: lt_draw TYPE HASHED TABLE OF cvdidrawkey
                   WITH UNIQUE KEY dokar doknr doktl dokvr,
      text TYPE String.

FIELD-SYMBOLS: <draw> like LINE OF lt_draw.

SELECT * FROM draw
  INto CORRESPONDING FIELDS OF TABLE lt_draw
  UP TO 100 ROWS.

LOOP at lt_draw ASSIGNING <draw>.
  text = |{ <draw>-dokar } { <draw>-doknr } { <draw>-doktl } { <draw>-dokvr }|.
  write: / text.
ENDLOOP.
