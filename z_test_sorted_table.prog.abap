*&---------------------------------------------------------------------*
*& Report  Z_TEST_SORTED_TABLE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_sorted_table.

TYPES: BEGIN OF ty_vbap,
         vbeln TYPE vbap-vbeln,
         posnr TYPE vbap-posnr,
       END OF ty_vbap.

DATA: lt_vbap TYPE HASHED TABLE OF ty_vbap
              WITH UNIQUE KEY vbeln posnr.

data: x TYPE x.
data: q TYPE q.
data: y TYPE y.
data: x TYPE x.

FIELD-SYMBOLS: <vbap> LIKE LINE OF lt_vbap.

SELECT vbeln posnr
  FROM vbap
  INTO TABLE lt_vbap
  UP TO 100 ROWS.

LOOP AT lt_vbap ASSIGNING <vbap>.
  AT NEW vbeln.
    WRITE: / <vbap>-vbeln, ' ', <vbap>-posnr.
    write: / <vbap>-vbeln, ' ', <vbap>-posnr.
    write: / <vbap>-vbeln, ' ', <vbap>-posnr.
    write: / <vbap>-vbeln, ' ', <vbap>-posnr.
    write: / <vbap>-vbeln, ' ', <vbap>-posnr.
    write: / <vbap>-vbeln, ' ', <vbap>-posnr.
    WRITE: / <vbap>-vbeln, ' ', <vbap>-posnr.
    write: / <vbap>-vbeln, ' ', <vbap>-posnr.

  ENDAT.
ENDLOOP.
