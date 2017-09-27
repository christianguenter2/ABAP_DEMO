*&---------------------------------------------------------------------*
*& Report  Z_TEST_SOURCE_CODE_SCAN
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_source_code_scan.

DATA: lt_source   TYPE sedi_source,
      gt_keywords TYPE TABLE OF char128, "txh01
      gt_tokens   TYPE stokesx_tab WITH HEADER LINE,
      gt_stm      TYPE sstmnt_tab WITH HEADER LINE.

READ REPORT 'Z_TEST_PRETTY_PRINT' INTO lt_source.

SCAN ABAP-SOURCE lt_source
  TOKENS      INTO gt_tokens
  STATEMENTS  INTO gt_stm
  "KEYWORDS FROM gt_keywords
  WITH ANALYSIS.

IF sy-subrc = 0.

ENDIF.
