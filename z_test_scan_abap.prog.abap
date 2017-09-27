*&---------------------------------------------------------------------*
*& Report  Z_TEST_SCAN_ABAP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_scan_abap.

START-OF-SELECTION.
  DATA: proglines TYPE stringtab,
        tokens TYPE stokes_tab,
        statements TYPE sstmnt_tab,
        levels TYPE slevel_tab,
        keywords TYPE STANDARD TABLE OF char255,
        keyword LIKE LINE OF keywords.

  READ REPORT 'Z_TEST_MACRO' INTO proglines.

  keyword = 'PARAMETERS'.
  INSERT keyword INTO TABLE keywords.

  keyword = 'SELECT-OPTIONS'.
  INSERT keyword INTO TABLE keywords.

  SCAN ABAP-SOURCE proglines
            TOKENS INTO tokens
            STATEMENTS INTO statements
            LEVELS INTO levels
            KEYWORDS FROM keywords
            WITH INCLUDES.

  IF sy-subrc = 0.

  ENDIF.
