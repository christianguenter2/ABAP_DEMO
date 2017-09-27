*&---------------------------------------------------------------------*
*& Report  Z_TEST_ABAP_SOURCE_SCAN
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_abap_source_scan.

PARAMETERS: p_report TYPE sy-repid OBLIGATORY.

DATA: proglines  TYPE stringtab,
      tokens     TYPE stokesx_tab,
      statements TYPE sstmnt_tab,
      keywords   TYPE STANDARD TABLE OF char255,
      keyword    LIKE LINE OF keywords.

keyword = `CLASS`.
INSERT keyword INTO TABLE keywords.
keyword = `"`.
INSERT keyword INTO TABLE keywords.

READ REPORT p_report INTO proglines.

SCAN ABAP-SOURCE proglines
          WITH COMMENTS
          TOKENS INTO tokens
          STATEMENTS INTO statements
          KEYWORDS FROM keywords
          WITH INCLUDES.

cl_demo_output=>write_data( value = tokens ).
cl_demo_output=>write_data( value = statements ).
cl_demo_output=>display( ).
