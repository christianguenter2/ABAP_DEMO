*&---------------------------------------------------------------------*
*& Report  Z_TEST_LOOP_INDEX
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_loop_index.

DATA: test_tab TYPE stringtab.

FIELD-SYMBOLS: <test> LIKE LINE OF test_tab.

DO 5 TIMES.
  INSERT `Test` INTO TABLE test_tab.
ENDDO.

DO 5 TIMES.
  INSERT `Test2` INTO TABLE test_tab.
ENDDO.

LOOP AT test_tab ASSIGNING <test>
                 WHERE TABLE_LINE = 'Test2'.
  WRITE: / 'TABIX: ', sy-tabix,
           'INDEX: ', sy-index.
ENDLOOP.
