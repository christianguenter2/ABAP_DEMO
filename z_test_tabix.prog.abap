REPORT z_test_tabix.

DATA: itab TYPE stringtab,
      line LIKE LINE OF itab.

FIELD-SYMBOLS: <line> LIKE LINE OF itab.

line = 'Test'.
INSERT line INTO TABLE itab.
line = 'Test2'.
INSERT line INTO TABLE itab.
line = 'Test'.
INSERT line INTO TABLE itab.
line = 'Test2'.
INSERT line INTO TABLE itab.
line = 'Test'.
INSERT line INTO TABLE itab.
line = 'Test2'.
INSERT line INTO TABLE itab.

LOOP AT itab ASSIGNING <line>
             WHERE table_line = 'Test2'.
  WRITE: / sy-tabix.
ENDLOOP.
