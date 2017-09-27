REPORT z_test_select_option.

TABLES: t100.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: s_sprsl FOR t100-sprsl,
                s_msgnr FOR t100-msgnr.
SELECTION-SCREEN END OF BLOCK b1.
