*&---------------------------------------------------------------------*
*& Report z_test_dialog_module
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_dialog_module.

DATA: BEGIN OF ITAB,
        LINE(72),
      END   OF ITAB,
      TITLE LIKE SY-TITLE.

CALL DIALOG 'RS_EDIT_TABLE'
    EXPORTING SOURCETAB FROM ITAB
              TITLE
    IMPORTING SOURCETAB TO   ITAB.
