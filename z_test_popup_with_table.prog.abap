*&---------------------------------------------------------------------*
*& Report  Z_TEST_POPUP_WITH_TABLE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_popup_with_table.

TYPES: BEGIN OF ty_matnr,
         matnr TYPE mara-matnr,
       END OF ty_matnr.

DATA: lt_matnr TYPE STANDARD TABLE OF ty_matnr,
      matnr    TYPE ty_matnr,
      columns  TYPE STANDARD TABLE OF help_value,
      column   LIKE LINE OF columns.

matnr-matnr = '01800180'.
INSERT matnr INTO TABLE lt_matnr.

column-fieldname  = 'MATNR'.
column-tabname    = 'MARA'.
column-selectflag = abap_true.
INSERT column INTO TABLE columns.

CALL FUNCTION 'MD_POPUP_SHOW_INTERNAL_TABLE'
  EXPORTING
    title   = 'Test'
*  IMPORTING
*    index   = index    " Index ausgewählter Eintrag
  TABLES
    values  = lt_matnr
    columns = columns    " Spalten der internen Tabelle
  EXCEPTIONS
    leave   = 1
    OTHERS  = 2.
