*&---------------------------------------------------------------------*
*& Report  Z_TEST_STRING_TO_SOLIX
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_string_to_solix.

DATA: lv_link    TYPE string,
      lt_solix   TYPE solix_tab,
      lv_xstring TYPE xstring,
      lv_size    TYPE so_obj_len.

lv_link = 'Bitte klicken Sie auf den Link, um Ihre Email Addresse zu bestätigen:http://www.hansgrohe.com/verify?uuid=53B50F8569191DC0E10080000A0601'.

cl_bcs_convert=>string_to_solix(
  EXPORTING
    iv_string   = lv_link
    iv_codepage = '4102'
    iv_add_bom  = abap_true
  IMPORTING
    et_solix    = lt_solix
    ev_size     = lv_size
       ).

CLEAR lv_link.

cl_bcs_convert=>solix_to_xstring(
  EXPORTING
    it_solix   = lt_solix    " Eingabedaten
*    iv_size    = lv_size    " Dokumentgröße
  RECEIVING
    ev_xstring = lv_xstring    " Ausgabedaten
).

cl_bcs_convert=>xstring_to_string(
  EXPORTING
    iv_xstr   = lv_xstring
    iv_cp     = '4102'    " SAP-Zeichensatzidentifikation
  RECEIVING
    rv_string = lv_link
).

WRITE: / lv_link.
