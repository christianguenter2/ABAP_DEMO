*&---------------------------------------------------------------------*
*& Report  Z_TEST_UNIT_TEST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_unit_test.

DATA: rspar TYPE STANDARD TABLE OF rsparams,
      par   LIKE LINE OF rspar.

CLEAR par.
par-selname = 'B_OBJ'.
par-kind    = 'P'.
par-low     = abap_true.
INSERT par INTO TABLE rspar.

CLEAR par.
par-selname = 'B_DEVC'.
par-kind    = 'P'.
par-low     = abap_false.
INSERT par INTO TABLE rspar.

CLEAR par.
par-selname = 'SO_CLASS'.
par-kind    = 'S'.
par-sign    = 'I'.
par-option  = 'EQ'.
par-low     = 'ZCL_TSC_APP_GET_CONFIGURATION'.
INSERT par INTO TABLE rspar.

CLEAR par.
par-selname = 'B_DIRECT'.
par-kind    = 'P'.
par-low     = abap_true.
INSERT par INTO TABLE rspar.

CLEAR par.
par-selname = 'B_EMAIL'.
par-kind    = 'P'.
par-low     = abap_false.
INSERT par INTO TABLE rspar.

CLEAR par.
par-selname = 'P_AUCV'.
par-kind    = 'P'.
par-low     = abap_false.
INSERT par INTO TABLE rspar.

SUBMIT rs_aucv_runner AND RETURN WITH SELECTION-TABLE rspar.
