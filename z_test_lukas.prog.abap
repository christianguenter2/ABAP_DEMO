*&---------------------------------------------------------------------*
*& Report  Z_TEST_LUKAS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_lukas.

DATA: lv_vbak TYPE vbak.

SELECT SINGLE *
  FROM vbak
  INTO lv_vbak
  WHERE vbeln = '4204692015'.

cl_demo_output=>display_data( lv_vbak-vbeln ).
