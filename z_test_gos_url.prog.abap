*&---------------------------------------------------------------------*
*& Report  Z_TEST_GOS_URL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_gos_url.

PARAMETERS: p_fehler TYPE sibfboriid DEFAULT '5419032876320F30E10080000A060165' OBLIGATORY.

DATA: links TYPE stringtab.

links = zcl_qm_fmea=>get_links( p_fehler ).

IF lines( links ) > 0.
  cl_demo_output=>display_data( links ).
ENDIF.
