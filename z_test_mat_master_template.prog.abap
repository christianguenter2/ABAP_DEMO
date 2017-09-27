*&---------------------------------------------------------------------*
*& Report  Z_TEST_MAT_MASTER_TEMPLATE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_mat_master_template.

*DATA: template_value TYPE string.
*
*template_value = zcl_material_master_template=>get_template_value(
**      i_bwkey          = i_bwkey    " Bewertungskreis
**      i_bwtar          = i_bwtar    " Bewertungsart
**      i_ekorg          = i_ekorg    " Einkaufsorganisation
*      i_fieldname      = 'HKMAT'
**      i_lgort          = i_lgort    " Lagerort
*      i_status         = 'G'
**      i_spart          = i_spart    " Sparte
*      i_tabname        = 'MBEW'
**      i_uname          = 'GUENTERC'
**      i_vkorg          = i_vkorg    " Verkaufsorganisation
**      i_vtweg          = i_vtweg    " Vertriebsweg
**      i_werks          = 'DE01'
*  ).
*
*cl_demo_output=>display_data( template_value ).

DATA: maru  TYPE maru,
      marcu TYPE marcu,
      mardu TYPE mardu,
      mbew  TYPE mbew,
      mvke  TYPE mvke.

zcl_material_master_template=>assgin_template_values(
    EXPORTING
      i_material_status = 'G'
    CHANGING
      c_marau = maru
      c_marcu = marcu
      c_mardu = mardu
      c_mbew  = mbew
      c_mvke  = mvke ).

IF sy-subrc = 0.

ENDIF.
