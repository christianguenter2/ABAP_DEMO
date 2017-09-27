*&---------------------------------------------------------------------*
*& Report  Z_TEST_POWL_SAP_GUI
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_powl_sap_gui.

PARAMETERS: p_appl TYPE wdy_wb_appl_name DEFAULT 'zmm_powl_eisbe' OBLIGATORY VALUE CHECK.

START-OF-SELECTION.
  CALL FUNCTION 'Z_BC_WEDYNPRO_IN_SAP_GUI'
    EXPORTING
      i_application_name = p_appl.
