*&---------------------------------------------------------------------*
*& Report  Z_TEST_PACKING_HYINFO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_packing_hyinfo.

TABLES: mara, t685.

SELECT-OPTIONS: s_matnr FOR mara-matnr OBLIGATORY,
                s_kschl FOR t685-kschl OBLIGATORY.

cl_demo_output=>display_data(
  zcl_mm_packing=>get_data_from_zpopc(
    it_r_matnr              = s_matnr[]
    it_r_kschl              = s_kschl[]
    i_default_pack_instruct = 'X' ) ).
