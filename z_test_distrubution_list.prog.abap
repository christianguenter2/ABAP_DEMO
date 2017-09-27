*&---------------------------------------------------------------------*
*& Report  Z_TEST_DISTRUBUTION_LIST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_distrubution_list.

DATA: users TYPE spers_ulst.

zcl_bc_bcs_service=>get_users_of_distribution_list(
  EXPORTING
    i_distribution_list_name = 'VERS-CHECK'
    i_private                = ' '
  RECEIVING
    rt_users                 = users ).

cl_demo_output=>display_data( users ).
