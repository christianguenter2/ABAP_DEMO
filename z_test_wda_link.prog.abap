*&---------------------------------------------------------------------*
*& Report  Z_TEST_WDA_LINK
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_wda_link.

DATA: url        TYPE string,
      parameters TYPE tihttpnvp,
      parameter  LIKE LINE OF parameters.

parameter-name  = 'LIFNR'.
parameter-value = '0000584708'.
INSERT parameter INTO TABLE parameters.

parameter-name  = 'MATNR'.
parameter-value = '69903201'.
INSERT parameter INTO TABLE parameters.

cl_wd_utilities=>construct_wd_url(
  EXPORTING
    application_name              = 'zmm_powl_packing_pos'
    in_parameters                 = parameters    " zusätzliche Parameter
  IMPORTING
    out_absolute_url              = url ).

cl_abap_browser=>show_url( url ).
