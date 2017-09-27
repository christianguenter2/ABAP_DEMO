*&---------------------------------------------------------------------*
*& Report  Z_TEST_EP_ZTERM
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_ep_zterm.

DATA: param       TYPE REF TO zcl_app_ep_css_user_parameters,
      zterm_texts TYPE tihttpnvp,
      kunnr       TYPE kunnr VALUE '0001920001'.

CREATE OBJECT param.

param->set_customer(
  EXPORTING
    iv_kunnr  = |{ kunnr }|
    iv_epuser = |{ kunnr }| ).

param->set_parameter(
  EXPORTING
    epuser    = |{ kunnr }| ).

zterm_texts = param->get_zterm_texts( ).

cl_demo_output=>display_data( zterm_texts ).
