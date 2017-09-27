*&---------------------------------------------------------------------*
*& Report  Z_TEST_EP_ACCOUNT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_ep_account.

DATA: lo_acc       TYPE REF TO zcl_app_ep_css_acc,
      lo_user_pars TYPE REF TO zcl_app_ep_css_user_parameters.

CREATE OBJECT lo_user_pars.
lo_user_pars->set_customer(
  EXPORTING
    iv_kunnr  = '0000082304'
    iv_epuser = '0000082304' ).

lo_user_pars->set_parameter(
  EXPORTING
    epuser         = '0000082304' ).

CREATE OBJECT lo_acc.
lo_acc->obj_user_parameters = lo_user_pars.

lo_acc->get_account_header(
  EXPORTING
    iv_seldate_to   = '20141201'
    iv_bukrs        = 'GB01'
    iv_item_type    = 'OPEN'
    iv_seldate_from = '20140101'
    iv_status       = 'OPEN' ).

lo_acc->calculate_saldo( with_overdue_amount = abap_true ).

FIELD-SYMBOLS: <over_amount> LIKE LINE OF lo_acc->itab_overdueamount,
               <saldo>       LIKE LINE OF lo_acc->itab_saldo.

LOOP AT lo_acc->itab_overdueamount ASSIGNING <over_amount>.
  WRITE: / <over_amount>-saldo, <over_amount>-waers.
ENDLOOP.

LOOP AT lo_acc->itab_saldo ASSIGNING <saldo>.
  WRITE: / <saldo>-saldo, <saldo>-waers.
ENDLOOP.
