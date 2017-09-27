*&---------------------------------------------------------------------*
*& Report  Z_TEST_BAPI_ALM_ORDER_MAINTAIN
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_bapi_alm_order_maintain.

DATA: methods   TYPE STANDARD TABLE OF bapi_alm_order_method,
      method    LIKE LINE OF methods,
      tasklists TYPE STANDARD TABLE OF bapi_alm_order_tasklists_i,
      tasklist  LIKE LINE OF tasklists,
      return    TYPE STANDARD TABLE OF bapiret2.

method-refnumber  = 1.
method-objecttype = 'TASKLIST'.
method-method     = 'ADD'.
method-objectkey  = '000050072252'.
INSERT method INTO TABLE methods.

CLEAR method.
method-method    = 'SAVE'.
method-objectkey = '000050072252'.
INSERT method INTO TABLE methods.

tasklist-task_list_type  = 'A'.
tasklist-task_list_group = '00000094'.
tasklist-group_counter   = '01'.
INSERT tasklist INTO TABLE tasklists.

CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
  TABLES
    it_methods   = methods    " BAPI-Struktur: Verarbeitungsmethoden
    it_tasklists = tasklists    " Parameter für das Einbinden von Anleitungen / Arbeitsplänen
    return       = return.    " Returnparameter

CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

cl_demo_output=>display_data( return ).
