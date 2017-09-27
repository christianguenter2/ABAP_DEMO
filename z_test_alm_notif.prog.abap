*&---------------------------------------------------------------------*
*& Report  Z_TEST_ALM_NOTIF
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_alm_notif.

PARAMETERS: number TYPE bapi2080_nothdre-notif_no OBLIGATORY DEFAULT '300704658'.

DATA: notiftask TYPE STANDARD TABLE OF bapi2080_nottaski,
      return    TYPE STANDARD TABLE OF bapiret2.

FIELD-SYMBOLS: <notiftask> LIKE LINE OF notiftask.

APPEND INITIAL LINE TO notiftask ASSIGNING <notiftask>.
<notiftask>-task_sort_no   = '1'.
<notiftask>-task_code      = '0002'.
<notiftask>-task_codegrp   = 'TSC'.
<notiftask>-carried_out_by = 'RUFALEXA'.

CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_ADD'
  EXPORTING
    number    = number    " Meldungsnummer
  TABLES
    notiftask = notiftask
    return    = return.

IF return IS NOT INITIAL.
  cl_demo_output=>display_data( return ).
ENDIF.

CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
  EXPORTING
    number = number    " Meldungsnummer
  TABLES
    return = return.    " Nachrichtenart: S = Erfolg, E = Fehler, W = Warnung, I = Inf

IF return IS NOT INITIAL.
  cl_demo_output=>display_data( return ).
ENDIF.

CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
  EXPORTING
    wait = abap_true.

READ TABLE notiftask ASSIGNING <notiftask> INDEX 1.

CALL FUNCTION 'BAPI_ALM_NOTIF_TASK_COMPLETE'
  EXPORTING
    number           = number    " Meldungsnummer
    task_key         = <notiftask>-task_key
    carried_out_by   = 'HARTUNGJ'
    carried_out_date = sy-datum
    carried_out_time = sy-uzeit
  TABLES
    return           = return.    " Message type:S success, E error, W warning, I in

IF return IS NOT INITIAL.
  cl_demo_output=>display_data( return ).
ENDIF.

*CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
*  EXPORTING
*    number = number    " Meldungsnummer
*  TABLES
*    return = return.    " Nachrichtenart: S = Erfolg, E = Fehler, W = Warnung, I = Inf

IF return IS NOT INITIAL.
  cl_demo_output=>display_data( return ).
ENDIF.

CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
  EXPORTING
    wait = abap_true.
