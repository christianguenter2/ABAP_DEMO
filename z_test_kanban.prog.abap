REPORT z_test_kanban.

PARAMETERS: kanb_id TYPE bapi1075_1-kanban_id OBLIGATORY.

DATA: kanbanidnumber     TYPE bapi1075_1-kanban_id,
      nextstatus         TYPE bapi1075_1-status,
      return             TYPE bapiret2,
      statuschangeresult TYPE STANDARD TABLE OF bapi1075_3
                              WITH NON-UNIQUE DEFAULT KEY.

kanbanidnumber = kanb_id.
nextstatus     = '2'.

CALL FUNCTION 'BAPI_KANBAN_CHANGESTATUS1'
  EXPORTING
    kanbanidnumber     = kanbanidnumber
    nextstatus         = nextstatus
  IMPORTING
    return             = return
  TABLES
    statuschangeresult = statuschangeresult.

CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

cl_demo_output=>write_data( return ).
cl_demo_output=>display_data( statuschangeresult ).
