class ZCL_SM_SYSTEM_RESETTER definition
  public
  create public .

public section.

  data MO_START_STATE type ref to ZCL_SM_STATE read-only .

  methods CONSTRUCTOR
    importing
      !IO_START_STATE type ref to ZCL_SM_STATE .
  methods ADD_RESET_EVENT
    importing
      !ID_EVENT_CODE type STRING .
  type-pools ABAP .
  methods IS_RESET_EVENT
    importing
      !ID_EVENT_CODE type STRING
    returning
      value(RF_IS_RESET_EVENT) type ABAP_BOOL .
protected section.
private section.

  types:
    BEGIN OF m_typ_reset_codes,
         event_code TYPE string,
       END OF m_typ_reset_codes .
  types:
    m_tt_reset_codes TYPE STANDARD TABLE OF m_typ_reset_codes .

  data MT_RESET_CODES type M_TT_RESET_CODES .
ENDCLASS.



CLASS ZCL_SM_SYSTEM_RESETTER IMPLEMENTATION.


method ADD_RESET_EVENT.
* Local Variables
  DATA: ls_reset_codes LIKE LINE OF mt_reset_codes.

  ls_reset_codes-event_code = id_event_code.

  APPEND ls_reset_codes TO mt_reset_codes.

endmethod.


method CONSTRUCTOR.

  mo_start_state = io_start_state.

endmethod.


method IS_RESET_EVENT.

  READ TABLE mt_reset_codes TRANSPORTING NO FIELDS
  WITH KEY event_code = id_event_code.

  IF sy-subrc = 0.
    rf_is_reset_event = abap_true.
  ELSE.
    rf_is_reset_event = abap_false.
  ENDIF.

endmethod.
ENDCLASS.
