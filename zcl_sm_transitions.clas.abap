class ZCL_SM_TRANSITIONS definition
  public
  create public .

public section.

  types:
    BEGIN OF m_typ_transitions,
             event_code TYPE string,
             transition TYPE REF TO zcl_sm_transitions,
           END OF m_typ_transitions .
  types:
    mtt_transitions TYPE STANDARD TABLE OF m_typ_transitions .

  data MO_SOURCE_STATE type ref to ZCL_SM_STATE read-only .
  data MO_TRIGGER_EVENT type ref to ZCL_SM_EVENT_IN read-only .
  data MO_TARGET_STATE type ref to ZCL_SM_STATE read-only .

  methods CONSTRUCTOR
    importing
      !IO_SOURCE_STATE type ref to ZCL_SM_STATE
      !IO_TRIGGER_EVENT type ref to ZCL_SM_EVENT_IN
      !IO_TARGET_STATE type ref to ZCL_SM_STATE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SM_TRANSITIONS IMPLEMENTATION.


method CONSTRUCTOR.

  mo_source_state  = io_source_state.
  mo_trigger_event = io_trigger_event.
  mo_target_state  = io_target_state.

endmethod.
ENDCLASS.
