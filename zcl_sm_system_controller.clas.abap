class ZCL_SM_SYSTEM_CONTROLLER definition
  public
  create public .

public section.

  data MO_CURRENT_STATE type ref to ZCL_SM_STATE read-only .

  methods CONSTRUCTOR
    importing
      !IO_EXTERNAL_SYSTEM type ref to ZIF_SM_EXTERNAL_SYSTEM
      !IO_SYSTEM_RESETTER type ref to ZCL_SM_SYSTEM_RESETTER .
  methods HANDLE_INBOUND_EVENT
    importing
      !ID_EVENT_CODE type STRING .
protected section.
private section.

  data MO_EXTERNAL_SYSTEM type ref to ZIF_SM_EXTERNAL_SYSTEM .
  data MO_SYSTEM_RESETTER type ref to ZCL_SM_SYSTEM_RESETTER .

  methods TRANSISITION_TO
    importing
      !IO_TARGET_STATE type ref to ZCL_SM_STATE .
ENDCLASS.



CLASS ZCL_SM_SYSTEM_CONTROLLER IMPLEMENTATION.


method CONSTRUCTOR.

  mo_external_system = io_external_system.
  mo_system_resetter = io_system_resetter.

endmethod.


method HANDLE_INBOUND_EVENT.
* Preconditions
  CHECK id_event_code IS NOT INITIAL.

  IF mo_current_state IS INITIAL.
    transisition_to( mo_system_resetter->mo_start_state ).
  ELSEIF mo_current_state->changes_state_after_event( id_event_code ) = abap_true.
    transisition_to( mo_current_state->target_state_after_event( id_event_code ) ).
  ELSEIF mo_system_resetter->is_reset_event( id_event_code ) = abap_true.
    transisition_to( mo_system_resetter->mo_start_state ).
  ENDIF.

endmethod.


method TRANSISITION_TO.

  mo_current_state = io_target_state.
  mo_current_state->send_outbound_commands( mo_external_system ).

endmethod.
ENDCLASS.
