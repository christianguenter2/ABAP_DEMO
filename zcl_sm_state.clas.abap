class ZCL_SM_STATE definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !ID_NAME type STRING .
  methods STATE_REACHED_SENDS_COMMAND
    importing
      !IO_COMMAND type ref to ZCL_SM_COMMAND_OUT .
  methods STATE_CHANGES_AFTER
    importing
      !EVENT type ref to ZCL_SM_EVENT_IN
      !TO_TARGET_STATE type ref to ZCL_SM_STATE .
  methods TARGET_STATE_AFTER_EVENT
    importing
      !ID_EVENT_CODE type STRING
    returning
      value(RO_TARGET_STATE) type ref to ZCL_SM_STATE .
  type-pools ABAP .
  methods CHANGES_STATE_AFTER_EVENT
    importing
      !ID_EVENT_CODE type STRING
    returning
      value(RF_YES_EVENT_CHANGES_STATE) type ABAP_BOOL .
  methods SEND_OUTBOUND_COMMANDS
    importing
      !IO_EXTERNAL_SYSTEM type ref to ZIF_SM_EXTERNAL_SYSTEM .
protected section.
private section.

  data MD_NAME type STRING .
  data:
    mt_outbound_commands TYPE STANDARD TABLE OF REF TO zcl_sm_command_out .
  data MT_TRANSITIONS type ZCL_SM_TRANSITIONS=>MTT_TRANSITIONS .
ENDCLASS.



CLASS ZCL_SM_STATE IMPLEMENTATION.


method CHANGES_STATE_AFTER_EVENT.

  READ TABLE mt_transitions TRANSPORTING NO FIELDS
  WITH KEY event_code = id_event_code.

  IF sy-subrc = 0.
    rf_yes_event_changes_state = abap_true.
  ELSE.
    rf_yes_event_changes_state = abap_false.
  ENDIF.

endmethod.


method CONSTRUCTOR.

  md_name = id_name.

endmethod.


method SEND_OUTBOUND_COMMANDS.
* Local Variables
  DATA: ls_commands LIKE LINE OF mt_outbound_commands.

  LOOP AT mt_outbound_commands INTO ls_commands.
    io_external_system->send_command( ls_commands->md_code ).
  ENDLOOP.

endmethod.


method STATE_CHANGES_AFTER.
*--------------------------------------------------------------------*
* This is called during configuration of the system to say what new
* state to jump into when a certain event is received from the outside
* world
*--------------------------------------------------------------------*
* IMPORTING io_event        TYPE REF TO zcl_sm_event_in
*           io_target_state TYPE REF TO lcl_state.
*--------------------------------------------------------------------*
* Local Variables
    DATA: lo_transition  TYPE REF TO zcl_sm_transitions,
          ls_transitions LIKE LINE OF mt_transitions.

    ASSERT to_target_state IS NOT INITIAL.

    ls_transitions-event_code = event->md_code.

    CREATE OBJECT lo_transition
      EXPORTING
        io_source_state  = me
        io_trigger_event = event
        io_target_state  = to_target_state.

    ls_transitions-transition = lo_transition.

    APPEND ls_transitions TO me->mt_transitions.

endmethod.


method STATE_REACHED_SENDS_COMMAND.
*--------------------------------------------------------------------*
* This is called during configuration of the system to say what
* actions/commands to exceute when this state is reached
*--------------------------------------------------------------------*
* IMPORTING io_command       TYPE REF TO zcl_sm_command_out
*--------------------------------------------------------------------*
  APPEND io_command TO me->mt_outbound_commands.

endmethod.


method TARGET_STATE_AFTER_EVENT.
* Local Variables
  DATA: ls_transitions LIKE LINE OF mt_transitions.

  READ TABLE me->mt_transitions INTO ls_transitions
  WITH KEY event_code = id_event_code.

  ro_target_state = ls_transitions-transition->mo_target_state.

endmethod.
ENDCLASS.
