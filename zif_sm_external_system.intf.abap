interface ZIF_SM_EXTERNAL_SYSTEM
  public .


  methods POLL_FOR_EVENT
    returning
      value(RD_EVENT_CODE) type STRING .
  methods SEND_COMMAND
    importing
      !ID_COMMAND_CODE type STRING .
endinterface.
