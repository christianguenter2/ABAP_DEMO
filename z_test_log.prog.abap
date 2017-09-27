REPORT z_test_log.

CLASS zcl_my_fi_log DEFINITION
   FINAL
   CREATE PRIVATE.

  PUBLIC SECTION.

    DATA mt_log TYPE bapirettab.

    CLASS-METHODS: factory
      RETURNING
        VALUE(ro_obj) TYPE REF TO zcl_my_fi_log,

      class_constructor.

    METHODS add_sy_message .

    METHODS display .

    CLASS-DATA mo_log TYPE REF TO zcl_my_fi_log READ-ONLY.
ENDCLASS.

CLASS zcl_my_fi_log IMPLEMENTATION.

  METHOD class_constructor.
    CREATE OBJECT mo_log.
  ENDMETHOD.

  METHOD factory.

    IF NOT mo_log IS BOUND.
      CREATE OBJECT mo_log.
    ENDIF.

    ro_obj = mo_log.

  ENDMETHOD.

  METHOD add_sy_message.

    DATA ls_return TYPE bapiret2.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type   = sy-msgty
        cl     = sy-msgid
        number = sy-msgno
        par1   = sy-msgv1
        par2   = sy-msgv2
        par3   = sy-msgv3
        par4   = sy-msgv4
      IMPORTING
        return = ls_return.

    APPEND ls_return TO mt_log.

  ENDMETHOD.

  METHOD display.

    CALL FUNCTION 'SUSR_DISPLAY_LOG'
      EXPORTING
        display_in_popup = abap_true
      TABLES
        it_log_bapiret2  = mt_log.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA lo_my_fi_log1 TYPE REF TO zcl_my_fi_log.
  DATA lo_my_fi_log2 TYPE REF TO zcl_my_fi_log.

  MESSAGE s000(zfi) WITH 'lalala'.
  zcl_my_fi_log=>mo_log->add_sy_message( ).

  LOOP AT zcl_my_fi_log=>factory( )->mt_log ASSIGNING FIELD-SYMBOL(<ls_log>)
     WHERE type = 'S'.

    <ls_log>-type = 'E'.
  ENDLOOP.

  LOOP AT zcl_my_fi_log=>mo_log->mt_log ASSIGNING <ls_log>
     WHERE type = 'S'.
    " Shouldn't be here! But check it out in debugger...
    <ls_log>-type = 'E'.
  ENDLOOP.

  zcl_my_fi_log=>factory( )->display( ).
