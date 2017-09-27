*&---------------------------------------------------------------------*
*& Report  Z_TEST_SERVICE_MELDUNG
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_service_meldung.

*----------------------------------------------------------------------*
*       CLASS lcl_test_notification DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test_notification DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING value(r_instance) TYPE REF TO lcl_test_notification.
    METHODS: start.

  PRIVATE SECTION.
    METHODS:
      _create_notification,
      _create_dis,
      _release_task,
      _finish_task IMPORTING i_task_key TYPE manum,
      _display.

    DATA: notiftask          TYPE bapi2080_nottaski,
          notifheader_export TYPE bapi2080_nothdre,
          notifheader_e      TYPE bapi2080_nothdre.
ENDCLASS.                    "lcl_test_notification DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_test_notification IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test_notification IMPLEMENTATION.
  METHOD get_instance.
    CREATE OBJECT r_instance.
  ENDMETHOD.                    "get_instance

  METHOD start.
    _create_notification( ).
    _create_dis( ).
    _finish_task( '0002' ).
    _finish_task( '0003' ).
    _release_task( ).
    _display( ).
  ENDMETHOD.                    "start

  METHOD _create_notification.
    DATA: notifheader   TYPE bapi2080_nothdri,
          notiftasks    TYPE TABLE OF bapi2080_nottaski,
          notifpartners TYPE TABLE OF bapi2080_notpartnri,
          notifpartner  LIKE LINE OF notifpartners,
          return        TYPE bapiret2_tab.

    notifheader-sales_org  = 'DE01'.
    notifheader-distr_chan = '01'.
    notifheader-division   = '01'.
    notifheader-material   = '01800180'.

    notifpartner-partn_role = 'AG'.
    notifpartner-partner    = '540005'.
    INSERT notifpartner INTO TABLE notifpartners.

    notifpartner-partn_role = 'WE'.
    notifpartner-partner    = '540005'.
    INSERT notifpartner INTO TABLE notifpartners.

    notifpartner-partn_role = 'YB'.
    notifpartner-partner    = '540005'.
    INSERT notifpartner INTO TABLE notifpartners.

    notiftask-task_codegrp = 'TSC'.
    notiftask-task_sort_no = '1'.
    notiftask-task_code    = '0036'.
    INSERT notiftask INTO TABLE notiftasks.

    notiftask-task_codegrp = 'TSC'.
    notiftask-task_sort_no = '2'.
    notiftask-task_code    = '0027'.
    notiftask-partn_role   = 'VU'.
    notiftask-partner      = 'GUENTERC'.
    INSERT notiftask INTO TABLE notiftasks.

    notiftask-task_codegrp = 'TSC'.
    notiftask-task_sort_no = '3'.
    notiftask-task_code    = '0027'.
    notiftask-partn_role   = 'VU'.
    notiftask-partner      = 'SCHUESSM'.
    INSERT notiftask INTO TABLE notiftasks.

    CALL FUNCTION 'BAPI_ALM_NOTIF_CREATE'
      EXPORTING
        notif_type         = 'ZT'
        notifheader        = notifheader    " BAPI-Servicemeldungskopf zur Erstellung
      IMPORTING
        notifheader_export = notifheader_export    " BAPI Servicemeldungskopf
      TABLES
        notiftask          = notiftasks    " Meldungsmaßnahme zur Erstellung
        notifpartnr        = notifpartners
        return             = return.    " Returnparameter

    IF return IS NOT INITIAL.
      cl_demo_output=>write_data( value = return ).
      CLEAR return.
    ENDIF.

    CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
      EXPORTING
        number      = notifheader_export-notif_no
      IMPORTING
        notifheader = notifheader_e
      TABLES
        return      = return.    " Nachrichtenart: S = Erfolg, E = Fehler, W = Warnung, I = Inf

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    IF return IS NOT INITIAL.
      cl_demo_output=>write_data( value = return ).
      CLEAR return.
    ENDIF.

    cl_demo_output=>write_data( notifheader_e ).

    READ TABLE notiftasks INTO notiftask INDEX 1.
  ENDMETHOD.                    "_create_notification

  METHOD _create_dis.
    DATA: documentdata TYPE bapi_doc_draw2,
          objectlinks  TYPE TABLE OF bapi_doc_drad,
          objectlink   LIKE LINE OF objectlinks,
          return       TYPE bapiret2.

    documentdata-documenttype = 'TSC'.
    documentdata-description  = 'Test'.

    objectlink-objecttype = 'SMQMEL'.
    objectlink-objectkey  = notifheader_e-notif_no.
    INSERT objectlink INTO TABLE objectlinks.

    CALL FUNCTION 'BAPI_DOCUMENT_CREATE2'
      EXPORTING
        documentdata = documentdata    " Dokumentendaten
      IMPORTING
        return       = return    " Rückgabestruktur
      TABLES
        objectlinks  = objectlinks.    " Objektverknüpfungen

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    IF return IS NOT INITIAL.
      cl_demo_output=>write_data( value = return ).
    ENDIF.
  ENDMETHOD.                    "_create_dis

  METHOD _release_task.
    DATA: systemstatus TYPE bapi2080_notadt-systatus,
          return       TYPE bapiret2_tab.

    CALL FUNCTION 'BAPI_ALM_NOTIF_TASK_RELEASE'
      EXPORTING
        number       = notifheader_e-notif_no
        task_key     = notiftask-task_key
      IMPORTING
        systemstatus = systemstatus    " Systemstatus der Maßnahme
      TABLES
        return       = return.    " Message type:S success, E error, W warning, I in

    IF return IS NOT INITIAL.
      cl_demo_output=>write_data( value = return ).
      CLEAR return.
    ENDIF.

    cl_demo_output=>write_data( value = systemstatus ).

    CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
      EXPORTING
        number = notifheader_e-notif_no
      TABLES
        return = return.    " Nachrichtenart: S = Erfolg, E = Fehler, W = Warnung, I = Inf

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    IF return IS NOT INITIAL.
      cl_demo_output=>write_data( value = return ).
      CLEAR return.
    ENDIF.
  ENDMETHOD.                    "_release_task

  METHOD _finish_task.
    DATA: return       TYPE bapiret2_tab,
          systemstatus TYPE bapi2080_notadt-systatus.

    CALL FUNCTION 'BAPI_ALM_NOTIF_TASK_COMPLETE'
      EXPORTING
        number       = notifheader_e-notif_no
        task_key     = i_task_key
      IMPORTING
        systemstatus = systemstatus    " Systemstatus der Maßnahme
      TABLES
        return       = return.    " Message type:S success, E error, W warning, I in

    IF return IS NOT INITIAL.
      cl_demo_output=>write_data( value = return ).
      CLEAR return.
    ENDIF.

    cl_demo_output=>write_data( value = systemstatus ).

    CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
      EXPORTING
        number = notifheader_e-notif_no
      TABLES
        return = return.    " Nachrichtenart: S = Erfolg, E = Fehler, W = Warnung, I = Inf

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    IF return IS NOT INITIAL.
      cl_demo_output=>write_data( value = return ).
      CLEAR return.
    ENDIF.
  ENDMETHOD.                    "_done_task

  METHOD _display.
    cl_demo_output=>display( ).
  ENDMETHOD.                    "_display
ENDCLASS.                    "lcl_test_notification IMPLEMENTATION

START-OF-SELECTION.
  lcl_test_notification=>get_instance( )->start( ).
