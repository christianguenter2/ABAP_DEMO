*&---------------------------------------------------------------------*
*& report  z_test_task_release
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_task_release.

*----------------------------------------------------------------------*
*       CLASS lcl_test_task_realease DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test_task_realease DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING value(r_instance) TYPE REF TO lcl_test_task_realease.
    METHODS: start.

  PRIVATE SECTION.
    METHODS: _commit_work_and_wait,
             _task_complete IMPORTING i_task TYPE qmsm-manum.

    DATA: lt_return TYPE bapiret2_tab,
          notif_no  TYPE qmsm-qmnum.
ENDCLASS.                    "lcl_test_task_realease DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_test_task_realease DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test_task_realease IMPLEMENTATION.
  METHOD get_instance.
    CREATE OBJECT r_instance.
  ENDMETHOD.                    "get_instance

  DEFINE _check_return.
    if lt_return is not initial.
      cl_demo_output=>display_data( lt_return ).
      return.
    endif.
  END-OF-DEFINITION.

  METHOD start.
    DATA: notifheader        TYPE bapi2080_nothdri,
          notifheader_export TYPE bapi2080_nothdre,
          partners           TYPE STANDARD TABLE OF bapi2080_notpartnri,
          partner            LIKE LINE OF partners,
          tasks              TYPE STANDARD TABLE OF bapi2080_nottaski,
          task               LIKE LINE OF tasks.

    partner-partn_role = 'AG'.
    partner-partner    = '540005'.
    INSERT partner INTO TABLE partners.
    partner-partn_role = 'WE'.
    INSERT partner INTO TABLE partners.
    partner-partn_role = 'YB'.
    INSERT partner INTO TABLE partners.

    task-task_sort_no = 1.
    task-task_code    = '0027'.
    task-task_codegrp = 'TSC'.
    task-partner      = 'GUENTERC'.
    task-partn_role   = 'VU'.
    INSERT task INTO TABLE tasks.

    task-task_sort_no = 2.
    task-task_code    = '0027'.
    task-task_codegrp = 'TSC'.
    task-partner      = 'SCHUESSM'.
    task-partn_role   = 'VU'.
    INSERT task INTO TABLE tasks.

    task-task_sort_no = 3.
    task-task_code    = '0027'.
    task-task_codegrp = 'TSC'.
    task-partner      = ''.
    task-partn_role   = 'VU'.
    INSERT task INTO TABLE tasks.

    task-task_sort_no = 4.
    task-task_code    = '0036'.
    task-task_codegrp = 'TSC'.
    task-partner      = 'GUENTERC'.
    task-partn_role   = 'VU'.
    INSERT task INTO TABLE tasks.

    notifheader-sales_org  = 'DE01'.
    notifheader-distr_chan = '01'.
    notifheader-division   = '01'.

    CALL FUNCTION 'BAPI_ALM_NOTIF_CREATE'
      EXPORTING
        notif_type         = 'ZT'
        notifheader        = notifheader    " BAPI-Servicemeldungskopf zur Erstellung
      IMPORTING
        notifheader_export = notifheader_export    " BAPI Servicemeldungskopf
      TABLES
        notiftask          = tasks
        notifpartnr        = partners
        return             = lt_return.    " Returnparameter

    _check_return.

    DATA: notifheader_new TYPE bapi2080_nothdre.

    CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
      EXPORTING
        number      = notifheader_export-notif_no
      IMPORTING
        notifheader = notifheader_new
      TABLES
        return      = lt_return.    " Nachrichtenart: S = Erfolg, E = Fehler, W = Warnung, I = Inf

    _check_return.

    _commit_work_and_wait( ).

    notif_no = notifheader_new-notif_no.

    _task_complete( 1 ).

    _check_return.

    _task_complete( 2 ).

    _check_return.

    _task_complete( 3 ).

    _check_return.

    CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
      EXPORTING
        number = notifheader_export-notif_no
      TABLES
        return = lt_return.    " Nachrichtenart: S = Erfolg, E = Fehler, W = Warnung, I = Inf

    _commit_work_and_wait( ).

    _check_return.

    DATA: documentdata TYPE bapi_doc_draw2,
          return       TYPE bapiret2,
          objectlinks  TYPE STANDARD TABLE OF bapi_doc_drad,
          objectlink   LIKE LINE OF objectlinks.

    documentdata-documenttype = 'TSC'.
    documentdata-description  = 'Test'.

    objectlink-objecttype = 'SMQMEL'.
    objectlink-objectkey  = notifheader_new-notif_no.
    INSERT objectlink INTO TABLE objectlinks.

    CALL FUNCTION 'BAPI_DOCUMENT_CREATE2'
      EXPORTING
        documentdata = documentdata    " Dokumentendaten
      IMPORTING
        return       = return    " Rückgabestruktur
      TABLES
        objectlinks  = objectlinks.    " Objektverknüpfungen

    IF return IS NOT INITIAL.
      cl_demo_output=>display_data( return ).
      RETURN.
    ENDIF.

    _commit_work_and_wait( ).

    CALL FUNCTION 'BAPI_ALM_NOTIF_TASK_RELEASE'
      EXPORTING
        number   = notifheader_new-notif_no
        task_key = 4
      TABLES
        return   = lt_return.    " Message type:S success, E error, W warning, I in

    _check_return.

    CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
      EXPORTING
        number = notifheader_export-notif_no
      TABLES
        return = lt_return.    " Nachrichtenart: S = Erfolg, E = Fehler, W = Warnung, I = Inf

    _commit_work_and_wait( ).

    IF lt_return IS NOT INITIAL.
      cl_demo_output=>display_data( lt_return ).
      RETURN.
    ENDIF.

    WRITE: / notif_no.
  ENDMETHOD.                    "start

  METHOD _commit_work_and_wait.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ENDMETHOD.                    "commit_work_and_wait

  METHOD _task_complete.
    CALL FUNCTION 'BAPI_ALM_NOTIF_TASK_COMPLETE'
      EXPORTING
        number   = notif_no
        task_key = i_task
      TABLES
        return   = lt_return.
  ENDMETHOD.                    "_task_complete

ENDCLASS.                    "lcl_test_task_realease DEFINITION

START-OF-SELECTION.
  DO 100 TIMES.
    lcl_test_task_realease=>get_instance( )->start( ).
  ENDDO.
