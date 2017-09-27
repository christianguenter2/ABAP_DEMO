*&---------------------------------------------------------------------*
*& Report  Z_TEST_IDOC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_idoc.

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor,
             start.

  PRIVATE SECTION.
    METHODS: _fill_idoc,
             _processing,
             _display.
    DATA: lt_data_records TYPE STANDARD TABLE OF edidd,
          control_record  TYPE edidc,
          control_records  TYPE STANDARD TABLE OF edidc,
          idoc_number TYPE edidc-docnum,
          lo_log TYPE REF TO if_reca_message_list.

ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD constructor.
    lo_log = cf_reca_message_list=>create( ).
  ENDMETHOD.                    "constructor

  METHOD start.
    _fill_idoc( ).
    _processing( ).
    _display( ).
  ENDMETHOD.                    "start

  METHOD _fill_idoc.
    DATA: lv_data LIKE LINE OF lt_data_records,
          lv_e1edk01 TYPE e1edk01,
          lv_e1edk14 TYPE e1edk14,
          lv_e1edka1 TYPE e1edka1,
          lv_e1edp01 TYPE e1edp01,
          lv_e1edp19 TYPE e1edp19.

    control_record-mestyp = 'ORDERS'.
    control_record-idoctp = 'ORDERS05'.
    control_record-rcvprn = 'LOGDV1_UC'.
    control_record-rcvprt = 'LS'.
    control_record-sndprn = 'LOGDV1_UC'.
    control_record-sndprt = 'LS'.
    control_record-sndpor = 'A000000040'.
    control_record-direct = 2.

    " Kopf Allgemein
    lv_data-segnum = 1.
    lv_data-psgnum = 0.
    lv_data-segnam = 'E1EDK01'.
    lv_data-mandt  = sy-mandt.
    INSERT lv_data INTO TABLE lt_data_records.

    " Organisation
    ADD 1 TO lv_data-segnum.
    lv_data-segnam = 'E1EDK14'.

    lv_e1edk14-qualf = '006'.
    lv_e1edk14-orgid = '01'.
    lv_data-sdata = lv_e1edk14.
    INSERT lv_data INTO TABLE lt_data_records.

    ADD 1 TO lv_data-segnum.
    lv_e1edk14-qualf = '007'.
    lv_e1edk14-orgid = '01'.
    lv_data-sdata = lv_e1edk14.
    INSERT lv_data INTO TABLE lt_data_records.

    ADD 1 TO lv_data-segnum.
    lv_e1edk14-qualf = '008'.
    lv_e1edk14-orgid = 'DE01'.
    lv_data-sdata = lv_e1edk14.
    INSERT lv_data INTO TABLE lt_data_records.

    ADD 1 TO lv_data-segnum.
    lv_e1edk14-qualf = '012'.
    lv_e1edk14-orgid = '  TA'.
    lv_data-sdata = lv_e1edk14.
    INSERT lv_data INTO TABLE lt_data_records.

    " Partner
    ADD 1 TO lv_data-segnum.
    lv_data-segnam = 'E1EDKA1'.

    lv_e1edka1-parvw = 'AG'.
    lv_e1edka1-partn = '0000130030'.
    lv_data-sdata = lv_e1edka1.
    INSERT lv_data INTO TABLE lt_data_records.

    " Positionen
    DO 3 TIMES.
      ADD 1 TO lv_data-segnum.
      lv_data-segnam = 'E1EDP01'.
      lv_e1edp01-matnr = '01800180'.
      lv_e1edp01-menge = '10'.
      lv_data-sdata = lv_e1edp01.
      INSERT lv_data INTO TABLE lt_data_records.

      ADD 1 TO lv_data-segnum.
      lv_data-segnam = 'E1EDP19'.
      lv_e1edp19-qualf = '002'.
      lv_e1edp19-idtnr = '01800180'.
      lv_data-sdata = lv_e1edp19.
      INSERT lv_data INTO TABLE lt_data_records.
    ENDDO.
  ENDMETHOD.                    "_fill_idoc

  METHOD _processing.
    CALL FUNCTION 'IDOC_INBOUND_WRITE_TO_DB'
      IMPORTING
        pe_idoc_number    = idoc_number    " IDocnummer
      TABLES
        t_data_records    = lt_data_records    " IDoc-Datensätze
      CHANGING
        pc_control_record = control_record    " IDoc-Kontrollsatz
      EXCEPTIONS
        idoc_not_saved    = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      lo_log->add_symsg( ).
      RETURN.
    ELSE.
      lo_log->add(
        EXPORTING
          id_msgty     = 'S'    " Meldung: Nachrichtentyp
          id_msgid     = 'SD'   " Meldung: Nachrichtenklasse
          id_msgno     = '024'   " Meldung: Nachrichtennummer
          id_msgv1     = | IDOC-Nummer: { idoc_number }| ).
    ENDIF.

    APPEND control_record TO control_records.

    CALL FUNCTION 'IDOC_START_INBOUND'
      TABLES
        t_control_records             = control_records    " Vorgangscode Eingang
      EXCEPTIONS
        invalid_document_number       = 1
        error_before_call_application = 2
        inbound_process_not_possible  = 3
        old_wf_start_failed           = 4
        wf_task_error                 = 5
        serious_inbound_error         = 6
        OTHERS                        = 7.
    IF sy-subrc <> 0.
      lo_log->add_symsg( ).
      RETURN.
    ENDIF.
  ENDMETHOD.                    "_processing

  METHOD _display.
    IF idoc_number IS INITIAL .
      CALL FUNCTION 'RECA_GUI_MSGLIST_POPUP'
        EXPORTING
          io_msglist = lo_log.
    ELSE.
      CALL FUNCTION 'EDI_DOCUMENT_TREE_DISPLAY'
        EXPORTING
          docnum        = idoc_number    " Nummer des IDoc's, das als Baum gezeigt werden s
*         open          =     " Parameter ist ab 4.6 nicht mehr relevant
        EXCEPTIONS
          no_idoc_found = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "_display
ENDCLASS.                    "lcl_application IMPLEMENTATION

START-OF-SELECTION.
  DATA: lo_application TYPE REF TO lcl_application.
  CREATE OBJECT lo_application.
  lo_application->start( ).
