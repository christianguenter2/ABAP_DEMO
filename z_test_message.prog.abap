*&---------------------------------------------------------------------*
*& Report  Z_TEST_MESSAGE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_message.

START-OF-SELECTION.
*  data: lo_reca TYPE REF TO cl_reca_message_list,
*        lo_zreca TYPE REF TO zcl_reca_message_list,
*        lif_reca type REF TO if_reca_message_list.
*
**  lif_reca ?= CF_RECA_MESSAGE_LIST=>create( ).
**  lo_reca ?= lif_reca.
**  lo_zreca ?= lif_reca.
*
*  lo_zreca ?= CF_RECA_MESSAGE_LIST=>create_by_reference( ).

*MESSAGE E011(zplm01). "DIS &1 &2 &3 &4 nicht vorhanden!

  DATA: error TYPE REF TO zcx_lo_error.

  TRY .
      zcx_lo_error=>raise_t100(
              EXPORTING
                i_msgid = 'ZPLM01'    " Nachrichtentyp
                i_msgno = '011'
                i_msgv1 = 'Test' ).   " Nachrichtenklasse
    CATCH zcx_lo_error INTO error.
      MESSAGE error TYPE 'E'.
  ENDTRY.
