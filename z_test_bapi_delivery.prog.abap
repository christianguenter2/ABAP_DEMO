*&---------------------------------------------------------------------*
*& Report  Z_TEST_BAPI_DELIVERY
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_bapi_delivery.

PARAMETERS: delivery TYPE bapiibdlvhdrchg-deliv_numb OBLIGATORY DEFAULT '8100000853',
            lifex TYPE likp-lifex OBLIGATORY DEFAULT '0816'.

DATA: header_data    TYPE bapiibdlvhdrchg ,
      header_control TYPE bapiibdlvhdrctrlchg,
      return         TYPE bapiret2_tab.

header_data-deliv_numb = delivery.
header_data-extdelv_no = lifex.

header_control-deliv_numb    = delivery.
header_control-dlv_extid_flg = abap_true.
header_control-simulate      = abap_false.

CALL FUNCTION 'BAPI_INB_DELIVERY_CHANGE'
  EXPORTING
    header_data          = header_data    " Kopfdaten der Anlieferung ändern
    header_control       = header_control    " Steuerungsdaten Anlieferung Kopfebene
    delivery             = delivery    " Lieferung
*   techn_control        = techn_control    " Technische Steuerung Lieferungs-BAPI
  TABLES
*   header_partner       = header_partner    " Lieferung Partneränderung
*   header_partner_addr  = header_partner_addr    " Partneradressen der Anlieferung ändern
*   header_deadlines     = header_deadlines    " Termine im Anlieferungskopf ändern
*   item_data            = item_data    " Positionsdaten der Anlieferung ändern
*   item_control         = item_control    " Steuerungsdaten Anlieferung Positionsebene
*   item_serial_no       = item_serial_no    " BAPI Lieferung Serialnummern Position
*   extension1           = extension1    " Container für 'Customer Exit'-Parameter
*   extension2           = extension2    " Container für 'Customer Exit'-Parameter
    return               = return    " Returnparameter
*   tokenreference       = tokenreference    " Referenz des CSL-Tokens
*   handling_unit_header = handling_unit_header    " Handling Unit Kopfdaten
*   handling_unit_item   = handling_unit_item    " Handling Unit Position
*   partial_gr_objects   =                     partial_gr_objects    " Objekte zur Teil-WE Buchung
  .


CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

cl_demo_output=>display_data( return ).
