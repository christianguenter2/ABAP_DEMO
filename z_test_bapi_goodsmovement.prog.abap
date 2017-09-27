*&---------------------------------------------------------------------*
*& Report  Z_TEST_BAPI_GOODSMOVEMENT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_bapi_goodsmovement.

*PARAMETERS: p_mblnr TYPE mseg-mblnr OBLIGATORY,
*            p_mjahr TYPE mseg-mjahr OBLIGATORY.

DATA: goodsmvt_header  TYPE bapi2017_gm_head_01,
      goodsmvt_code    TYPE bapi2017_gm_code,
      goodsmvt_headret TYPE bapi2017_gm_head_ret,
      materialdocument TYPE bapi2017_gm_head_ret-mat_doc,
      matdocumentyear  TYPE bapi2017_gm_head_ret-doc_year,
      goodsmvt_items   TYPE STANDARD TABLE OF bapi2017_gm_item_create,
      goodsmvt_item    LIKE LINE OF goodsmvt_items,
      return           TYPE STANDARD TABLE OF bapiret2.


* X01er WE zur Bestellung
*The following fields must be populated:
*Purchase order
*Purchase order item
*Movement type
*Movement indicator
*Quantity in unit of entry
*ISO code unit of measurement for unit of entry or quantity proposal

"Step 2: WE-Buchung für Entnahme Konsignation im SMI Prozess
goodsmvt_code = '01'.
goodsmvt_header-ref_doc_no = 'REF002'.
goodsmvt_header-pstng_date = sy-datum.
goodsmvt_header-EXT_WMS = '2'.
goodsmvt_item-PO_NUMBER   = '0002109153'.
goodsmvt_item-PO_ITEM     = '00001'.
goodsmvt_item-base_uom   = 'ST'.
goodsmvt_item-move_type  = 'X01'.
goodsmvt_item-MVT_IND = 'B'.
goodsmvt_item-ENTRY_QNT = '10'.
goodsmvt_item-ENTRY_UOM = 'ST'.
goodsmvt_item-plant = 'DE01'.
goodsmvt_item-STGE_LOC = '3510'.
goodsmvt_item-MOVE_REAS = '10'.
goodsmvt_item-NO_MORE_GR = 'X'.

* 502 K, Konsi rückabwickeln
"The following fields must be populated:
"Material number
"Plant
"Storage location
"Movement type
"Movement indicator
"Quantity in unit of entry
"ISO code unit of measurement for unit of entry


*"Step 1: Ausbuchen aus Konsignation für Entnahme Konsignation im SMI Prozess"
*goodsmvt_code = '01'.
*goodsmvt_header-ref_doc_no = 'REF002'.
*goodsmvt_header-pstng_date = sy-datum.
*goodsmvt_header-EXT_WMS = '2'.
*goodsmvt_item-material   = '19111518'.    "'19326000'.
*goodsmvt_item-plant = 'DE01'.
*goodsmvt_item-STGE_LOC = '3510'.
*goodsmvt_item-move_type  = '502'.
*goodsmvt_item-SPEC_STOCK = 'K'.
*goodsmvt_item-vendor = '0001011598'.      ""'0000593212'.
*goodsmvt_item-MVT_IND = ''.
*goodsmvt_item-ENTRY_QNT = '10'.
*goodsmvt_item-ENTRY_UOM = 'ST'.
*goodsmvt_item-MOVE_REAS = '10'.




*goodsmvt_code = '04'.
*goodsmvt_header-pstng_date = sy-datum.
*goodsmvt_header-ref_doc_no = p_mblnr.
*
*goodsmvt_item-material   = '30923420'.
*goodsmvt_item-entry_qnt  = '1'.
*goodsmvt_item-base_uom   = 'ST'.
*goodsmvt_item-move_type  = '315'.
*goodsmvt_item-plant      = 'DE01'.
*goodsmvt_item-stge_loc   = '3482'.
*goodsmvt_item-move_stloc = '3484'.


INSERT goodsmvt_item INTO TABLE goodsmvt_items.



CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
  EXPORTING
    goodsmvt_header  = goodsmvt_header    " Kopfdaten des Materialbelegs
    goodsmvt_code    = goodsmvt_code    " Zuordnung Code zu Transaktion für Warenbewegung
  IMPORTING
    materialdocument = materialdocument
    matdocumentyear  = matdocumentyear
  TABLES
    goodsmvt_item    = goodsmvt_items    " Materialbelegpositionen
    return           = return.

CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

IF return IS NOT INITIAL.
  cl_demo_output=>display_data( return ).
ENDIF.

IF  materialdocument IS NOT INITIAL.
  cl_demo_output=>write_data( materialdocument ).
  cl_demo_output=>write_data( matdocumentyear ).
  cl_demo_output=>display( ).
ENDIF.
