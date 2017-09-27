REPORT z_test_bapi_saleorder_create2.

DATA: salesdocumentin         TYPE bapivbeln-vbeln,
      order_header_in         TYPE bapisdhd1,
      order_header_inx        TYPE bapisdhd1x,
      sender                  TYPE bapi_sender,
      binary_relationshiptype TYPE bapireltype-reltype,
      int_number_assignment   TYPE bapiflag-bapiflag,
      behave_when_error       TYPE bapiflag-bapiflag,
      logic_switch            TYPE bapisdls,
      testrun                 TYPE bapiflag-bapiflag,
      convert                 TYPE bapiflag-bapiflag,
      salesdocument           TYPE bapivbeln-vbeln,
      return                  TYPE STANDARD TABLE OF bapiret2,
      order_items_in          TYPE STANDARD TABLE OF bapisditm,
      order_item_in           LIKE LINE OF order_items_in,
      order_items_inx         TYPE STANDARD TABLE OF bapisditmx,
      order_item_inx          LIKE LINE OF order_items_inx,
      order_partners          TYPE STANDARD TABLE OF bapiparnr,
      order_partner           LIKE LINE OF order_partners,
      order_schedules_in      TYPE STANDARD TABLE OF bapischdl,
      order_schedules_inx     TYPE STANDARD TABLE OF bapischdlx,
      order_conditions_in     TYPE STANDARD TABLE OF bapicond,
      order_conditions_inx    TYPE STANDARD TABLE OF bapicondx,
      order_cfgs_ref          TYPE STANDARD TABLE OF bapicucfg,
      order_cfgs_inst         TYPE STANDARD TABLE OF bapicuins,
      order_cfgs_part_of      TYPE STANDARD TABLE OF bapicuprt,
      order_cfgs_value        TYPE STANDARD TABLE OF bapicuval,
      order_cfgs_blob         TYPE STANDARD TABLE OF bapicublb,
      order_cfgs_vk           TYPE STANDARD TABLE OF bapicuvk,
      order_cfgs_refinst      TYPE STANDARD TABLE OF bapicuref,
      order_ccard             TYPE STANDARD TABLE OF bapiccard,
      order_text              TYPE STANDARD TABLE OF bapisdtext,
      order_keys              TYPE STANDARD TABLE OF bapisdkey,
      extensionin             TYPE STANDARD TABLE OF bapiparex,
      partneraddresses        TYPE STANDARD TABLE OF bapiaddr1.


order_header_in-doc_type   = 'TA'.
order_header_in-sales_org  = 'RU01'.
order_header_in-distr_chan = '01'.
order_header_in-division   = '01'.

order_header_inx-doc_type   = 'X'.
order_header_inx-sales_org  = 'X'.
order_header_inx-distr_chan = 'X'.
order_header_inx-division   = 'X'.

order_partner-partn_role = 'AG'.
order_partner-partn_numb = '0001920001'.
INSERT order_partner INTO TABLE order_partners.

order_item_in-itm_number = 1.
order_item_in-material   = '13912000'.
order_item_in-target_qty = 10.
order_item_in-target_qu  = 'ST'.
INSERT order_item_in INTO TABLE order_items_in.

order_item_in-itm_number  = 1.
order_item_inx-material   = 'X'.
order_item_inx-target_qty = 'X'.
order_item_inx-target_qu  = 'X'.
INSERT order_item_inx INTO TABLE order_items_inx.

CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
  EXPORTING
*    salesdocumentin         = salesdocumentin    " Nummer des Vertriebsbeleges
    order_header_in         = order_header_in    " Auftragskopf
    order_header_inx        = order_header_inx    " Ankreuzleiste Auftragskopf
*    sender                  = sender    " Logisches System Sender
*    binary_relationshiptype = binary_relationshiptype    " Binärer Beziehungstyp (private)
*    int_number_assignment   = int_number_assignment    " int. Positionsnummervergabe
*    behave_when_error       = behave_when_error    " Fehlerhandling
*    logic_switch            = logic_switch    " interner Steuerungsparameter
    testrun                 = abap_true
*    convert                 = SPACE    " Umschlüsselung Partnerrolle + Auftragsart
*  IMPORTING
*    salesdocument           = salesdocument    " Nummer des erzeugten Beleges
  TABLES
    return                  = return    " Rückgabemeldungen
    order_items_in          = order_items_in    " Positionsdaten
    order_items_inx         = order_items_inx    " Ankreuzleiste Positionsdaten
    order_partners          = order_partners    " Belegpartner
*    order_schedules_in      = order_schedules_in    " Einteilungsdaten
*    order_schedules_inx     = order_schedules_inx    " Ankreuzleiste Einteilungsdaten
*    order_conditions_in     = order_conditions_in    " Konditionen
*    order_conditions_inx    = order_conditions_inx    " Ankreuzleiste Konditionen
*    order_cfgs_ref          = order_cfgs_ref    " Konfiguration: Referenzdaten
*    order_cfgs_inst         = order_cfgs_inst    " Konfiguration: Instanzen
*    order_cfgs_part_of      = order_cfgs_part_of    " Konfiguration: Part-of Angaben
*    order_cfgs_value        = order_cfgs_value    " Konfiguration: Merkmalswerte
*    order_cfgs_blob         = order_cfgs_blob    " Konfiguration: BLOB interne Daten (SCE)
*    order_cfgs_vk           = order_cfgs_vk    " Konfiguration: Variantenkonditionsschluessel
*    order_cfgs_refinst      = order_cfgs_refinst    " Konfiguration: Referenz Position / Instanz
*    order_ccard             = order_ccard    " Kreditkartendaten
*    order_text              = order_text    " Texte
*    order_keys              = order_keys    " Ausgabetabelle der Referenzschlüssel
*    extensionin             = extensionin    " Kundenerweiterung für VBAK, VBAP, VBEP
*    partneraddresses        = partneraddresses    " BAPI-Referenzstruktur für Adressen (Org./Firma)
  .

CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

cl_demo_output=>write_data( return ).
cl_demo_output=>write_data( order_items_in ).
cl_demo_output=>display(  ).
