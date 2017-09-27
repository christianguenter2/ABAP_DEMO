*&---------------------------------------------------------------------*
*& Report  Z_TEST_FIORI
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_fiori.

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-000.
PARAMETERS: gw     TYPE abap_bool RADIOBUTTON GROUP r0 DEFAULT 'X',
            portal TYPE abap_bool RADIOBUTTON GROUP r0.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: lpad TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X' USER-COMMAND ucomm,
            app  TYPE abap_bool RADIOBUTTON GROUP r1,
            doc  TYPE abap_bool RADIOBUTTON GROUP r1,
            wiid TYPE sww_wiid MODIF ID m2.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-003.
PARAMETERS: gui     TYPE abap_bool RADIOBUTTON GROUP r2 DEFAULT 'X',
            browser TYPE abap_bool RADIOBUTTON GROUP r2.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK screen-name = |WIID|.
    IF doc = abap_true.
      screen-input    = 1.
      screen-required = 1.
    ELSE.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_fiori_test DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING value(r_instance) TYPE REF TO lcl_fiori_test.
    METHODS: start_test.

  PRIVATE SECTION.
    METHODS: _get_url,
             _get_launchpad_url,
             _get_app_url,
             _get_document_url,
             _display.

    DATA: _url TYPE string.
ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_fiori_test IMPLEMENTATION.
  METHOD get_instance.
    CREATE OBJECT r_instance.
  ENDMETHOD.                    "get_instance

  METHOD start_test.
    _get_url( ).
    _display( ).
  ENDMETHOD.                    "start_test

  METHOD _get_url.
    CASE abap_true.
      WHEN lpad.
        _get_launchpad_url( ).
      WHEN app.
        _get_app_url( ).
      WHEN doc.
        _get_document_url( ).
    ENDCASE.
  ENDMETHOD.                    "_get_url

  METHOD _get_launchpad_url.
    CASE abap_true.
      WHEN gw.
        _url = |https://dvmobilesap.hansgrohe.com:10443/sap/bc/ui5_ui5/ui2/ushell/shells/abap/FioriLaunchpad.html|.
      WHEN portal.
        _url = |http://dvportal.hansgrohe.com:50100/irj/servlet/prt/portal/prtroot/pcd!3aportal_content!2ffolder_hansgrohe_test!2ffolder_fiori_01!2frole_gateway_fiori_launchpad!2fgateway_launchpad|.
        "url = |http://dvportal.hansgrohe.com:50100/irj/portal/gateway_fiori_launchpad|.
    ENDCASE.
  ENDMETHOD.                    "_get_launchpad_url

  METHOD _get_app_url.
    CASE abap_true.
      WHEN gw.
        _url = |https://dvmobilesap.hansgrohe.com:10443/sap/bc/ui5_ui5/ui2/ushell/shells/abap/FioriLaunchpad.html#PurchaseOrderExt-approve|.
      WHEN portal.
        _url = |http://dvportal.hansgrohe.com:50100/irj/servlet/prt/portal/prtroot/pcd!3aportal_content!2ffolder_hansgrohe_test!2ffolder_fiori_01!2frole_gateway_fiori_launchpad!2fiview_fiori_app_01|.
        "url = |http://dvportal.hansgrohe.com:50100/irj/portal/fiori_app|.
    ENDCASE.
  ENDMETHOD.                    "_get_app_url

  METHOD _get_document_url.
    CASE abap_true.
      WHEN gw.
        _url = |https://dvmobilesap.hansgrohe.com:10443/sap/bc/ui5_ui5/ui2/ushell/shells/abap/FioriLaunchpad.html#PurchaseOrderExt-approve&/HeaderDetails/WorkflowTaskCollection(SAP__Origin='HG_ERP',WorkitemID='{ wiid }')|.
      WHEN portal.
        MESSAGE 'Not supported' TYPE 'I'.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "_get_document_url

  METHOD _display.
    CHECK _url IS NOT INITIAL.

    CASE abap_true.
      WHEN gui.
        cl_abap_browser=>show_url( _url ).
      WHEN browser.
        cl_gui_frontend_services=>execute( document = _url ).
    ENDCASE.
  ENDMETHOD.                    "_display
ENDCLASS.                    "lcl_application IMPLEMENTATION

START-OF-SELECTION.
  lcl_fiori_test=>get_instance( )->start_test( ).
