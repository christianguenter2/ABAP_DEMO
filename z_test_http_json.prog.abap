*&---------------------------------------------------------------------*
*& Report  Z_TEST_HTTP_JSON
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_http_json.

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: start.
  PRIVATE SECTION.
    CLASS-DATA: lo_client TYPE REF TO if_http_client.
ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD start.
    DATA: message TYPE string.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = 'http://openlibrary.org'    " URL
        proxy_host         = '10.6.9.240'
        proxy_service      = '8080'
*        ssl_id             =     " SSL Identität
*        sap_username       =     " R/3-System, Anmeldename des Benutzers
*        sap_client         =     " R/3-System, Mandantennummer aus Anmeldung
      IMPORTING
        client             = lo_client    " HTTP Client Abstraction
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I'  NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                DISPLAY LIKE sy-msgty.
    ENDIF.

    lo_client->request->set_method( cl_http_entity=>co_request_method_get ).

    lo_client->request->set_version(
          version = if_http_request=>co_protocol_version_1_0 ).

    cl_http_utility=>set_request_uri(
            request = lo_client->request
            uri     = '/api/books?bibkeys=ISBN:0385472579,LCCN:62019420&format=json' ).

    lo_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I'  NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                      DISPLAY LIKE sy-msgty.
    ENDIF.

    lo_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).

    IF sy-subrc <> 0.
      lo_client->get_last_error(
        IMPORTING
*          code    =     " Rückgabewert, Rückgabewert nach ABAP-Anweisungen
          message = message     " Fehlermeldung
      ).
      MESSAGE message TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.

    DATA: xstring TYPE xstring,
          string TYPE string.

    string = lo_client->response->get_cdata( ).
    xstring = lo_client->response->get_data( ).

    lo_client->close(
      EXCEPTIONS
        http_invalid_state = 1
        OTHERS             = 2 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I'  NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                DISPLAY LIKE sy-msgty.
    ENDIF.

    DATA: json_parser TYPE REF TO zcl_json_parser,
          zut_data TYPE zut_data.

    CREATE OBJECT json_parser.

    zut_data = json_parser->parse( iv_json = string ).

  ENDMETHOD.                    "start
ENDCLASS.                    "lcl_application IMPLEMENTATION


START-OF-SELECTION.
  lcl_application=>start( ).
