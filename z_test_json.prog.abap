*&---------------------------------------------------------------------*
*& Report  Z_TEST_JSON
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z_test_json.

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: start.

  PRIVATE SECTION.

    TYPES: BEGIN OF data_type,
             username TYPE string,
             firstname TYPE string,
             lastname TYPE string,
             fullname TYPE string,
           END OF data_type.
ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD start.
    DATA: lv_data TYPE data_type,
          json_data TYPE zut_data,
          json_string TYPE string,
          json_parser TYPE REF TO zcl_json_parser.

    lv_data-username = 'GUENTERC'.
    lv_data-firstname = 'Guenter'.
    lv_data-lastname = 'Christian'.
    lv_data-fullname = 'Christian Guenter'.

    json_string = zcl_sup_dcn=>abap2json(
                      abap_data   = lv_data
*                      name        = 'ABAP_DATA'
*                      no_wrap     =
*                      strict      = 'X'
*                      upcase      =
                         ).

    WRITE: / json_string.

*TRY.
    CLEAR lv_data.
    CREATE OBJECT json_parser.
    json_data = json_parser->parse( iv_json = json_string  ).

* CATCH zcx_parse_error .
*ENDTRY.


  ENDMETHOD.                    "start
ENDCLASS.                    "lcl_application IMPLEMENTATION

START-OF-SELECTION.
  lcl_application=>start( ).
