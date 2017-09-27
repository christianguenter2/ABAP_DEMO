*&---------------------------------------------------------------------*
*& Report  Z_TEST_XSLT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_xslt.

*----------------------------------------------------------------------*
*       CLASS lcl_test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test DEFINITION.
  PUBLIC SECTION.
    METHODS: start.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_cur,
             numcode  TYPE string,
             charcode TYPE string,
             nominal  TYPE string,
             name     TYPE string,
             value    TYPE string,
           END   OF ty_cur,
           tty_cur TYPE STANDARD TABLE OF ty_cur
                        WITH NON-UNIQUE DEFAULT KEY.
ENDCLASS.                    "lcl_test DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test IMPLEMENTATION.
  METHOD start.
    DATA: data_tab TYPE solix_tab,
          xml      TYPE xstring.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = 'C:\Temp\test_ru01.xml'
        filetype                = 'BIN'
      CHANGING
        data_tab                = data_tab    " Übergabetabelle für Datei-Inhalt
      EXCEPTIONS
        OTHERS                  = 19 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

    DATA: lt_data    TYPE tty_cur,
          xml_string TYPE string.

    "xml = concat_lines_of( data_tab ).

    CALL TRANSFORMATION z_exchange_rates_ru01
         SOURCE XML data_tab
         RESULT XML xml_string.

    cl_demo_output=>display_xml( xml_string ).

    CALL TRANSFORMATION z_exchange_rates_ru01
         SOURCE XML data_tab
         RESULT values = lt_data.

    cl_demo_output=>display_data( lt_data ).
  ENDMETHOD.                    "start
ENDCLASS.                    "lcl_test IMPLEMENTATION

START-OF-SELECTION.
  DATA: lo_test TYPE REF TO lcl_test.
  CREATE OBJECT lo_test.
  lo_test->start( ).
