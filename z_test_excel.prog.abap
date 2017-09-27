*&---------------------------------------------------------------------*
*& Report  Z_TEST_EXCEL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_excel.

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: start.

  PRIVATE SECTION.
    CLASS-METHODS: _create_excel,
                   _send_mail.
    CLASS-DATA: lo_excel TYPE REF TO zcl_excel.
ENDCLASS."lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD start.
    _create_excel( ).
    _send_mail( ).
  ENDMETHOD.                    "start

  METHOD _create_excel.
    DATA: lo_worksheet TYPE REF TO zcl_excel_worksheet.

    CREATE OBJECT lo_excel.

    lo_worksheet = lo_excel->get_active_worksheet( ).

    lo_worksheet->set_cell(
      EXPORTING
        ip_column    = 'B'
        ip_row       = '1'
        ip_value     = 'Hallo Welt' ).

    lo_worksheet->get_column_dimension( 'B'
               )->set_width( 30 ).

  ENDMETHOD.                    "_create_excel

  METHOD _send_mail.
    DATA: lo_bcs      TYPE REF TO cl_bcs,
          lo_document TYPE REF TO cl_document_bcs,
          lt_data     TYPE STANDARD TABLE OF mara.

    lo_bcs = cl_bcs=>create_persistent( ).

    lo_document = cl_document_bcs=>create_document( i_type        = 'HTM'
                                                    i_subject     = 'Test' ).

    lo_bcs->add_recipient( cl_cam_address_bcs=>create_internet_address( `christian.guenter@hansgrohe.com` ) ).

    zcl_bc_bcs_functions=>attach_excel_as_xlsx( EXPORTING io_excel    = lo_excel
                                                          i_filename  = 'test.xlsx'
                                                CHANGING  co_document = lo_document ).

    SELECT * from mara
             INTO TABLE lt_data
             UP TO 10 ROWS.

    zcl_bc_bcs_functions=>attach_itab_as_excel( EXPORTING itab       = lt_data
                                                          i_filename = 'abc.xlsx'
                                                CHANGING co_document = lo_document ).

    lo_bcs->set_document( lo_document ).

    lo_bcs->send( ).
    COMMIT WORK.
  ENDMETHOD.                    "_send_mail
ENDCLASS.                    "lcl_application IMPLEMENTATION

START-OF-SELECTION.
  lcl_application=>start( ).
