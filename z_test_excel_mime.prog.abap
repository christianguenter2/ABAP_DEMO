*&---------------------------------------------------------------------*
*& Report  Z_TEST_EXCEL_MIME
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_excel_mime.

*----------------------------------------------------------------------*
*       CLASS lcl_excel_factory DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_excel_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
          RETURNING value(r_instance) TYPE REF TO lcl_excel_factory.

    METHODS:
      create_from_mime_repository
        IMPORTING
          i_mime_path TYPE csequence
        RETURNING value(r_excel) TYPE REF TO zcl_excel
        RAISING zcx_excel.

ENDCLASS.                    "lcl_excel_factory DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: start.
ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_excel_factory IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_excel_factory IMPLEMENTATION.
  METHOD get_instance.
    CREATE OBJECT r_instance.
  ENDMETHOD.                    "get_instance

  METHOD create_from_mime_repository.
    DATA: lv_mime_file    TYPE xstring,
          lo_excel_reader TYPE REF TO zif_excel_reader,
          lx_excel        TYPE REF TO zcx_excel.

    cl_mime_repository_api=>get_api( )->get(
      EXPORTING  i_url             = i_mime_path
                 i_check_authority = abap_false
      IMPORTING e_content          = lv_mime_file
      EXCEPTIONS OTHERS            = 8 ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_excel
        EXPORTING
          error = |File could not be found in Mime repository at { i_mime_path }|.
    ENDIF.

    CREATE OBJECT lo_excel_reader TYPE zcl_excel_reader_2007.

    TRY.
        r_excel = lo_excel_reader->load( lv_mime_file ).
      CATCH zcx_excel INTO lx_excel.
        RAISE EXCEPTION TYPE zcx_excel
          EXPORTING
            error = |Fiel at { i_mime_path } could not be interpreted as Excel file|.
    ENDTRY.
  ENDMETHOD.                    "create_from_mime_repository
ENDCLASS.                    "lcl_excel_factory IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD start.
    TYPES: BEGIN OF ty_data,
             no   TYPE i,
             item TYPE string,
           END OF ty_data,
           tty_data TYPE STANDARD TABLE OF ty_data
                         WITH NON-UNIQUE DEFAULT KEY.

    DATA: lo_excel          TYPE REF TO zcl_excel,
          lo_writer         TYPE REF TO zif_excel_writer,
          lv_xstring        TYPE xstring,
          data_tab          TYPE solix_tab,
          lx_excel          TYPE REF TO zcx_excel,
          lt_data           TYPE tty_data,
          lv_table_settings TYPE zexcel_s_table_settings.

    FIELD-SYMBOLS: <data> LIKE LINE OF lt_data.

    TRY .
        lo_excel = lcl_excel_factory=>get_instance(
                                   )->create_from_mime_repository( `/SAP/PUBLIC/HANSGROHE/excel_templates/Type_of_goods.xlsx` ).

        lo_excel->get_active_worksheet( )->set_cell(
            ip_column = 2
            ip_row    = 1
            ip_value  = 'Fruits' ).

        INSERT INITIAL LINE INTO TABLE lt_data ASSIGNING <data>.
        <data>-no   = 1.
        <data>-item = 'Apple'.

        INSERT INITIAL LINE INTO TABLE lt_data ASSIGNING <data>.
        <data>-no   = 2.
        <data>-item = 'Pears'.

        lv_table_settings-top_left_column = 'A'.
        lv_table_settings-top_left_row    = 4.

        lo_excel->get_active_worksheet( )->bind_table(
            ip_table          = lt_data
            is_table_settings = lv_table_settings ).

        CREATE OBJECT lo_writer TYPE zcl_excel_writer_2007.
        lv_xstring = lo_writer->write_file( lo_excel ).

        data_tab = cl_bcs_convert=>xstring_to_solix( lv_xstring ).

        cl_gui_frontend_services=>gui_download(
          EXPORTING
            bin_filesize = xstrlen( lv_xstring )
            filename     = 'C:\Temp\Test_ABAP2XLSX.xlsx'
            filetype     = 'BIN'    " Dateityp (Ascii, Binär, ...)
          CHANGING
            data_tab     = data_tab    " Übergabetabelle
          EXCEPTIONS
            OTHERS       = 24 ).

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                    DISPLAY LIKE sy-msgty.
        ENDIF.

      CATCH zcx_excel INTO lx_excel.
        MESSAGE lx_excel TYPE 'S'
               DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
  ENDMETHOD.                    "start
ENDCLASS.                    "lcl_application IMPLEMENTATION

START-OF-SELECTION.
  lcl_application=>start( ).
