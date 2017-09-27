*&---------------------------------------------------------------------*
*& Report  Z_TEST_ABAP2XLSX_DIS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_abap2xlsx_dis.

PARAMETER: p_dokar TYPE draw-dokar OBLIGATORY,
           p_doknr TYPE draw-doknr OBLIGATORY,
           p_dokvr TYPE draw-dokvr OBLIGATORY,
           p_doktl TYPE draw-doktl OBLIGATORY.

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS: start.
ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD start.
    DATA: lo_xlsx_dis_interface TYPE REF TO zcl_xlsx_dis_interface,
          lo_error              TYPE REF TO cx_root,
          lo_excel              TYPE REF TO zcl_excel,
          lo_worksheet          TYPE REF TO zcl_excel_worksheet,
          text                  TYPE string.

    FIELD-SYMBOLS: <sheet_content> LIKE LINE OF lo_worksheet->sheet_content.

    CREATE OBJECT lo_xlsx_dis_interface
      EXPORTING
        i_dokar = p_dokar    " Dokumentart
        i_doknr = p_doknr    " Dokumentnummer
        i_dokvr = p_dokvr    " Dokumentversion
        i_doktl = p_doktl.

    TRY.
        lo_excel     = lo_xlsx_dis_interface->get_xlsx( ).
        lo_worksheet = lo_excel->get_active_worksheet( ).

        LOOP AT  lo_worksheet->sheet_content ASSIGNING <sheet_content>.
          WRITE: / <sheet_content>-cell_row, ` `, <sheet_content>-cell_column, ` `, <sheet_content>-cell_value.
        ENDLOOP.

        IF sy-subrc = 0.
          lo_worksheet->set_cell(
            EXPORTING
              ip_column    = <sheet_content>-cell_column
              ip_row       = <sheet_content>-cell_row + 1
              ip_value     = sy-uname ).
        ELSE.
          lo_worksheet->set_cell(
            EXPORTING
              ip_column    = 'A'
              ip_row       = 1
              ip_value     = sy-uname ).
        ENDIF.

        lo_xlsx_dis_interface->save_xlsx( ).
      CATCH zcx_excel INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
      CATCH zcx_lo_error INTO lo_error.
        text = lo_error->get_text( ).
        MESSAGE text TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.                    "start
ENDCLASS.                    "lcl_application IMPLEMENTATION

START-OF-SELECTION.
  DATA: lo_application TYPE REF TO lcl_application.

  CREATE OBJECT lo_application.
  lo_application->start( ).
