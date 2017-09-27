*&---------------------------------------------------------------------*
*& Report  Z_TEST_ABAP2XLSX_DIS2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_abap2xlsx_dis2.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_dokar TYPE draw-dokar OBLIGATORY,
            p_doknr TYPE draw-doknr OBLIGATORY,
            p_dokvr TYPE draw-dokvr OBLIGATORY,
            p_doktl TYPE draw-doktl OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

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
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  DEFINE _set_cell.
    xlsx_to_dis->get_xlsx(
              )->get_active_worksheet(
              )->set_cell( ip_column = &1
                           ip_row    = &2
                           ip_value  = &3 ).
  END-OF-DEFINITION.

  METHOD start.
    DATA: xlsx_to_dis TYPE REF TO zcl_xlsx_dis_interface,
          error       TYPE REF TO cx_root,
          text        TYPE string.

    TRY.
        CREATE OBJECT xlsx_to_dis
          EXPORTING
            i_dokar = p_dokar
            i_doknr = p_doknr
            i_dokvr = p_dokvr
            i_doktl = p_doktl.

        _set_cell: 'A' 1 'Hallo Welt!',
                   'A' 2 1234,
                   'A' 3 7890.

        xlsx_to_dis->save_xlsx( ).
      CATCH zcx_excel zcx_lo_error INTO error.
        text = error->get_text( ).
        MESSAGE text TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.                    "start
ENDCLASS.                    "lcl_application IMPLEMENTATION

START-OF-SELECTION.
  lcl_application=>start( ).
