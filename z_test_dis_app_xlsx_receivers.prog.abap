*&---------------------------------------------------------------------*
*& Report  Z_TEST_DIS_APP_XLSX_RECEIVERS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_dis_app_xlsx_receivers.

PARAMETERS: p_dokar TYPE draw-dokar OBLIGATORY,
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
    CLASS-METHODS: start.
ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD start.
    DATA: lo_dis_approval TYPE REF TO zcl_plm_dis_approval,
          lo_error        TYPE REF TO zcx_lo_error,
          draw_key        TYPE cvdidrawkey,
          lt_receivers    TYPE stringtab,
          text            TYPE string.

    FIELD-SYMBOLS: <receiver> LIKE LINE OF lt_receivers.

    draw_key-dokar = p_dokar.
    draw_key-doknr = p_doknr.
    draw_key-dokvr = p_dokvr.
    draw_key-doktl = p_doktl.

    TRY.
        CREATE OBJECT lo_dis_approval
          EXPORTING
            i_draw_key = draw_key.    " DDS: DRAW-Schlüssel

        lt_receivers = lo_dis_approval->get_receivers_from_dis( ).
      CATCH zcx_lo_error INTO lo_error.  " Allgemeine Fehlerklasse
        text = lo_error->get_text( ).
        MESSAGE text TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    LOOP AT lt_receivers ASSIGNING <receiver>.
      WRITE: / <receiver>.
    ENDLOOP.

    IF sy-subrc <> 0.
      WRITE: / 'Keine Empfänger gefunden'.
    ENDIF.
  ENDMETHOD.                      "start
ENDCLASS.                    "lcl_application IMPLEMENTATION

START-OF-SELECTION.
  lcl_application=>start( ).
