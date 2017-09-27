*&---------------------------------------------------------------------*
*& Report  z_test_read_table_multi
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT z_test_read_table_multi.

PARAMETERS: p_vbeln TYPE vbeln OBLIGATORY DEFAULT '5801023001'.

*----------------------------------------------------------------------*
*       CLASS text_texts DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS test_texts DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING value(ro_instance) TYPE REF TO test_texts.

    METHODS:
      start.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:
      _read_multiple_texts
        RAISING
          zcx_lo_error,

      _read_text_table
        RAISING
          zcx_lo_error,

      _out
        IMPORTING
          it_text_table  TYPE text_lh
          it_error_table TYPE text_lh.

ENDCLASS.                    "text_texts DEFINITION

*----------------------------------------------------------------------*
*       CLASS text_texts IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS test_texts IMPLEMENTATION.

  METHOD get_instance.

    CREATE OBJECT ro_instance.

  ENDMETHOD.                    "get

  METHOD start.

    DATA: error TYPE REF TO zcx_lo_error.

    TRY.
        _read_multiple_texts( ).
        _read_text_table( ).

        cl_demo_output=>display(  ).

      CATCH zcx_lo_error INTO error.
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.                    "start

  METHOD _read_multiple_texts.

    DATA: name_ranges   TYPE tspsrname,
          name_range    LIKE LINE OF name_ranges,
          object_ranges TYPE tspsrobj,
          object_range  LIKE LINE OF object_ranges,
          text_table    TYPE text_lh,
          error_table   TYPE text_lh.

    name_range-sign   = 'I'.
    name_range-option = 'EQ'.
    name_range-low    = p_vbeln.
    INSERT name_range INTO TABLE name_ranges.

    object_range-sign   = 'I'.
    object_range-option = 'EQ'.
    object_range-low    = 'VBBK'.
    INSERT object_range INTO TABLE object_ranges.

    CALL FUNCTION 'READ_MULTIPLE_TEXTS'
      EXPORTING
        name_ranges             = name_ranges    " Tabelle für Textnamen (RANGES)
        object_ranges           = object_ranges    " Tabelle für Textobjekte (RANGES)
      IMPORTING
        text_table              = text_table    " Tabelle mit Text-Header und Inhalten
        error_table             = error_table    " Texte mit Fehlern beim Lesen
      EXCEPTIONS
        wrong_access_to_archive = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      zcx_lo_error=>raise_syst_message( ).
    ENDIF.

    _out( it_text_table  = text_table
          it_error_table = error_table ).

  ENDMETHOD.                    "_read_multiple_texts

  METHOD _read_text_table.

    DATA: tdname  TYPE stxh-tdname,
          lt_stxh TYPE STANDARD TABLE OF stxh
                       WITH NON-UNIQUE DEFAULT KEY,
          text_headers TYPE STANDARD TABLE OF thead
                            WITH NON-UNIQUE DEFAULT KEY,
          text_header LIKE LINE OF text_headers,
          text_table  TYPE text_lh,
          error_table TYPE text_lh.

    FIELD-SYMBOLS: <stxh> LIKE LINE OF lt_stxh.

    tdname = p_vbeln.

    SELECT * FROM stxh
             INTO TABLE lt_stxh
             WHERE tdobject = 'VBBK'
             AND   tdname   = tdname.

    LOOP AT lt_stxh ASSIGNING <stxh>.
      MOVE-CORRESPONDING <stxh> TO text_header.
      INSERT text_header INTO TABLE text_headers.
    ENDLOOP.

    CALL FUNCTION 'READ_TEXT_TABLE'
      IMPORTING
        text_table              = text_table    " Tabelle mit Text-Header und Inhalten
        error_table             = error_table    " Texte mit Fehlern beim Lesen
      TABLES
        text_headers            = text_headers    " SAPscript: Text-Header
      EXCEPTIONS
        wrong_access_to_archive = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      zcx_lo_error=>raise_syst_message( ).
    ENDIF.

    _out( it_text_table  = text_table
          it_error_table = error_table ).

  ENDMETHOD.                    "_read_text_table

  METHOD _out.

    DATA: text LIKE LINE OF it_text_table.

    READ TABLE it_text_table INTO text INDEX 1.

    cl_demo_output=>write_data( text-lines ).
    cl_demo_output=>write_data( it_error_table ).

  ENDMETHOD.                    "_out

ENDCLASS.                    "text_texts IMPLEMENTATION

START-OF-SELECTION.
  test_texts=>get_instance( )->start( ).
