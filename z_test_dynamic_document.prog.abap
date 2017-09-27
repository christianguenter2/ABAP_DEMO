*&---------------------------------------------------------------------*
*& Report z_test_dynamic_document
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_dynamic_document.

TABLES: t100.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-000.
SELECT-OPTIONS: s_sprsl FOR t100-sprsl,
                s_arbgb FOR t100-arbgb,
                s_msgnr FOR t100-msgnr,
                s_text  FOR t100-text.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_screen DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS: get RETURNING VALUE(r_instance) TYPE REF TO lcl_screen.
    METHODS:
      constructor,
      pbo,
      pai.

  PRIVATE SECTION.
    CLASS-DATA _instance TYPE REF TO lcl_screen.

    DATA:
      _custom_container  TYPE REF TO cl_gui_custom_container,
      _docking_container TYPE REF TO cl_gui_docking_container,
      _doc               TYPE REF TO cl_dd_document,
      _alv               TYPE REF TO cl_salv_table,
      t100_tab           TYPE STANDARD TABLE OF t100.

ENDCLASS.

CLASS lcl_screen IMPLEMENTATION.

  METHOD get.

    IF _instance IS NOT BOUND.

      _instance = NEW lcl_screen( ).

    ENDIF.

    r_instance = _instance.

  ENDMETHOD.

  METHOD pbo.

    DATA: table     TYPE REF TO cl_dd_table_element,
          tablearea TYPE REF TO cl_dd_table_area.

    IF _custom_container IS NOT BOUND.

      _custom_container = NEW cl_gui_custom_container( 'CUSTOM_CONTAINER' ).

    ENDIF.

    IF _docking_container IS NOT BOUND.

      _docking_container = NEW cl_gui_docking_container( side      = cl_gui_docking_container=>dock_at_top
                                                         extension = 100 ).


    ENDIF.

    IF _doc IS NOT BOUND.

      _doc = NEW cl_dd_document( ).

      _doc->add_text( text      = |Test|
                      sap_style = cl_dd_area=>heading ).

      _doc->add_icon( EXPORTING sap_icon = |ICON_LED_RED| ).

      _doc->add_link( name = |Test Link|
                      url  = |http://www.google.de|
                      text = |Link| ).

      _doc->new_line( ).
      _doc->underline( ).
      _doc->new_line( ).

      _doc->add_table(
        EXPORTING
          no_of_columns               = 2
          cell_background_transparent = ''
        IMPORTING
          table                       = table    " Tabellenelement
          tablearea                   = tablearea    " Tabellenbereich
        EXCEPTIONS
          table_already_used          = 1
          OTHERS                      = 2 ).

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      table->set_column_style(
        EXPORTING
          col_no        = 1    " Nummer der Tabellenspalte
          sap_style     = 'key' ).

      tablearea->add_text( text = |S_SPRSL| ).

      tablearea->add_text( text = |{ REDUCE string( INIT result = ||
                                                    FOR <sprsl> IN s_sprsl
                                                    NEXT result = result && ` | ` && <sprsl>-low ) }| ).

      tablearea->new_row( ).

      tablearea->add_text( text = |Col 1| ).
      tablearea->add_text( text = |Col 2| ).

      tablearea->new_row( ).

      tablearea->add_text( text = |Val 1| ).
      tablearea->add_text( text = |Val 2| ).

      tablearea->new_row( ).

      _doc->merge_document( ).

      _doc->display_document(
        EXPORTING
          parent             = _docking_container
        EXCEPTIONS
          html_display_error = 1
          OTHERS             = 2 ).

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                DISPLAY LIKE sy-msgty.
      ENDIF.

    ENDIF.

    IF _alv IS NOT BOUND.

      TRY.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = _custom_container
            IMPORTING
              r_salv_table   = _alv
            CHANGING
              t_table        = t100_tab ).

          _alv->get_functions( )->set_all( abap_true ).

          _alv->display( ).

        CATCH cx_salv_error INTO DATA(error).
          MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD pai.

  ENDMETHOD.

  METHOD constructor.

    SELECT FROM t100
           FIELDS *
           WHERE sprsl IN @s_sprsl
           AND   arbgb IN @s_arbgb
           AND   msgnr IN @s_msgnr
           AND   text  IN @s_text
           INTO TABLE @t100_tab
           UP TO 100 ROWS.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  lcl_screen=>get( )->pbo( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  lcl_screen=>get( )->pai( ).
ENDMODULE.
