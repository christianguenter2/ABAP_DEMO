*&---------------------------------------------------------------------*
*& Report z_test_2017_02_21
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_02_21.

TABLES: t100.
DATA: ok_code TYPE sy-ucomm.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: s_sprsl FOR t100-sprsl,
                s_arbgb FOR t100-arbgb,
                s_msgnr FOR t100-msgnr,
                s_text  FOR t100-text.
SELECTION-SCREEN END OF BLOCK b1.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS get
      RETURNING
        VALUE(r_instance) TYPE REF TO controller.

    METHODS:
      constructor,
      start,
      pbo_0100,
      pai_0100.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF co_fcode,
                 back   TYPE sy-ucomm VALUE `BACK`,
                 exit   TYPE sy-ucomm VALUE `EXIT`,
                 test   TYPE sy-ucomm VALUE `&TEST`,
                 cancel TYPE sy-ucomm VALUE `CANC`,
               END OF co_fcode.
    CLASS-DATA _instance TYPE REF TO controller.

    DATA: t100_tab           TYPE STANDARD TABLE OF t100,
          m_custom_container TYPE REF TO cl_gui_docking_container,
          visible            TYPE c,
          m_first_time       TYPE abap_bool.

    METHODS:
      _processing,
      _select,
      _display,
      _on_added_function FOR EVENT added_function
            OF cl_salv_events_table IMPORTING e_salv_function,
      _toggle_docking_container,
      _show_select_options,
      _dispatch_fcode
            IMPORTING
              i_fcode TYPE sy-ucomm.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

    _select( ).
    _processing( ).
    _display( ).

  ENDMETHOD.

  METHOD _select.

    SELECT FROM t100
           FIELDS *
           WHERE sprsl IN @s_sprsl
           AND   arbgb IN @s_arbgb
           AND   msgnr IN @s_msgnr
           AND   text  IN @s_text
           INTO TABLE @t100_tab
           UP TO 100 ROWS.

  ENDMETHOD.

  METHOD _processing.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).

    ENDLOOP.

  ENDMETHOD.

  METHOD _display.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = cl_gui_custom_container=>screen0
          IMPORTING
            r_salv_table   = DATA(alv)    " Basisklasse einfache ALV Tabellen
          CHANGING
            t_table        = t100_tab ).

        alv->get_functions( )->add_function(
          EXPORTING
            name               = |&TEST|
            text               = |Test|
            tooltip            = |Test|
            position           = if_salv_c_function_position=>right_of_salv_functions ).

        DATA(event) = alv->get_event( ).

        SET HANDLER _on_added_function FOR event.

        alv->get_functions( )->set_all( ).
        alv->display( ).

      CATCH cx_salv_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    CALL SCREEN 0100.

  ENDMETHOD.

  METHOD _on_added_function.

    _dispatch_fcode( e_salv_function ).

  ENDMETHOD.

  METHOD get.

    IF _instance IS NOT BOUND.

      _instance = NEW controller( ).

    ENDIF.

    r_instance = _instance.

  ENDMETHOD.

  METHOD pbo_0100.

    SET PF-STATUS 'SCREEN_0100'.
    SET TITLEBAR 'TITLE_0100'.

    IF m_first_time = abap_true.

      m_first_time = abap_false.

      _toggle_docking_container( ).
      _show_select_options( ).

    ENDIF.

  ENDMETHOD.

  METHOD pai_0100.

    DATA(save_ok_code) = ok_code.

    CLEAR ok_code.

    _dispatch_fcode( save_ok_code ).

  ENDMETHOD.

  METHOD _toggle_docking_container.

    IF m_custom_container IS NOT BOUND.

      m_custom_container = NEW cl_gui_docking_container(
                                 parent = cl_gui_container=>screen0
                                 side   = cl_gui_docking_container=>dock_at_top
                                 ratio  = 10 ).

      visible = abap_true.

    ELSE.

      visible = boolc( visible = abap_false ).

      m_custom_container->set_visible(
        EXPORTING
          visible           = visible    " visible/invisible state flag
        EXCEPTIONS
          OTHERS            = 3 ).

    ENDIF.

  ENDMETHOD.

  METHOD _show_select_options.

    DATA: selection_table TYPE STANDARD TABLE OF rsparams.

    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = cl_abap_syst=>get_current_program( )
      TABLES
        selection_table = selection_table    " Tabelle mit Ranges-Struktur die Sel. enthält.
      EXCEPTIONS
        OTHERS          = 3.

  ENDMETHOD.

  METHOD constructor.

    m_first_time = abap_true.

  ENDMETHOD.

  METHOD _dispatch_fcode.

    CASE i_fcode.
      WHEN co_fcode-back
      OR   co_fcode-cancel
      OR   co_fcode-exit.

        SET SCREEN 0.

      WHEN co_fcode-test.

        _toggle_docking_container( ).

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  controller=>get( )->start( ).

*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  controller=>get( )->pbo_0100( ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  controller=>get( )->pai_0100( ).
ENDMODULE.
