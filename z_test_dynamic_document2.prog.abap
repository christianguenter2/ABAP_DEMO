*&---------------------------------------------------------------------*
*& Report z_test_dynamic_document2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_dynamic_document2.

PARAMETERS: dummy.

CLASS dyn_doc DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

  PRIVATE SECTION.
    DATA: t100_tab                 TYPE STANDARD TABLE OF t100,
          alv                      TYPE REF TO cl_salv_table,
          docking_container_top    TYPE REF TO cl_gui_docking_container,
          docking_container_bottom TYPE REF TO cl_gui_docking_container.

    METHODS:
      _select,
      _display,
      _on_double_click
        FOR EVENT double_click
        OF cl_salv_events_table.

ENDCLASS.

CLASS dyn_doc IMPLEMENTATION.

  METHOD start.

    _select( ).
    _display( ).

  ENDMETHOD.

  METHOD _display.


*    docking_container_bottom = NEW cl_gui_docking_container( ratio     = 80
*                                                             extension = 200
*                                                             style     = cl_gui_docking_container=>dock_at_bottom ).


    TRY.
        cl_salv_table=>factory(
*         EXPORTING
*           r_container    = docking_container_bottom
         IMPORTING
           r_salv_table   = alv    " Basisklasse einfache ALV Tabellen
         CHANGING
           t_table        = t100_tab ).

        DATA(event) = alv->get_event( ).

        SET HANDLER _on_double_click FOR event.

        alv->display( ).

      CATCH cx_salv_error INTO DATA(error).    "
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD _select.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @t100_tab
           UP TO 100 ROWS.

  ENDMETHOD.

  METHOD _on_double_click.

    docking_container_top = NEW cl_gui_docking_container( extension = 100
                                                          style     = cl_gui_docking_container=>dock_at_left ).
    DATA(doc) = NEW cl_dd_document( ).

    doc->add_text( text = |Test| ).

    doc->merge_document( ).

    doc->display_document(
      EXPORTING
        parent             = docking_container_top
      EXCEPTIONS
        html_display_error = 1
        OTHERS             = 2 ).

  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN.
  NEW dyn_doc( )->start( ).
