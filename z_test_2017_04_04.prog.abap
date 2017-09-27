*&---------------------------------------------------------------------*
*& Report z_test_2017_04_04
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_04_04.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(r_instance) TYPE REF TO controller.

    METHODS:
      constructor,
      start,
      pbo_0100,
      pai_0100.

  PRIVATE SECTION.
    CLASS-DATA: _instance TYPE REF TO controller.

    DATA:
      t100_tab    TYPE STANDARD TABLE OF t100,
      alv         TYPE REF TO cl_salv_table,
      html_viewer TYPE REF TO cl_gui_html_viewer,
      rnd         TYPE REF TO cl_abap_random_int.

    METHODS:
      _select,
      _process,
      _display,
      _pbo_alv,
      _pbo_html,

      _on_event FOR EVENT sapevent OF cl_gui_html_viewer
        IMPORTING action frame  getdata postdata query_table,
      _refresh.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD constructor.

    rnd = cl_abap_random_int=>create( seed = CONV #( sy-uzeit )
                                      min  = 1
                                      max  = 20 ).

  ENDMETHOD.

  METHOD get_instance.

    IF _instance IS NOT BOUND.

      _instance = NEW controller( ).

    ENDIF.

    r_instance = _instance.

  ENDMETHOD.


  METHOD start.

    _select( ).
    _process( ).
    _display( ).

  ENDMETHOD.


  METHOD _select.

    DATA(rows) = rnd->get_next( ).

    SELECT FROM t100
           FIELDS *
           INTO TABLE @t100_tab
           UP TO @rows ROWS.

  ENDMETHOD.


  METHOD _process.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).

    ENDLOOP.

  ENDMETHOD.


  METHOD _display.

    CALL SCREEN 0100.

  ENDMETHOD.


  METHOD pbo_0100.

    _pbo_alv( ).
    _pbo_html( ).

  ENDMETHOD.

  METHOD _pbo_alv.

    IF alv IS NOT BOUND.

      TRY.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = cl_gui_container=>screen0
            IMPORTING
              r_salv_table   = alv
            CHANGING
              t_table        = t100_tab ).

          alv->display( ).

        CATCH cx_salv_error INTO DATA(error).
          MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

    ELSE.

      _refresh( ).

    ENDIF.

  ENDMETHOD.

  METHOD pai_0100.

  ENDMETHOD.


  METHOD _pbo_html.

    CHECK html_viewer IS NOT BOUND.

    html_viewer = NEW cl_gui_html_viewer( parent = cl_gui_container=>screen9 ).

    html_viewer->enable_sapsso(
      EXPORTING
        enabled = abap_true
      EXCEPTIONS
        OTHERS	= 2 ).

    html_viewer->set_registered_events(
      EXPORTING
        events = VALUE #( ( eventid    = 1
                            appl_event = abap_true ) )
      EXCEPTIONS
        OTHERS = 4 ).

    SET HANDLER _on_event FOR html_viewer.

    html_viewer->show_url(
      EXPORTING
        url 	 = 'https://rs125.hansgrohe.com:1443/sap/bc/bsp/sap/z_testwebsocket/start.htm?channel=channel_1'
      EXCEPTIONS
        OTHERS = 5 ).

  ENDMETHOD.

  METHOD _on_event.

    _select( ).

  ENDMETHOD.

  METHOD _refresh.

    TRY.
        alv->set_data( CHANGING t_table = t100_tab ).
        alv->refresh( ).

      CATCH cx_salv_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  controller=>get_instance( )->start( ).

MODULE pbo_0100 OUTPUT.
  controller=>get_instance( )->pbo_0100( ).
ENDMODULE.

MODULE pai_0100 INPUT.
  controller=>get_instance( )->pai_0100( ).
ENDMODULE.
