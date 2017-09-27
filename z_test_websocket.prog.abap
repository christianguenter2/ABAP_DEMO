*&---------------------------------------------------------------------*
*& Report z_test_websocket
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_websocket.

DATA: g_text TYPE string.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(r_instance) TYPE REF TO controller.

    METHODS:
      start,
      pbo_0100,
      pai_0100.

  PRIVATE SECTION.
    CLASS-DATA _instance TYPE REF TO controller.
    DATA: html_control          TYPE REF TO cl_gui_html_viewer,
          custom_text_container TYPE REF TO cl_gui_custom_container,
          text_control          TYPE REF TO cl_gui_textedit,
          mt_text               TYPE STANDARD TABLE OF char100
                                     WITH NON-UNIQUE DEFAULT KEY.

    METHODS:
      on_event FOR EVENT sapevent OF  cl_gui_html_viewer
        IMPORTING
            action frame getdata postdata query_table.
    METHODS _pbo_html.
    METHODS _pbo_text.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD get_instance.

    IF _instance IS NOT BOUND.

      _instance = NEW controller( ).

    ENDIF.

    r_instance = _instance.

  ENDMETHOD.

  METHOD start.

    CALL SCREEN 0100.

  ENDMETHOD.


  METHOD pbo_0100.

    _pbo_html( ).
    _pbo_text( ).

  ENDMETHOD.

  METHOD pai_0100.

  ENDMETHOD.

  METHOD on_event.

    READ TABLE query_table ASSIGNING FIELD-SYMBOL(<line>)
                           WITH KEY name = 'text'.
    IF sy-subrc = 0.

      g_text = <line>-value.

      INSERT CONV string( <line>-value )
             INTO mt_text INDEX 1.

    ENDIF.

  ENDMETHOD.

  METHOD _pbo_html.

    DATA: lt_events TYPE cntl_simple_events.

    CHECK html_control IS NOT BOUND.

    html_control = NEW cl_gui_html_viewer( parent = cl_gui_container=>screen9 ).

    html_control->enable_sapsso(
      EXPORTING
        enabled     = abap_true
      EXCEPTIONS
        cntl_error  = 1
        OTHERS      = 2 ).

    lt_events = VALUE #( ( eventid    = 1
                           appl_event = abap_true  ) ).

    html_control->set_registered_events( lt_events ).
    SET HANDLER me->on_event FOR html_control.

    html_control->show_url(
      EXPORTING
        url 	 = 'https://rs125.hansgrohe.com:1443/sap/bc/bsp/sap/z_testwebsocket/start.htm?channel=channel_1'
      EXCEPTIONS
        OTHERS = 5 ).

  ENDMETHOD.

  METHOD _pbo_text.

    IF custom_text_container IS NOT BOUND.

      custom_text_container = NEW cl_gui_custom_container( container_name = 'CC_TEXT' ).

      text_control = NEW cl_gui_textedit( parent = custom_text_container ).

    ENDIF.

    text_control->set_text_as_r3table(
      EXPORTING
        table           = mt_text
      EXCEPTIONS
        error_dp        = 1
        error_dp_create = 2
        OTHERS          = 3 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE sy-msgty.
    ENDIF.

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
