*&---------------------------------------------------------------------*
*& Report z_test_2017_03_29
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_03_29.

CLASS lcx_error DEFINITION FINAL
                INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    CLASS-METHODS:
      raise_syst
        RAISING
          lcx_error,

      raise_text
        IMPORTING
          i_text TYPE csequence
        RAISING
          lcx_error.

    METHODS:
      constructor
        IMPORTING
          textid   LIKE textid	 OPTIONAL
          previous LIKE previous OPTIONAL
          msg      TYPE symsg 	 OPTIONAL
          text     TYPE string OPTIONAL,

      get_text REDEFINITION.

  PRIVATE SECTION.
    DATA m_msg TYPE symsg.
    DATA m_text TYPE string.

    METHODS:
      _get_msg_text
        RETURNING
          VALUE(r_text) TYPE string .

ENDCLASS.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-DATA: okcode TYPE sy-ucomm.

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

    DATA: t100_tab TYPE STANDARD TABLE OF t100,
          alv      TYPE REF TO cl_salv_table,
          docking  TYPE REF TO cl_gui_docking_container.

    METHODS:
      _select,
      _process,
      _display,

      _on_added_function_handler FOR EVENT added_function OF cl_salv_events_table
        IMPORTING e_salv_function,

      _dispatch_fcode
        IMPORTING
          i_fcode TYPE sy-ucomm
        RAISING
          lcx_error,

      _pbo_alv,
      _pbo_docking
        RAISING
          lcx_error.

ENDCLASS.

CLASS lcx_error IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid = textid previous = previous ).

    m_msg  = msg.
    m_text = text.

  ENDMETHOD.

  METHOD raise_syst.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        msg = VALUE symsg( msgty  = sy-msgty
                           msgid  = sy-msgid
                           msgno  = sy-msgno
                           msgv1  = sy-msgv1
                           msgv2  = sy-msgv2
                           msgv3  = sy-msgv3
                           msgv4  = sy-msgv4 ).

  ENDMETHOD.

  METHOD get_text.

    result = COND #( WHEN m_msg  IS NOT INITIAL THEN _get_msg_text( )
                     WHEN m_text IS NOT INITIAL THEN m_text
                     ELSE super->get_text( ) ).

  ENDMETHOD.

  METHOD _get_msg_text.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO r_text.

  ENDMETHOD.


  METHOD raise_text.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        text = i_text.

  ENDMETHOD.

ENDCLASS.

CLASS controller IMPLEMENTATION.

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

    SELECT FROM t100
           FIELDS *
           INTO TABLE @t100_tab
           UP TO 100 ROWS.

  ENDMETHOD.

  METHOD _process.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).

    ENDLOOP.

  ENDMETHOD.

  METHOD _display.

    CALL SCREEN 0100.

  ENDMETHOD.

  METHOD pbo_0100.

    SET PF-STATUS 'STATUS_0100'.

    TRY.
        _pbo_alv( ).
        _pbo_docking( ).

      CATCH lcx_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD _pbo_alv.

    CHECK alv IS NOT BOUND.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = cl_gui_container=>screen0
          IMPORTING
            r_salv_table   = alv
          CHANGING
            t_table        = t100_tab ).

        alv->get_functions( )->add_function( name 		= |TEST|
                                             icon 		= |{ icon_led_red }|
                                             text 		= |Test|
                                             tooltip	= |Test|
                                             position = if_salv_c_function_position=>left_of_salv_functions ).

        DATA(event) = alv->get_event( ).

        SET HANDLER _on_added_function_handler FOR event.

        alv->display( ).

      CATCH cx_salv_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD pai_0100.

    DATA(save_ok_code) = okcode.

    CLEAR okcode.

    TRY.
        _dispatch_fcode( save_ok_code ).

      CATCH lcx_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD _dispatch_fcode.

    CASE i_fcode.
      WHEN 'BACK'
        OR 'CANC'
        OR 'EXIT'.

        SET SCREEN 0.

      WHEN OTHERS.

        lcx_error=>raise_text( |Function code { i_fcode } not implemented!| ).

    ENDCASE.

  ENDMETHOD.

  METHOD _on_added_function_handler.

    TRY.
        _dispatch_fcode( e_salv_function ).

      CATCH lcx_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD _pbo_docking.

    DATA: assigned_url TYPE char255,
          html_table   TYPE STANDARD TABLE OF char255
                            WITH NON-UNIQUE DEFAULT KEY.

    CHECK docking IS NOT BOUND.

    docking = NEW cl_gui_docking_container( parent    = cl_gui_container=>screen0
                                            side      = cl_gui_docking_container=>dock_at_top
                                            extension = 100 ).

    DATA(html_control) = NEW cl_gui_html_viewer( parent = docking ).

    html_table = VALUE #( ( '<html>' )
                          ( '<body>' )
                          ( '<h1>Test</h1>' )
                          ( '</body>' )
                          ( '</html>' ) ).

    html_control->load_data(
      IMPORTING
        assigned_url = assigned_url    " URL
      CHANGING
        data_table	 = html_table
      EXCEPTIONS
        OTHERS			 = 5 ).

    IF sy-subrc <> 0.
      lcx_error=>raise_syst( ).
    ENDIF.

    html_control->show_url(
      EXPORTING
        url 	 = assigned_url
      EXCEPTIONS
        OTHERS = 5 ).

    IF sy-subrc <> 0.
      lcx_error=>raise_syst( ).
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
