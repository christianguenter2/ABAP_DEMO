*&---------------------------------------------------------------------*
*& Report z_test_2017_03_16
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_03_16.

INCLUDE zmustache.
INCLUDE zmustache_ut.

CLASS lcx_error DEFINITION FINAL
                INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    CLASS-METHODS:
      raise_syst
        RAISING
          lcx_error,

      raise_exception
        IMPORTING
          io_previous TYPE REF TO cx_root
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
          textid   LIKE textid OPTIONAL
          previous LIKE previous OPTIONAL
          msg      TYPE symsg OPTIONAL
          text     TYPE csequence OPTIONAL,

      get_text REDEFINITION.

  PRIVATE SECTION.
    DATA:
      m_msg  TYPE symsg,
      m_text TYPE string.

    METHODS:
      _get_syst_msg_text
        IMPORTING
          i_msg           TYPE symsg
        RETURNING
          VALUE(r_result) TYPE string.

ENDCLASS.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(r_instance) TYPE REF TO controller.

    CLASS-DATA:
      okcode TYPE syucomm.

    METHODS:
      constructor,
      start,
      pai_0100,
      pbo_0100.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_data,
        name TYPE string,
        text TYPE string,
      END OF ty_data.

    CONSTANTS:
      c_n_times TYPE i VALUE 200.

    CLASS-DATA:
      __instance TYPE REF TO controller.

    DATA:
      mt_100               TYPE STANDARD TABLE OF t100,
      mo_alv               TYPE REF TO cl_salv_table,
      mt_icon_tab          TYPE STANDARD TABLE OF icon,
      mo_docking_container TYPE REF TO cl_gui_docking_container,
      mo_html_control      TYPE REF TO cl_gui_html_viewer.

    METHODS:
      _select,

      _processing,

      _display,

      _dispatch_fcode
        IMPORTING
          i_fcode TYPE syucomm
        RAISING
          lcx_error,

      _pbo_alv
        RAISING
          lcx_error,

      _pbo_docking
        RAISING
          lcx_error,

      _get_html
        RETURNING
          VALUE(r_html) TYPE string
        RAISING
          lcx_error,

      _pbo_html_control
        IMPORTING
          i_html TYPE string
        RAISING
          lcx_error,

      _handle_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING
            column row,

      _handle_added_function FOR EVENT added_function OF cl_salv_events_table
        IMPORTING
            e_salv_function,

      _handle_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
            column row.

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
        msg = VALUE symsg( msgty = sy-msgty
                           msgid = sy-msgid
                           msgno = sy-msgno
                           msgv1 = sy-msgv1
                           msgv2 = sy-msgv2
                           msgv3 = sy-msgv3
                           msgv4 = sy-msgv4 ).

  ENDMETHOD.

  METHOD get_text.

    result = COND #( WHEN m_msg 	 IS NOT INITIAL THEN _get_syst_msg_text( m_msg )
          					 WHEN m_text   IS NOT INITIAL THEN m_text
          					 WHEN previous IS BOUND 			THEN previous->get_text( )
          					 ELSE super->get_text( ) ).

  ENDMETHOD.

  METHOD raise_exception.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        previous = io_previous.

  ENDMETHOD.

  METHOD raise_text.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        text = i_text.

  ENDMETHOD.

  METHOD _get_syst_msg_text.

    MESSAGE ID i_msg-msgid TYPE i_msg-msgty NUMBER i_msg-msgno
            WITH i_msg-msgv1 i_msg-msgv2 i_msg-msgv3 i_msg-msgv4
            INTO r_result.

  ENDMETHOD.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD constructor.

    SELECT FROM icon
           FIELDS *
           INTO TABLE @mt_icon_tab
           UP TO @c_n_times ROWS.

  ENDMETHOD.

  METHOD get_instance.

    IF __instance IS NOT BOUND.

      __instance = NEW controller( ).

    ENDIF.

    r_instance = __instance.

  ENDMETHOD.

  METHOD start.

    _select( ).
    _processing( ).
    _display( ).

  ENDMETHOD.

  METHOD _select.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @mt_100
           UP TO 100 ROWS.

  ENDMETHOD.

  METHOD _processing.

    LOOP AT mt_100 ASSIGNING FIELD-SYMBOL(<t100>).

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
      OR   'EXIT'
      OR   'BACK'.

        SET SCREEN 0.

      WHEN OTHERS.

        lcx_error=>raise_text( |Function code { i_fcode } not supported!| ).

    ENDCASE.

  ENDMETHOD.

  METHOD _handle_added_function.

    TRY.
        _dispatch_fcode( e_salv_function ).

      CATCH lcx_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD _handle_link_click.

    MESSAGE |Row: { row } Col: { column }| TYPE 'I'.

  ENDMETHOD.

  METHOD _pbo_alv.

    CHECK mo_alv IS NOT BOUND.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = cl_gui_container=>screen0
          IMPORTING
            r_salv_table   = mo_alv
          CHANGING
            t_table        = mt_100 ).

        LOOP AT mt_icon_tab ASSIGNING FIELD-SYMBOL(<icon>).

          mo_alv->get_functions( )->add_function( name 		= |TEST{ sy-tabix }|
                                                  icon 		= |{ <icon>-id }|
                                                  tooltip	= |Test|
                                                  position = if_salv_c_function_position=>right_of_salv_functions ).

        ENDLOOP.

        CAST cl_salv_column_table( mo_alv->get_columns( )->get_column( `TEXT` ) )->set_cell_type( if_salv_c_cell_type=>hotspot ).

        DATA(event) = mo_alv->get_event( ).

        SET HANDLER:
          _handle_added_function FOR event,
          _handle_link_click		 FOR event,
          _handle_double_click	 FOR event.

        mo_alv->display( ).

      CATCH cx_salv_error INTO DATA(error).
        lcx_error=>raise_exception( error ).
    ENDTRY.

  ENDMETHOD.

  METHOD _pbo_docking.

    CHECK mo_docking_container IS NOT BOUND.

    mo_docking_container = NEW cl_gui_docking_container( side = cl_gui_docking_container=>dock_at_top ).

    _pbo_html_control( _get_html( ) ).

  ENDMETHOD.

  METHOD _get_html.

    TRY.
        DATA(lo_mustache) = lcl_mustache=>create( `<html>`
                                               && `<body>`
                                               && `<b>{{name}}</b>`
                                               && `<br/>`
                                               && `<b>{{text}}</b>`
                                               && `</body>`
                                               && `</html>` ).

        r_html = lo_mustache->render( VALUE ty_data( name = 'Christian' text = 'Hallo Welt!' ) ).

      CATCH lcx_mustache_error INTO DATA(mustache_error).
        lcx_error=>raise_exception( mustache_error ).
    ENDTRY.

  ENDMETHOD.

  METHOD _pbo_html_control.

    TYPES: tty_html TYPE STANDARD TABLE OF char100
                         WITH NON-UNIQUE DEFAULT KEY.

    DATA: assigned_url TYPE char255.

    CHECK mo_html_control IS NOT BOUND.

    DATA(html_tab) = VALUE tty_html( ( CONV #( i_html ) ) ).

    mo_html_control = NEW cl_gui_html_viewer( parent = mo_docking_container ).

    mo_html_control->load_data(
      IMPORTING
        assigned_url = assigned_url
      CHANGING
        data_table   = html_tab
      EXCEPTIONS
        OTHERS       = 5 ).

    IF sy-subrc <> 0.
      lcx_error=>raise_syst( ).
    ENDIF.

    mo_html_control->show_data(
      EXPORTING
        url    = assigned_url
      EXCEPTIONS
        OTHERS = 5 ).

    IF sy-subrc <> 0.
      lcx_error=>raise_syst( ).
    ENDIF.

  ENDMETHOD.

  METHOD _handle_double_click.

    MESSAGE |Columns: { column } Row: { row }| TYPE 'I'.

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
