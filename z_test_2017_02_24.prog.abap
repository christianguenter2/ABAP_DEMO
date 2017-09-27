*&---------------------------------------------------------------------*
*& Report z_test_2017_02_24
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_02_24.

INCLUDE zmustache.
INCLUDE zmustache_ut.

CLASS lcx_error DEFINITION CREATE PUBLIC
                INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    CLASS-METHODS:
      raise_syst_error
        RAISING
          lcx_error.

    METHODS:
      constructor
        IMPORTING
          i_msg TYPE symsg,

      get_text REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: m_msg TYPE symsg.

ENDCLASS.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get
        RETURNING
          VALUE(r_instance) TYPE REF TO controller.

    CLASS-DATA:
      okcode TYPE sy-ucomm.

    METHODS:
      start,
      pai_0100,
      pbo_0100
        RAISING
          lcx_error
          lcx_mustache_error.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_html,
             line TYPE c LENGTH 132,
           END OF ty_html,
           tty_html TYPE STANDARD TABLE OF ty_html
                         WITH NON-UNIQUE DEFAULT KEY.

    CLASS-DATA:
      _instance TYPE REF TO controller.

    DATA:
      t100_tab            TYPE STANDARD TABLE OF t100,
      m_alv               TYPE REF TO cl_salv_table,
      m_docking_container TYPE REF TO cl_gui_docking_container,
      m_html_control      TYPE REF TO cl_gui_html_viewer.

    METHODS:
      _get_header_html
        RETURNING
          VALUE(rt_html) TYPE tty_html
        RAISING
          lcx_mustache_error.

ENDCLASS.

CLASS lcx_error IMPLEMENTATION.

  METHOD constructor.

    super->constructor(
      EXPORTING
        textid   = textid
        previous = previous ).

    m_msg = i_msg.

  ENDMETHOD.

  METHOD raise_syst_error.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        i_msg = VALUE #( msgid = sy-msgid
                         msgno = sy-msgno
                         msgty = sy-msgty
                         msgv1 = sy-msgv1
                         msgv2 = sy-msgv2
                         msgv3 = sy-msgv3
                         msgv4 = sy-msgv4 ).

  ENDMETHOD.

  METHOD get_text.

    IF m_msg IS NOT INITIAL.

      MESSAGE ID m_msg-msgid TYPE m_msg-msgty NUMBER m_msg-msgno
              WITH m_msg-msgv1 m_msg-msgv2 m_msg-msgv3 m_msg-msgv4
              INTO result.
      RETURN.

    ENDIF.

    result = super->get_text( ).

  ENDMETHOD.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD get.

    IF _instance IS NOT BOUND.

      _instance = NEW controller( ).

    ENDIF.

    r_instance = _instance.

  ENDMETHOD.

  METHOD start.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @t100_tab
           UP TO 100 ROWS.

    CALL SCREEN 0100.

  ENDMETHOD.

  METHOD pbo_0100.

    DATA: assigned_url TYPE char100.

    SET PF-STATUS 'STATUS_0100'.

    IF m_alv IS NOT BOUND.

      TRY.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = cl_gui_container=>screen0
            IMPORTING
              r_salv_table   = m_alv
            CHANGING
              t_table        = t100_tab ).

          m_alv->display( ).

        CATCH cx_salv_error INTO DATA(error).    "
          MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

    ENDIF.

    IF m_docking_container IS NOT BOUND.

      m_docking_container = NEW cl_gui_docking_container(
                                  side      = cl_gui_docking_container=>dock_at_top
                                  extension = 50 ).

      m_html_control = NEW cl_gui_html_viewer( parent = m_docking_container ).

      DATA(html_table) = _get_header_html( ).

      m_html_control->load_data(
        EXPORTING
          type                   = 'text'
          subtype                = 'html'
        IMPORTING
          assigned_url           = assigned_url    " URL
        CHANGING
          data_table             = html_table    " data table
        EXCEPTIONS
          OTHERS                 = 5 ).

      IF sy-subrc <> 0.
        lcx_error=>raise_syst_error( ).
      ENDIF.

      m_html_control->show_data(
        EXPORTING
          url                    = assigned_url
        EXCEPTIONS
          OTHERS                 = 5 ).

      IF sy-subrc <> 0.
        lcx_error=>raise_syst_error( ).
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD pai_0100.

    DATA(save_ok_code) = controller=>okcode.

    CLEAR: controller=>okcode.

    CASE save_ok_code.
      WHEN 'EXIT'
      OR   'BACK'
      OR   'CANC'.

        SET SCREEN 0.

    ENDCASE.

  ENDMETHOD.

  METHOD _get_header_html.

    TYPES: BEGIN OF ty_data,
             text TYPE string,
           END OF ty_data.

    DATA(template) = lcl_mustache=>create( iv_template = `<html>`
                                                      && `<body>`
                                                      && `<h1><b>{{text}}</b></h1>`
                                                      && `</body>`
                                                      && `</html>` ).

    rt_html = template->render_tt( VALUE ty_data( text = `Hallo Christian` ) ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  controller=>get( )->start( ).

MODULE pbo_0100 OUTPUT.
  controller=>get( )->pbo_0100( ).
ENDMODULE.

MODULE pai_0100 INPUT.
  controller=>get( )->pai_0100( ).
ENDMODULE.
