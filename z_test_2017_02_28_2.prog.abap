*&---------------------------------------------------------------------*
*& Report z_test_2017_02_28_2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_02_28_2.

TABLES: t100.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: s_sprsl FOR t100-sprsl,
                s_arbgb FOR t100-arbgb,
                s_msgnr FOR t100-msgnr,
                s_text  FOR t100-text.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcx_error DEFINITION FINAL
                INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    CLASS-METHODS:
      raise_text
        IMPORTING
          i_text TYPE csequence
        RAISING
          lcx_error,

      raise_syst
        RAISING
          lcx_error.

    METHODS:
      constructor
        IMPORTING
          textid   LIKE textid OPTIONAL
          previous LIKE previous OPTIONAL
          text     TYPE csequence OPTIONAL
          msg      TYPE symsg OPTIONAL,

      get_text REDEFINITION.

  PRIVATE SECTION.
    DATA: m_text TYPE string,
          m_msg  TYPE symsg.

ENDCLASS.

CLASS selection DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          i_current_report TYPE rsvar-report OPTIONAL
        RAISING
          lcx_error,
      get
        IMPORTING
          i_name           TYPE string
        EXPORTING
          et_select_option TYPE ANY TABLE
        RAISING
          lcx_error.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_select_option,
             name  TYPE string,
             value TYPE REF TO data,
           END OF ty_select_option,
           tty_select_option TYPE HASHED TABLE OF ty_select_option
                             WITH UNIQUE KEY name.
    DATA: mt_select_option TYPE tty_select_option.

ENDCLASS.

CLASS model DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      get_data
        IMPORTING
          io_selection TYPE REF TO selection
        EXPORTING
          et_data      TYPE ANY TABLE
        RAISING
          lcx_error.

ENDCLASS.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(r_instance) TYPE REF TO controller.

    CLASS-DATA: okcode   TYPE syucomm.

    METHODS:
      constructor
        RAISING
          lcx_error,
      start
        RAISING
          lcx_error,
      pbo_0100,
      pai_0100.

  PRIVATE SECTION.
    CLASS-DATA _instance TYPE REF TO controller .

    DATA: t100_tab     TYPE STANDARD TABLE OF t100,
          m_alv        TYPE REF TO cl_salv_table,
          mo_model     TYPE REF TO model,
          mo_selection TYPE REF TO selection.

    METHODS:
      _select
        RAISING
          lcx_error,
      _process,
      _display,

      _on_added_function FOR EVENT added_function OF cl_salv_events_table
        IMPORTING e_salv_function,

      _dispatch_fcode
        IMPORTING
          i_fcode TYPE syucomm.

ENDCLASS.

CLASS lcx_error IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid = textid previous = previous ).

    m_text = text.
    m_msg  = msg.

  ENDMETHOD.

  METHOD raise_text.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        text = i_text.

  ENDMETHOD.

  METHOD get_text.

    IF m_text IS NOT INITIAL.

      result = m_text.
      RETURN.

    ENDIF.

    IF m_msg IS NOT INITIAL.

      MESSAGE ID m_msg-msgid TYPE m_msg-msgty NUMBER m_msg-msgno
              WITH m_msg-msgv1 m_msg-msgv2 m_msg-msgv3 m_msg-msgv4
              INTO result.
      RETURN.

    ENDIF.

    result = super->get_text( ).

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

ENDCLASS.

CLASS selection IMPLEMENTATION.

  METHOD constructor.

    DATA: selection_table TYPE STANDARD TABLE OF rsparams.

    DATA(current_report) = COND #( WHEN i_current_report IS SUPPLIED THEN i_current_report
                                   ELSE cl_abap_syst=>get_current_program( ) ).

    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = current_report    " Programm für den Sel. angezeigt werden sollen
      TABLES
        selection_table = selection_table    " Tabelle mit Ranges-Struktur die Sel. enthält.
      EXCEPTIONS
        OTHERS          = 3.

    IF sy-subrc <> 0.
      lcx_error=>raise_syst( ).
    ENDIF.

    LOOP AT selection_table ASSIGNING FIELD-SYMBOL(<selection>).

      DATA(fname) = SWITCH #( <selection>-kind
                      WHEN 'P' THEN |({ current_report }){ <selection>-selname }|
                      WHEN 'S' THEN |({ current_report }){ <selection>-selname }[]|
                      ELSE THROW lcx_error( text = |Invalid type kind { <selection>-kind }| ) ).

      ASSIGN (fname) TO FIELD-SYMBOL(<value>).
      ASSERT sy-subrc = 0.

      INSERT VALUE #( name  = <selection>-selname
                      value = REF #( <value> ) )
             INTO TABLE mt_select_option.

    ENDLOOP.

  ENDMETHOD.

  METHOD get.

    ASSIGN mt_select_option[ name = i_name ] TO FIELD-SYMBOL(<select_option>).
    IF sy-subrc <> 0.
      lcx_error=>raise_text( |Invalid Select-Option/Parameter { i_name }!| ).
    ENDIF.

    ASSIGN <select_option>-value->* TO FIELD-SYMBOL(<value>).
    ASSERT sy-subrc = 0.

    et_select_option = <value>.

  ENDMETHOD.

ENDCLASS.

CLASS model IMPLEMENTATION.

  METHOD get_data.

    DATA: ltr_sprsl TYPE RANGE OF t100-sprsl,
          ltr_arbgb TYPE RANGE OF t100-arbgb,
          ltr_msgnr TYPE RANGE OF t100-msgnr,
          ltr_text  TYPE RANGE OF t100-text.

    io_selection->get(
      EXPORTING i_name           = 'S_SPRSL'
      IMPORTING et_select_option = ltr_sprsl ).

    io_selection->get(
      EXPORTING i_name           = 'S_ARBGB'
      IMPORTING et_select_option = ltr_arbgb ).

    io_selection->get(
      EXPORTING i_name           = 'S_MSGNR'
      IMPORTING et_select_option = ltr_msgnr ).

    io_selection->get(
      EXPORTING i_name           = 'S_TEXT'
      IMPORTING et_select_option = ltr_text ).

    SELECT FROM t100
           FIELDS *
           WHERE sprsl IN @ltr_sprsl
           AND   arbgb IN @ltr_arbgb
           AND   msgnr IN @ltr_msgnr
           AND   text  IN @ltr_text
           INTO TABLE @et_data
           UP TO 100 ROWS.

  ENDMETHOD.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD constructor.

    mo_selection = NEW selection( ).
    mo_model = NEW model( ).

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

  METHOD _display.

    CALL SCREEN 0100.

  ENDMETHOD.

  METHOD _process.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).

    ENDLOOP.

  ENDMETHOD.

  METHOD _select.

    mo_model->get_data(
      EXPORTING
        io_selection  = mo_selection
      IMPORTING
        et_data       = t100_tab ).

  ENDMETHOD.

  METHOD pbo_0100.

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

          m_alv->get_functions( )->add_function( name     = |TEST|
                                                 icon     = |{ icon_led_red }|
                                                 text     = |Test|
                                                 tooltip  = |Test|
                                                 position = if_salv_c_function_position=>right_of_salv_functions ).

          DATA(event) = m_alv->get_event( ).

          SET HANDLER _on_added_function FOR event.

          m_alv->display( ).

        CATCH cx_salv_error INTO DATA(error).    "
          MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD pai_0100.

    DATA(save_ok_code) = okcode.

    CLEAR okcode.

    _dispatch_fcode( save_ok_code ).

  ENDMETHOD.

  METHOD _dispatch_fcode.

    CASE i_fcode.
      WHEN 'BACK'
      OR   'CANC'
      OR   'EXIT'.

        SET SCREEN 0.

      WHEN OTHERS.

        MESSAGE |{ i_fcode }| TYPE 'I'.

    ENDCASE.

  ENDMETHOD.

  METHOD _on_added_function.

    _dispatch_fcode( e_salv_function ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  TRY.
      controller=>get_instance( )->start( ).

    CATCH lcx_error INTO DATA(error) .
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

MODULE pbo_0100 OUTPUT.

  TRY.
      controller=>get_instance( )->pbo_0100( ).

    CATCH lcx_error INTO error .
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDMODULE.

MODULE pai_0100 INPUT.

  TRY.
      controller=>get_instance( )->pai_0100( ).

    CATCH lcx_error INTO error .
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDMODULE.
