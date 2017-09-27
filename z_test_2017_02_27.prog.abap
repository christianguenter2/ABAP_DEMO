*&---------------------------------------------------------------------*
*& Report z_test_2017_02_27
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_02_27.

TABLES: t100.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: s_sprsl FOR t100-sprsl,
                s_arbgb FOR t100-arbgb,
                s_msgnr FOR t100-msgnr,
                s_text  FOR t100-text.
PARAMETERS: p_rows TYPE i OBLIGATORY DEFAULT '100'.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcx_error DEFINITION CREATE PUBLIC
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
          textid   LIKE textid OPTIONAL
          previous LIKE previous OPTIONAL
          msg      TYPE symsg OPTIONAL
          text     TYPE csequence OPTIONAL,

      get_text REDEFINITION.

  PRIVATE SECTION.
    DATA: m_msg  TYPE symsg,
          m_text TYPE string.

ENDCLASS.

CLASS select_options DEFINITION DEFERRED.

INTERFACE lif_data_provider.
  METHODS:
    get_data
      IMPORTING
        io_select_options TYPE REF TO select_options
      EXPORTING
        et_data           TYPE ANY TABLE
      RAISING
        lcx_error.

ENDINTERFACE.

CLASS model_factory DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_model_for
        IMPORTING
          i_type          TYPE csequence
        RETURNING
          VALUE(ro_model) TYPE REF TO lif_data_provider
        RAISING
          lcx_error.

ENDCLASS.

CLASS select_options DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          i_program TYPE sy-repid OPTIONAL
        RAISING
          lcx_error,

      get
        IMPORTING
          i_name           TYPE string
        EXPORTING
          et_select_option TYPE ANY TABLE
          e_parameter      TYPE data
        RAISING
          lcx_error.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_select_option,
             name TYPE string,
             data TYPE REF TO data,
           END OF ty_select_option,
           tty_select_option TYPE HASHED TABLE OF ty_select_option
                             WITH UNIQUE KEY name.

    DATA: mt_select_options TYPE tty_select_option.

ENDCLASS.

CLASS model DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      lif_data_provider.

ENDCLASS.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get
        RETURNING
          VALUE(r_instance) TYPE REF TO controller
        RAISING
          lcx_error.

    CLASS-DATA:
      ok_code  TYPE syucomm.

    METHODS:
      constructor
        RAISING
          lcx_error,

      start
        RAISING
          lcx_error,
      pai_0100,
      pbo_0100.

  PRIVATE SECTION.
    CLASS-DATA:
      _instance TYPE REF TO controller.

    DATA:
      mt_t100           TYPE STANDARD TABLE OF t100,
      m_alv             TYPE REF TO cl_salv_table,
      mo_model          TYPE REF TO lif_data_provider,
      mo_select_options TYPE REF TO select_options.

    METHODS:
      _on_added_function FOR EVENT added_function OF cl_salv_events_table
        IMPORTING
            e_salv_function,

      _dispatch_fcode
        IMPORTING
          i_fcode TYPE salv_de_function,

      _pbo_0100_alv.

ENDCLASS.

CLASS lcx_error IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid   = textid
                        previous = previous ).

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

    IF m_msg IS NOT INITIAL.

      MESSAGE ID m_msg-msgid TYPE m_msg-msgty NUMBER m_msg-msgno
              WITH m_msg-msgv1 m_msg-msgv2 m_msg-msgv3 m_msg-msgv4
              INTO result.
      RETURN.

    ENDIF.

    IF m_text IS NOT INITIAL.

      result = m_text.
      RETURN.

    ENDIF.

    result = super->get_text( ).

  ENDMETHOD.

  METHOD raise_text.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        text = i_text.

  ENDMETHOD.

ENDCLASS.

CLASS model_factory IMPLEMENTATION.

  METHOD get_model_for.

    ro_model = SWITCH #( i_type WHEN `T100` THEN NEW model( )
                                ELSE THROW lcx_error( text = |Model { i_type } not supported!| ) ).

  ENDMETHOD.

ENDCLASS.

CLASS select_options IMPLEMENTATION.

  METHOD constructor.

    DATA: selection_table TYPE STANDARD TABLE OF rsparams.

    FIELD-SYMBOLS: <value> TYPE any.

    DATA(current_program) = COND #( WHEN i_program IS SUPPLIED THEN i_program
                                    ELSE cl_abap_syst=>get_current_program( ) ).

    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = current_program
      TABLES
        selection_table = selection_table
      EXCEPTIONS
        OTHERS          = 3.

    IF sy-subrc <> 0.
      lcx_error=>raise_syst( ).
    ENDIF.

    LOOP AT selection_table ASSIGNING FIELD-SYMBOL(<selection>).

      DATA(fname) = SWITCH string( <selection>-kind
                      WHEN 'S' THEN |({ current_program }){ <selection>-selname }[]|
                      WHEN 'P' THEN |({ current_program }){ <selection>-selname }|
                      ELSE THROW lcx_error( text = 'Invalid kind' ) ).

      ASSIGN (fname) TO <value>.
      ASSERT sy-subrc = 0.

      INSERT VALUE #( name = <selection>-selname
                      data = REF #( <value> ) )
             INTO TABLE mt_select_options.

    ENDLOOP.

  ENDMETHOD.

  METHOD get.

    READ TABLE mt_select_options ASSIGNING FIELD-SYMBOL(<select_option>)
                                 WITH TABLE KEY name = i_name.
    IF sy-subrc <> 0.
      lcx_error=>raise_text( |Invalid Select-Option or Parameter { i_name }!| ).
    ENDIF.

    ASSIGN <select_option>-data->* TO FIELD-SYMBOL(<value>).
    ASSERT sy-subrc = 0.

    IF et_select_option IS SUPPLIED.
      et_select_option = <value>.
      RETURN.
    ENDIF.

    IF e_parameter IS SUPPLIED.
      e_parameter = <value>.
      RETURN.
    ENDIF.

    lcx_error=>raise_text( |Invalid method call!| ).

  ENDMETHOD.

ENDCLASS.

CLASS model IMPLEMENTATION.

  METHOD lif_data_provider~get_data.

    DATA: ltr_sprsl TYPE RANGE OF t100-sprsl,
          ltr_arbgb TYPE RANGE OF t100-arbgb,
          ltr_msgnr TYPE RANGE OF t100-msgnr,
          ltr_text  TYPE RANGE OF t100-text,
          p_rows    TYPE i.

    io_select_options->get(
      EXPORTING i_name           = 'S_SPRSL'
      IMPORTING et_select_option = ltr_sprsl ).

    io_select_options->get(
      EXPORTING i_name           = 'S_ARBGB'
      IMPORTING et_select_option = ltr_arbgb ).

    io_select_options->get(
      EXPORTING i_name           = 'S_MSGNR'
      IMPORTING et_select_option = ltr_msgnr ).

    io_select_options->get(
      EXPORTING i_name           = 'S_TEXT'
      IMPORTING et_select_option = ltr_text ).

    io_select_options->get(
      EXPORTING i_name           = 'P_ROWS'
      IMPORTING e_parameter      = p_rows ).

    SELECT FROM t100
           FIELDS *
           WHERE sprsl IN @ltr_sprsl
           AND   arbgb IN @ltr_arbgb
           AND   msgnr IN @ltr_msgnr
           AND   text  IN @ltr_text
           INTO TABLE @et_data
           UP TO @p_rows ROWS.

  ENDMETHOD.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD constructor.

    mo_select_options = NEW select_options( ).
    mo_model = model_factory=>get_model_for( `T101` ).

  ENDMETHOD.

  METHOD get.

    IF _instance IS NOT BOUND.
      _instance = NEW controller( ).
    ENDIF.

    r_instance = _instance.

  ENDMETHOD.

  METHOD start.

    mo_model->get_data( EXPORTING io_select_options = mo_select_options
                        IMPORTING et_data = mt_t100 ).

    CALL SCREEN 0100.

  ENDMETHOD.

  METHOD pbo_0100.

    SET PF-STATUS 'STATUS_0100'.

    _pbo_0100_alv( ).

  ENDMETHOD.

  METHOD _pbo_0100_alv.

    CHECK m_alv IS NOT BOUND.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = cl_gui_container=>screen0
          IMPORTING
            r_salv_table   = m_alv
          CHANGING
            t_table        = mt_t100 ).

        m_alv->get_functions( )->add_function( name     = |TEST|
                                               icon     = |ICON_LED_GREEN|
                                               text     = |Test|
                                               tooltip  = |Test|
                                               position = if_salv_c_function_position=>right_of_salv_functions ).

        DATA(event) = m_alv->get_event( ).

        SET HANDLER _on_added_function FOR event.

        m_alv->display( ).

      CATCH cx_salv_error INTO DATA(error).    "
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD pai_0100.

    DATA(save_ok_code) = ok_code.

    CLEAR ok_code.

    _dispatch_fcode( save_ok_code ).

  ENDMETHOD.

  METHOD _on_added_function.

    _dispatch_fcode( e_salv_function ).

  ENDMETHOD.

  METHOD _dispatch_fcode.

    CASE i_fcode.
      WHEN 'BACK'
      OR   'EXIT'
      OR   'CANC'.

        SET SCREEN 0.

      WHEN OTHERS.

        MESSAGE |{ i_fcode }| TYPE 'I'.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  TRY.
      controller=>get( )->start( ).
    CATCH lcx_error INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

MODULE pbo_0100 OUTPUT.

  TRY.
      controller=>get( )->pbo_0100( ).
    CATCH lcx_error INTO error.
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDMODULE.

MODULE pai_0100 INPUT.

  TRY.
      controller=>get( )->pai_0100( ).
    CATCH lcx_error INTO error.
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDMODULE.
