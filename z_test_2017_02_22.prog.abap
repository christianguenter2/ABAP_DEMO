*&---------------------------------------------------------------------*
*& Report z_test_2017_02_22
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_02_22.

TABLES: t100.
DATA: okcode TYPE sy-ucomm.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: s_sprsl FOR t100-sprsl,
                s_arbgb FOR t100-arbgb,
                s_msgnr FOR t100-msgnr,
                s_text  FOR t100-text.
PARAMETERS: p_test TYPE abap_bool AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

CLASS select_options DEFINITION DEFERRED.

CLASS lcx_error DEFINITION CREATE PUBLIC
                INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    CLASS-METHODS:
      raise_syst_error
        RAISING lcx_error,

      raise_text
        IMPORTING
          i_text TYPE csequence
        RAISING
          lcx_error.

    METHODS:
      constructor
        IMPORTING
          i_msg  TYPE symsg OPTIONAL
          i_text TYPE csequence OPTIONAL,

      get_text REDEFINITION.

  PRIVATE SECTION.
    DATA: m_msg  TYPE symsg,
          m_text TYPE string.

ENDCLASS.

INTERFACE lif_data_provider.
  METHODS:
    get_data
      EXPORTING
        VALUE(et_data) TYPE ANY TABLE
      RAISING
        lcx_error,

    set_select_options
      IMPORTING
        io_select_options  TYPE REF TO select_options
      RETURNING
        VALUE(ro_instance) TYPE REF TO lif_data_provider.

ENDINTERFACE.

CLASS select_options DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_select_option,
             name  TYPE string,
             text  TYPE string,
             kind  TYPE rsscr_kind,
             value TYPE REF TO data,
           END OF ty_select_option,
           tty_select_option TYPE SORTED TABLE OF ty_select_option
                             WITH UNIQUE KEY name.

    DATA: mt_select_options TYPE tty_select_option READ-ONLY.
    METHODS:
      constructor
        IMPORTING
          i_current_program TYPE sycprog OPTIONAL
        RAISING
          lcx_error,

      get_sel_opt
        IMPORTING
          i_name          TYPE csequence
        EXPORTING
          e_select_option TYPE ANY TABLE
        RAISING
          lcx_error,

      get_parameter
        IMPORTING
          i_name          TYPE csequence
        EXPORTING
          e_select_option TYPE any
        RAISING
          lcx_error.


ENDCLASS.

CLASS dd_select_options DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          io_select_options TYPE REF TO select_options
          io_container      TYPE REF TO cl_gui_container,

      render
        RAISING
          lcx_error.

  PRIVATE SECTION.
    DATA:
      mo_select_options TYPE REF TO select_options,
      mo_container      TYPE REF TO cl_gui_container.

ENDCLASS.

CLASS t100_data_provider DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: lif_data_provider.

  PRIVATE SECTION.
    DATA mo_select_options TYPE REF TO select_options.

ENDCLASS.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS get
      RETURNING VALUE(r_instance) TYPE REF TO controller
      RAISING   lcx_error.

    METHODS:
      constructor
        IMPORTING
                  io_model TYPE REF TO lif_data_provider
        RAISING   lcx_error,
      start
        RAISING
          lcx_error,
      pai_0100
        RAISING
          lcx_error,
      pbo_0100
        RAISING
          lcx_error.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF co_fcode,
        back   TYPE sy-ucomm VALUE `BACK`,
        exit   TYPE sy-ucomm VALUE `EXIT`,
        cancel TYPE sy-ucomm VALUE `CANC`,
      END OF co_fcode.

    CLASS-DATA:
      _instance TYPE REF TO controller.

    DATA:
      t100_tab          TYPE STANDARD TABLE OF t100,
      m_alv             TYPE REF TO cl_salv_table,
      mo_docking_top    TYPE REF TO cl_gui_docking_container,
      mo_model          TYPE REF TO lif_data_provider,
      m_docking_visible TYPE abap_bool,
      mo_select_options TYPE REF TO select_options.

    METHODS:
      _get_data
        RAISING
          lcx_error,
      _display,
      _on_added_function FOR EVENT added_function OF if_salv_events_functions
        IMPORTING e_salv_function,
      _dispatch_fcode
        IMPORTING
          i_fcode TYPE salv_de_function
        RAISING
          lcx_error,
      _pbo_set,
      _pbo_alv
        RAISING
          lcx_error,
      _pbo_docking
        RAISING
          lcx_error,
      _toggle_docking_container.

ENDCLASS.

CLASS lcx_error IMPLEMENTATION.

  METHOD constructor.

    super->constructor(
      EXPORTING
        textid   = textid
        previous = previous ).

    m_msg  = i_msg.
    m_text = i_text.

  ENDMETHOD.

  METHOD raise_syst_error.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        i_msg  = VALUE #( msgty = sy-msgty
                          msgid = sy-msgid
                          msgno = sy-msgno
                          msgv1 = sy-msgv1
                          msgv2 = sy-msgv2
                          msgv3 = sy-msgv3
                          msgv4 = sy-msgv4 ).

  ENDMETHOD.

  METHOD raise_text.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        i_text = i_text.

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

ENDCLASS.

CLASS select_options IMPLEMENTATION.

  METHOD constructor.

    DATA: selections_info TYPE STANDARD TABLE OF selinfo,
          fname           TYPE string.

    FIELD-SYMBOLS: <data> TYPE data.

    DATA(current_program) = COND #( WHEN i_current_program IS SUPPLIED THEN i_current_program
                                    ELSE cl_abap_syst=>get_current_program( ) ).

    CALL FUNCTION 'RS_SELECTIONS_DESCRIPTION'
      EXPORTING
        report          = current_program
      TABLES
        selections_info = selections_info    " Infos über Typ, Bezugsfeld etc
      EXCEPTIONS
        OTHERS          = 4.

    IF sy-subrc <> 0.
      lcx_error=>raise_syst_error( ).
    ENDIF.

    LOOP AT selections_info ASSIGNING FIELD-SYMBOL(<selection_info>).

      CASE <selection_info>-kind.
        WHEN 'S'.

          fname = |({ current_program }){ <selection_info>-name }[]|.
          ASSIGN (fname) TO <data>.
          ASSERT sy-subrc = 0.

        WHEN 'P'.

          fname = |({ current_program }){ <selection_info>-name }|.
          ASSIGN (fname) TO <data>.
          ASSERT sy-subrc = 0.

      ENDCASE.

      INSERT VALUE #( name  = <selection_info>-name
                      kind  = <selection_info>-kind
                      text  = <selection_info>-name
                      value = REF #( <data> ) )
              INTO TABLE mt_select_options.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_sel_opt.

    FIELD-SYMBOLS: <value> TYPE ANY TABLE.

    DATA(select_option) = VALUE #( mt_select_options[ name = i_name ] OPTIONAL ).

    IF select_option IS INITIAL.
      lcx_error=>raise_text( |Select-Option { i_name } not found!| ).
    ENDIF.

    ASSIGN select_option-value->* TO <value>.
    ASSERT sy-subrc = 0.

    e_select_option = <value>.

  ENDMETHOD.

  METHOD get_parameter.

    FIELD-SYMBOLS: <value> TYPE any.

    DATA(parameter) = VALUE #( mt_select_options[ name = i_name ] OPTIONAL ).

    IF parameter IS INITIAL.
      lcx_error=>raise_text( |Parameter { i_name } not found!| ).
    ENDIF.

    ASSIGN parameter-value->* TO <value>.
    ASSERT sy-subrc = 0.

    e_select_option = <value>.

  ENDMETHOD.

ENDCLASS.


CLASS dd_select_options IMPLEMENTATION.

  METHOD constructor.

    mo_select_options = io_select_options.
    mo_container      = io_container.

  ENDMETHOD.

  METHOD render.

    FIELD-SYMBOLS: <table> TYPE ANY TABLE.

    DATA(dd) = NEW cl_dd_document(  ).

    dd->add_text_as_heading(
      EXPORTING
        text          = |Parameter| ).

    dd->add_table(
      EXPORTING
        no_of_columns               = 2
      IMPORTING
        table                       = DATA(table)    " Tabellenelement
        tablearea                   = DATA(tablearea)    " Tabellenbereich
      EXCEPTIONS
        table_already_used          = 1
        OTHERS                      = 2 ).

    IF sy-subrc <> 0.
      lcx_error=>raise_syst_error( ).
    ENDIF.

    LOOP AT mo_select_options->mt_select_options ASSIGNING FIELD-SYMBOL(<parameter>)
                                                 WHERE kind = 'P'.

      ASSIGN <parameter>-value->* TO FIELD-SYMBOL(<value>).
      CHECK sy-subrc = 0.

      tablearea->add_text( text = |{ <parameter>-text }| ).
      tablearea->add_text( text = |{ <value> }| ).

      tablearea->new_row( ).

    ENDLOOP.

    dd->add_text_as_heading(
      EXPORTING
        text          = |Select-Options| ).

    dd->add_table(
      EXPORTING
        no_of_columns               = 5
      IMPORTING
        table                       = table    " Tabellenelement
        tablearea                   = tablearea    " Tabellenbereich
      EXCEPTIONS
        table_already_used          = 1
        OTHERS                      = 2 ).

    IF sy-subrc <> 0.
      lcx_error=>raise_syst_error( ).
    ENDIF.

    LOOP AT mo_select_options->mt_select_options ASSIGNING FIELD-SYMBOL(<select_options>)
                                                 WHERE kind = 'S'.

      ASSIGN <select_options>-value->* TO <table>.
      CHECK sy-subrc = 0.

      LOOP AT <table> ASSIGNING FIELD-SYMBOL(<line>).

        tablearea->add_text( text = |{ <select_options>-text }| ).

        ASSIGN COMPONENT 'SIGN' OF STRUCTURE <line> TO FIELD-SYMBOL(<sign>).
        tablearea->add_text( text = |{ <sign> }| ).

        ASSIGN COMPONENT 'OPTION' OF STRUCTURE <line> TO FIELD-SYMBOL(<option>).
        tablearea->add_text( text = |{ <option> }| ).

        ASSIGN COMPONENT 'LOW' OF STRUCTURE <line> TO FIELD-SYMBOL(<low>).
        tablearea->add_text( text = |{ <low> }| ).

        ASSIGN COMPONENT 'HIGH' OF STRUCTURE <line> TO FIELD-SYMBOL(<high>).
        tablearea->add_text( text = |{ <high> }| ).

        tablearea->new_row( ).

      ENDLOOP.

    ENDLOOP.

    dd->merge_document( ).

    dd->display_document(
      EXPORTING
        parent             = mo_container
      EXCEPTIONS
        html_display_error = 1
        OTHERS             = 2 ).

    IF sy-subrc <> 0.
      lcx_error=>raise_syst_error( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS t100_data_provider IMPLEMENTATION.

  METHOD lif_data_provider~set_select_options.

    mo_select_options = io_select_options.

    ro_instance = me.

  ENDMETHOD.

  METHOD lif_data_provider~get_data.

    DATA: ltr_sprsl TYPE RANGE OF t100-sprsl,
          ltr_arbgb TYPE RANGE OF t100-arbgb,
          ltr_msgnr TYPE RANGE OF t100-msgnr,
          ltr_text  TYPE RANGE OF t100-text,
          test      TYPE abap_bool.

    mo_select_options->get_sel_opt(
      EXPORTING i_name          = 'S_SPRSL'
      IMPORTING e_select_option = ltr_sprsl ).

    mo_select_options->get_sel_opt(
      EXPORTING i_name          = 'S_ARBGB'
      IMPORTING e_select_option = ltr_arbgb ).

    mo_select_options->get_sel_opt(
      EXPORTING i_name          = 'S_MSGNR'
      IMPORTING e_select_option = ltr_msgnr ).

    mo_select_options->get_sel_opt(
      EXPORTING i_name          = 'S_TEXT'
      IMPORTING e_select_option = ltr_text ).

    mo_select_options->get_parameter(
      EXPORTING i_name          = |P_TEST|
      IMPORTING e_select_option = test ).

    SELECT FROM t100
           FIELDS *
           WHERE sprsl IN @ltr_sprsl
           AND   arbgb IN @ltr_arbgb
           AND   msgnr IN @ltr_msgnr
           AND   text  IN @ltr_text
           INTO CORRESPONDING FIELDS OF TABLE @et_data
           UP TO 100 ROWS.

    IF sy-subrc <> 0.
      lcx_error=>raise_text( `Keine Daten gefunden` ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD constructor.

    mo_select_options = NEW select_options( ).
    mo_model = io_model->set_select_options( NEW select_options( ) ).

  ENDMETHOD.

  METHOD get.

    IF _instance IS NOT BOUND.

      _instance = NEW controller( NEW t100_data_provider( ) ).

    ENDIF.

    r_instance = _instance.

  ENDMETHOD.

  METHOD start.

    _get_data( ).
    _display( ).

  ENDMETHOD.

  METHOD _display.

    CALL SCREEN 0100.

  ENDMETHOD.

  METHOD _get_data.

    mo_model->get_data(
      IMPORTING
        et_data = t100_tab ).

  ENDMETHOD.

  METHOD pbo_0100.

    _pbo_set( ).
    _pbo_alv( ).
    _pbo_docking( ).

  ENDMETHOD.

  METHOD _pbo_docking.

    CHECK mo_docking_top IS NOT BOUND.

    mo_docking_top = NEW cl_gui_docking_container(
                           parent    = cl_gui_container=>screen0
                           side      = cl_gui_docking_container=>dock_at_top
                           extension = 80 ).

    NEW dd_select_options( io_select_options = mo_select_options
                           io_container      = mo_docking_top )->render( ).

    m_docking_visible = abap_true.

  ENDMETHOD.

  METHOD _pbo_alv.

    CHECK m_alv IS NOT BOUND.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = cl_gui_container=>screen0
          IMPORTING
            r_salv_table   = m_alv
          CHANGING
            t_table        = t100_tab ).

        DATA(functions) = m_alv->get_functions( ).

        functions->set_all( abap_true ).

        functions->add_function(
          EXPORTING
            name               = |TEST|
            icon               = |{ icon_led_red }|
            text               = |Test|
            tooltip            = |Test|
            position           = if_salv_c_function_position=>right_of_salv_functions ).

        functions->add_function(
          EXPORTING
            name               = |TEST2|
            icon               = |{ icon_led_green }|
            text               = |Test|
            tooltip            = |Test|
            position           = if_salv_c_function_position=>right_of_salv_functions ).

        DATA(event) = m_alv->get_event( ).

        SET HANDLER _on_added_function FOR event.

        m_alv->display( ).

      CATCH cx_salv_error INTO DATA(error).    "
        lcx_error=>raise_text( error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD _pbo_set.

    SET PF-STATUS 'STATUS_0100'.
    SET TITLEBAR 'TITLE_0100'.

  ENDMETHOD.

  METHOD pai_0100.

    DATA(save_ok_code) = okcode.

    CLEAR okcode.

    _dispatch_fcode( save_ok_code ).

  ENDMETHOD.

  METHOD _on_added_function.

    TRY.
        _dispatch_fcode( e_salv_function ).
      CATCH lcx_error INTO DATA(error) .
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD _dispatch_fcode.

    CASE i_fcode.
      WHEN co_fcode-back
      OR   co_fcode-exit
      OR   co_fcode-cancel.

        SET SCREEN 0.

      WHEN `TEST`.

        _toggle_docking_container( ).

      WHEN OTHERS.

        MESSAGE e016(56) WITH |Funktionscode { i_fcode } nicht implementiert!|
                INTO DATA(dummy).
        lcx_error=>raise_syst_error( ).

    ENDCASE.

  ENDMETHOD.

  METHOD _toggle_docking_container.

    m_docking_visible = boolc( m_docking_visible = abap_false ).

    mo_docking_top->set_visible(
      EXPORTING
        visible           = m_docking_visible
      EXCEPTIONS
        OTHERS            = 3 ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  TRY.
      controller=>get( )->start( ).
    CATCH lcx_error INTO DATA(error) .
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  TRY.
      controller=>get( )->pbo_0100( ).
    CATCH lcx_error INTO error .
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  TRY.
      controller=>get( )->pai_0100( ).
    CATCH lcx_error INTO error .
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
ENDMODULE.
