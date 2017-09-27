*&---------------------------------------------------------------------*
*& Report z_test_2017_09_25
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_09_25.

PARAMETERS: alv   TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X',
            demo  TYPE abap_bool RADIOBUTTON GROUP r1,
            write TYPE abap_bool RADIOBUTTON GROUP r1,
            fiori TYPE abap_bool RADIOBUTTON GROUP r1.

INTERFACE lif_view DEFERRED.

CLASS lcx_error DEFINITION CREATE PUBLIC
                INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          textid   LIKE textid    OPTIONAL
          previous LIKE previous  OPTIONAL
          text     TYPE csequence OPTIONAL
          msg      TYPE symsg     OPTIONAL,

      get_text REDEFINITION.

    CLASS-METHODS:
      raise_invalid_view
        RETURNING
          VALUE(ro_view) TYPE REF TO lif_view
        RAISING
          lcx_error,

      raise_text
        IMPORTING
          i_text TYPE csequence
        RAISING
          lcx_error,

      raise_syst
        RAISING
          lcx_error.

  PRIVATE SECTION.
    DATA: m_text TYPE string,
          m_msg  TYPE symsg.

    METHODS:
      _get_message_text
        RETURNING
          VALUE(r_text) TYPE string.

ENDCLASS.

INTERFACE lif_view.

  METHODS:
    display
      IMPORTING
        it_data TYPE STANDARD TABLE
      RAISING
        lcx_error.

ENDINTERFACE.

CLASS lcl_alv DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS lcl_demo DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS lcl_write DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS lcl_view_factory DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      get_view
        RETURNING
          VALUE(ro_view) TYPE REF TO lif_view
        RAISING
          lcx_error.

ENDCLASS.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          io_view TYPE REF TO lif_view,

      run
        RAISING
          lcx_error.

  PRIVATE SECTION.
    DATA:
      mt_t100 TYPE STANDARD TABLE OF t100,
      mo_view TYPE REF TO lif_view.

    METHODS:
      _select,
      _process,
      _display
        RAISING
          lcx_error.

ENDCLASS.

CLASS lcl_radio_button DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS:
      get_marked_radio_button
        IMPORTING
          i_id                         TYPE string
          i_report                     TYPE rsvar-report
        RETURNING
          VALUE(r_marked_radio_button) TYPE string
        RAISING
          lcx_error.

  PRIVATE SECTION.

    TYPES: tty_sel_table TYPE STANDARD TABLE OF rsparams
                         WITH NON-UNIQUE DEFAULT KEY,
           tty_selinfo   TYPE STANDARD TABLE OF selinfo
                         WITH NON-UNIQUE DEFAULT KEY.

    CLASS-METHODS:
      _filter_by_id
        IMPORTING
          i_id                            TYPE string
          it_selections_info              TYPE tty_selinfo
        RETURNING
          VALUE(rt_selections_info_range) TYPE rseloption
        RAISING
          lcx_error,

      _filter_by_fields_and_selected
        IMPORTING
          it_fields                 TYPE rseloption
          it_selection_table        TYPE lcl_radio_button=>tty_sel_table
        RETURNING
          VALUE(rt_selection_table) TYPE stringtab
        RAISING
          lcx_error.

ENDCLASS.

CLASS lcx_error IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid = textid previous = previous ).
    m_text = text.
    m_msg  = msg.

  ENDMETHOD.

  METHOD get_text.

    result = COND #( WHEN m_text IS NOT INITIAL THEN m_text
                     WHEN m_msg  IS NOT INITIAL THEN _get_message_text( )
                     ELSE super->get_text( ) ).

  ENDMETHOD.

  METHOD raise_invalid_view.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        text = |Invalid view!|.

  ENDMETHOD.

  METHOD raise_text.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        text = i_text.

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

  METHOD _get_message_text.

    MESSAGE ID m_msg-msgid TYPE m_msg-msgty NUMBER m_msg-msgno
            WITH m_msg-msgv1 m_msg-msgv2 m_msg-msgv3 m_msg-msgv4
            INTO r_text.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_alv IMPLEMENTATION.

  METHOD lif_view~display.

    DATA: lr_data TYPE REF TO data.

    CREATE DATA lr_data LIKE it_data.
    ASSIGN lr_data->* TO FIELD-SYMBOL(<table>).
    <table> = it_data.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(alv)
          CHANGING
            t_table      = <table> ).

        alv->display( ).

      CATCH cx_salv_msg INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.

  METHOD lif_view~display.

    cl_demo_output=>display( it_data ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_write IMPLEMENTATION.

  METHOD lif_view~display.

    lcx_error=>raise_text( |View 'Write' not yet implemented| ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_view_factory IMPLEMENTATION.

  METHOD get_view.

    DATA(classname) = |LCL_| && lcl_radio_button=>get_marked_radio_button( i_id     = `R1`
                                                                           i_report = cl_abap_syst=>get_current_program( ) ).

    TRY.
        CREATE OBJECT ro_view TYPE (classname).

      CATCH cx_sy_create_object_error INTO DATA(error).
        lcx_error=>raise_text( error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD constructor.

    mo_view = io_view.

  ENDMETHOD.

  METHOD run.

    _select( ).
    _process( ).
    _display( ).

  ENDMETHOD.

  METHOD _select.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @mt_t100
           UP TO 100 ROWS.

  ENDMETHOD.

  METHOD _process.

    LOOP AT mt_t100 ASSIGNING FIELD-SYMBOL(<t100>).

    ENDLOOP.

  ENDMETHOD.

  METHOD _display.

    mo_view->display( mt_t100 ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_radio_button IMPLEMENTATION.

  METHOD get_marked_radio_button.

    DATA: selection_table TYPE tty_sel_table,
          selections_info TYPE tty_selinfo.

    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = i_report
      TABLES
        selection_table = selection_table
      EXCEPTIONS
        not_found       = 1
        no_report       = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      lcx_error=>raise_syst( ).
    ENDIF.

    CALL FUNCTION 'RS_SELECTIONS_DESCRIPTION'
      EXPORTING
        report              = i_report
      TABLES
        selections_info     = selections_info
      EXCEPTIONS
        no_selections       = 1
        report_not_existent = 2
        subroutine_pool     = 3
        OTHERS              = 4.

    IF sy-subrc <> 0.
      lcx_error=>raise_syst( ).
    ENDIF.

    DATA(radio_button_fields_range) = _filter_by_id( i_id               = i_id
                                                     it_selections_info = selections_info ).

    DATA(lt_marked_radio_buttons) = _filter_by_fields_and_selected( it_fields          = radio_button_fields_range
                                                                    it_selection_table = selection_table ).

    r_marked_radio_button = lt_marked_radio_buttons[ 1 ].

  ENDMETHOD.

  METHOD _filter_by_id.

    rt_selections_info_range = VALUE rseloption( FOR select_info IN it_selections_info
                                                 WHERE ( id = i_id )
                                                 ( sign   = 'I'
                                                   option = 'EQ'
                                                   low    = select_info-name ) ).

    IF lines( rt_selections_info_range ) = 0.
      lcx_error=>raise_text( |No fields for radiobutton id { i_id } found!| ).
    ENDIF.

  ENDMETHOD.

  METHOD _filter_by_fields_and_selected.

    rt_selection_table = VALUE stringtab( FOR selection IN it_selection_table
                                                        WHERE ( low     = abap_true
                                                        AND     selname IN it_fields  )
                                                        ( CONV #( selection-selname ) ) ).

    IF lines(  rt_selection_table ) = 0.
      lcx_error=>raise_text( |Radiobutton isn't selected!| ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  TRY.
      NEW controller( NEW lcl_view_factory( )->get_view( ) )->run( ).
    CATCH lcx_error INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
