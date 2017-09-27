*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_03_02.

PARAMETERS: t100 RADIOBUTTON GROUP r1 DEFAULT 'X',
            t000 RADIOBUTTON GROUP r1.

CLASS lcx_error DEFINITION CREATE PUBLIC
                INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    CLASS-METHODS:
      raise
        IMPORTING
          previous TYPE REF TO cx_root OPTIONAL
        RAISING
          lcx_error.

    METHODS:
      constructor
        IMPORTING
          textid   LIKE textid OPTIONAL
          previous LIKE previous OPTIONAL
          text     TYPE csequence OPTIONAL,

      _,

      get_text REDEFINITION.

  PRIVATE SECTION.
    DATA:
      m_text TYPE string.

ENDCLASS.

INTERFACE lif_controller.

  METHODS:
    start
      RAISING
        lcx_error.

ENDINTERFACE.

INTERFACE lif_model.

  METHODS:
    get_data
      EXPORTING
        er_data TYPE REF TO data.

ENDINTERFACE.

INTERFACE lif_view.

  METHODS:
    display
      IMPORTING
        it_data TYPE REF TO data
      RAISING
        lcx_error,

    pbo_0100
      RAISING
        lcx_error,

    pai_0100.

  EVENTS:
    usercommand
      EXPORTING
        VALUE(e_ucomm) TYPE sy-ucomm.

ENDINTERFACE.

CLASS controller DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES: lif_controller.
    METHODS:
      constructor
        RAISING
          lcx_error.

  PRIVATE SECTION.
    DATA: mo_model TYPE REF TO lif_model,
          mo_view  TYPE REF TO lif_view,
          mr_data  TYPE REF TO data.

    METHODS:
      handle_user_command FOR EVENT usercommand OF lif_view
        IMPORTING
            e_ucomm.

ENDCLASS.

CLASS t100_model DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: lif_model.

  PRIVATE SECTION.
    DATA: mt_t100 TYPE STANDARD TABLE OF t100
                       WITH NON-UNIQUE DEFAULT KEY.

ENDCLASS.

CLASS t000_model DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: lif_model.

  PRIVATE SECTION.
    DATA: mt_t000 TYPE STANDARD TABLE OF t000
                       WITH NON-UNIQUE DEFAULT KEY.

ENDCLASS.

CLASS lcx_error IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid = textid previous = previous ).

    m_text = text.

  ENDMETHOD.

  METHOD get_text.

    IF m_text IS NOT INITIAL.

      result = m_text.
      RETURN.

    ENDIF.

    result = super->get_text( ).

  ENDMETHOD.

  METHOD raise.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        previous = previous.

  ENDMETHOD.

  METHOD _.

  ENDMETHOD.

ENDCLASS.

CLASS view DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: lif_view.
    CLASS-DATA:
      okcode TYPE sy-ucomm.

  PRIVATE SECTION.
    DATA: mr_data TYPE REF TO data.

    METHODS:
      _added_function_handler FOR EVENT added_function OF cl_salv_events_table
        IMPORTING e_salv_function.

ENDCLASS.

CLASS factory DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        IMPORTING
          i_type            TYPE csequence
        RETURNING
          VALUE(r_instance) TYPE REF TO object
        RAISING
          lcx_error.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_instance,
             type     TYPE string,
             instance TYPE REF TO object,
           END OF ty_instance,
           tty_instance TYPE HASHED TABLE OF ty_instance
                             WITH UNIQUE KEY type.

    CLASS-DATA: mt_instance TYPE tty_instance.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD constructor.

    mo_model ?= factory=>get_instance( `MODEL` ).
    mo_view  ?= factory=>get_instance( `VIEW` ).

    SET HANDLER handle_user_command FOR mo_view.

  ENDMETHOD.

  METHOD lif_controller~start.

    mo_model->get_data(
      IMPORTING
        er_data = mr_data ).

    mo_view->display( mr_data ).

  ENDMETHOD.

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'EXIT'
      OR   'BACK'
      OR   'CANC'.

        SET SCREEN 0.

      WHEN OTHERS.

        MESSAGE |{ e_ucomm }| TYPE 'I'.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS t100_model IMPLEMENTATION.

  METHOD lif_model~get_data.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @mt_t100
           UP TO 100 ROWS.

    er_data = REF #( mt_t100 ).

  ENDMETHOD.

ENDCLASS.

CLASS t000_model IMPLEMENTATION.

  METHOD lif_model~get_data.

    SELECT FROM t000
           FIELDS *
           INTO TABLE @mt_t000.

    er_data = REF #( mt_t000 ).

  ENDMETHOD.

ENDCLASS.

CLASS view IMPLEMENTATION.

  METHOD lif_view~display.

    mr_data = it_data.

    CALL SCREEN 0100.

  ENDMETHOD.

  METHOD lif_view~pbo_0100.

    SET PF-STATUS 'STATUS_0100'.

    ASSIGN mr_data->* TO FIELD-SYMBOL(<table>).
    ASSERT sy-subrc = 0.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container  = cl_gui_container=>screen0
          IMPORTING
            r_salv_table = DATA(alv)
          CHANGING
            t_table      = <table> ).

        alv->get_functions( )->add_function( name     = |TEST|
                                             icon     = |{ icon_led_yellow }|
                                             text     = |Test|
                                             tooltip  = |Test|
                                             position = if_salv_c_function_position=>left_of_salv_functions ).

        DATA(event) = alv->get_event( ).

        SET HANDLER _added_function_handler FOR event.

        alv->display( ).

      CATCH cx_salv_error INTO DATA(error).    "
        lcx_error=>raise( previous = error ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_view~pai_0100.

    DATA(save_ok_code) = okcode.

    CLEAR okcode.

    RAISE EVENT lif_view~usercommand
      EXPORTING
        e_ucomm = save_ok_code.

  ENDMETHOD.

  METHOD _added_function_handler.

    RAISE EVENT lif_view~usercommand
      EXPORTING
        e_ucomm = e_salv_function.

  ENDMETHOD.

ENDCLASS.

CLASS factory IMPLEMENTATION.

  METHOD get_instance.

    r_instance = COND #( WHEN line_exists( mt_instance[ type = i_type ] )
                           THEN mt_instance[ type = i_type ]-instance
                         ELSE
                           SWITCH #( i_type
                             WHEN `MODEL`
                               THEN COND #( WHEN t100 = abap_true THEN NEW t100_model( )
                                            WHEN t000 = abap_true THEN NEW t000_model( )
                                            ELSE THROW lcx_error( text = |Cannot create model| ) )
                             WHEN `VIEW`
                               THEN NEW view( )
                             WHEN `CONTROLLER`
                               THEN NEW controller( )
                             ELSE
                               THROW lcx_error( text = |Cannot create instance of { i_type }!| ) ) ).

    INSERT VALUE #( type     = i_type
                    instance = r_instance )
           INTO TABLE mt_instance.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  CONSTANTS: BEGIN OF __,
               controller TYPE string VALUE `CONTROLLER`,
             END OF __.

  TRY.
      CAST lif_controller( factory=>get_instance( __-controller ) )->start( ).

    CATCH lcx_error INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

MODULE pbo_0100 OUTPUT.
  CAST lif_view( factory=>get_instance( `VIEW` ) )->pbo_0100( ).
ENDMODULE.

MODULE pai_0100 INPUT.
  CAST lif_view( factory=>get_instance( `VIEW` ) )->pai_0100( ).
ENDMODULE.
