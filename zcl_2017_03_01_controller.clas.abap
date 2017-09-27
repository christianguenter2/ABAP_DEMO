CLASS zcl_2017_03_01_controller DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-DATA:
      okcode TYPE syucomm .

    CLASS-METHODS:
      get_instance
        IMPORTING
          io_view           TYPE REF TO zif_2017_03_01_view OPTIONAL
          io_selections     TYPE REF TO zcl_2017_03_01_selections OPTIONAL
        RETURNING
          VALUE(r_instance) TYPE REF TO zcl_2017_03_01_controller
        RAISING
          zcx_2017_03_01.

    METHODS:
      constructor
        IMPORTING
          io_view       TYPE REF TO zif_2017_03_01_view
          io_selections TYPE REF TO zcl_2017_03_01_selections OPTIONAL
        RAISING
          zcx_2017_03_01,

      start
        RAISING
          zcx_2017_03_01,

      pai_0100,
      pbo_0100
        RAISING
          zcx_2017_03_01.

  PRIVATE SECTION.

    CLASS-DATA:
      _instance TYPE REF TO zcl_2017_03_01_controller .

    DATA:
      m_alv         TYPE REF TO cl_salv_table,
      t100_tab      TYPE STANDARD TABLE OF t100,
      mo_model      TYPE REF TO zcl_2017_03_01_model,
      mo_selections TYPE REF TO zcl_2017_03_01_selections,
      mo_view       TYPE REF TO zif_2017_03_01_view.

    METHODS:
      _select
        RAISING
          zcx_2017_03_01,

      _process,
      _display,

      _on_added_function FOR EVENT added_function OF cl_salv_events_table
        IMPORTING
            !e_salv_function,

      _dispatch_fcode
        IMPORTING
          !i_fcode TYPE syucomm.

ENDCLASS.



CLASS zcl_2017_03_01_controller IMPLEMENTATION.

  METHOD constructor.

    mo_view       = io_view.
    mo_selections = io_selections.
    mo_model      = NEW zcl_2017_03_01_model( ).

  ENDMETHOD.

  METHOD get_instance.

    IF _instance IS NOT BOUND.

      _instance = NEW zcl_2017_03_01_controller( io_view       = io_view
                                                 io_selections = io_selections ).

    ENDIF.

    r_instance = _instance.

  ENDMETHOD.

  METHOD pai_0100.

    DATA(save_ok_code) = okcode.

    CLEAR okcode.

    _dispatch_fcode( save_ok_code ).

  ENDMETHOD.


  METHOD pbo_0100.

    SET PF-STATUS 'STATUS_0100' OF PROGRAM mo_view->m_current_program.

    CHECK m_alv IS NOT BOUND.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = cl_gui_container=>screen0
          IMPORTING
            r_salv_table   = m_alv
          CHANGING
            t_table        = t100_tab ).

        m_alv->get_functions( )->add_function( name     = |TEST|
                                               icon     = |{ icon_led_green }|
                                               text     = |Test|
                                               tooltip  = |Test|
                                               position = if_salv_c_function_position=>right_of_salv_functions ).

        DATA(event) = m_alv->get_event( ).

        SET HANDLER _on_added_function FOR event.

        m_alv->display( ).

      CATCH cx_salv_error INTO DATA(error).
        zcx_2017_03_01=>raise_previous( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD start.

    _select( ).
    _process( ).
    _display( ).

  ENDMETHOD.


  METHOD _dispatch_fcode.

    CASE i_fcode.
      WHEN 'EXIT'
      OR   'BACK'
      OR   'CANC'.

        SET SCREEN 0.

      WHEN OTHERS.

        MESSAGE |{ i_fcode }| TYPE 'I'.

    ENDCASE.

  ENDMETHOD.


  METHOD _display.

    mo_view->display( ).

  ENDMETHOD.


  METHOD _on_added_function.

    _dispatch_fcode( e_salv_function ).

  ENDMETHOD.


  METHOD _process.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).

    ENDLOOP.

  ENDMETHOD.


  METHOD _select.

    mo_model->get_data(
      EXPORTING
        io_selections = mo_selections
      IMPORTING
        et_data       = t100_tab ).

  ENDMETHOD.
ENDCLASS.
