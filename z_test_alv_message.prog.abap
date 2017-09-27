REPORT z_test_alv_message.

CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_after_refresh FOR EVENT after_refresh OF cl_gui_alv_grid,
      on_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid,
      on_double_click FOR EVENT double_click OF cl_salv_events_table.

  PRIVATE SECTION.
    DATA: first_time TYPE abap_bool.

ENDCLASS.

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS: on_finished FOR EVENT finished OF cl_gui_timer
      IMPORTING sender.
ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.
  METHOD on_after_refresh.
    CHECK first_time = abap_false.

    first_time = abap_true.
*    MESSAGE 'after refresh' TYPE 'I'.

*    CALL FUNCTION 'POPUP_TO_INFORM'
*      EXPORTING
*        titel = 'Test'
*        txt1  = 'Test'
*        txt2  = 'Test'.
  ENDMETHOD.

  METHOD on_top_of_page.
    MESSAGE 'TOP_OF_PAGE' TYPE 'I'.
  ENDMETHOD.

  METHOD on_double_click.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Test'
        txt1  = 'Test'
        txt2  = 'Test'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD on_finished.
    MESSAGE 'Test' TYPE 'I'.

    sender->run( ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  SELECT * FROM t100
           INTO TABLE @DATA(lt_data)
           UP TO 20 ROWS.

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table   = DATA(alv)
    CHANGING
      t_table        = lt_data ).

  DATA(handler) = NEW lcl_handler( ).

  SET HANDLER handler->on_after_refresh FOR ALL INSTANCES.
  SET HANDLER handler->on_top_of_page FOR ALL INSTANCES.
  SET HANDLER handler->on_double_click FOR alv->get_event( ).

  alv->set_top_of_list( NEW cl_salv_form_text( text = 'Test' ) ).

  DATA(timer) = NEW cl_gui_timer( ).
  DATA(event_receiver) = NEW lcl_event_receiver( ).

  timer->interval = '1'.
  timer->run( ).
  SET HANDLER event_receiver->on_finished FOR timer.

  alv->display( ).
