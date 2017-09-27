REPORT z_test_channel_consumer.

DATA: alv TYPE REF TO cl_salv_table.

CLASS message_receiver DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      constructor,
      handle_added_function FOR EVENT added_function OF cl_salv_events_table
        IMPORTING e_salv_function.

    TYPES: BEGIN OF ty_data,
             text TYPE string,
           END OF ty_data,
           tty_data TYPE STANDARD TABLE OF ty_data
                    WITH NON-UNIQUE DEFAULT KEY.

    INTERFACES: if_amc_message_receiver_text.
    DATA: text_message TYPE string,
          message_list TYPE tty_data.

  PRIVATE SECTION.
    DATA: message_received TYPE abap_bool.
ENDCLASS.

CLASS message_receiver IMPLEMENTATION.
  METHOD constructor.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = alv
      CHANGING
        t_table      = message_list ).
  ENDMETHOD.

  METHOD if_amc_message_receiver_text~receive.
    message_received = abap_true.
    INSERT VALUE #( text = i_message ) INTO TABLE message_list.
  ENDMETHOD.

  METHOD handle_added_function.
    CASE e_salv_function.
      WHEN 'E' OR 'ENDE' OR 'ECAN'.
        LEAVE TO SCREEN 0.
      WHEN '&START'.
        DO 10 TIMES.
          message_received = abap_false.
          WAIT FOR MESSAGING CHANNELS
               ASYNCHRONOUS TASKS
               UNTIL message_received = abap_true.
          alv->refresh( ).
          cl_gui_cfw=>flush( ).
        ENDDO.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(receiver) = NEW message_receiver( ).

  TRY.
      cl_amc_channel_manager=>create_message_consumer(
        EXPORTING
          i_application_id       = 'Z_TEST'
          i_channel_id           = '/test'
        )->start_message_delivery( receiver ).
    CATCH cx_amc_error INTO DATA(exc).
      cl_demo_output=>display( exc->get_text( ) ).
  ENDTRY.


  DATA(events) = alv->get_event( ).

  SET HANDLER receiver->handle_added_function FOR events.

  alv->set_screen_status(
    EXPORTING
      report        = cl_abap_syst=>get_current_program( )
      pfstatus      = 'DEFAULT' ).
  alv->display( ).
