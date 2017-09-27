*&---------------------------------------------------------------------*
*& Report z_test_send_websocket
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_send_websocket.

PARAMETERS: text TYPE string OBLIGATORY.

TRY.
    DATA(producer) = CAST if_amc_message_producer_text( cl_amc_channel_manager=>create_message_producer(
                                                             i_application_id       = 'ZAMC_TEST'
                                                             i_channel_id           = '/channel_1'
                                                         ) ).

    producer->send( text ).

  CATCH cx_amc_error INTO DATA(error).
    MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
ENDTRY.
