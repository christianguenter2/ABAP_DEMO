REPORT z_test_channel_sender.

PARAMETER: message TYPE string OBLIGATORY DEFAULT 'Test'.

TRY.
    CAST if_amc_message_producer_text(
        cl_amc_channel_manager=>create_message_producer(
            i_application_id       = 'Z_TEST'
            i_channel_id           = '/test' )
        )->send( i_message = message ) .
  CATCH cx_amc_error INTO DATA(text_exc).
    cl_demo_output=>display( text_exc->get_text( ) ).
ENDTRY.
