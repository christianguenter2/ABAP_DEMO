CLASS zcl_apc_wsp_ext_zapc_test DEFINITION
  PUBLIC
  INHERITING FROM cl_apc_wsp_ext_stateless_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: if_apc_wsp_extension~on_message REDEFINITION,
      if_apc_wsp_extension~on_start REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_apc_wsp_ext_zapc_test IMPLEMENTATION.


  METHOD if_apc_wsp_extension~on_message.

    TRY.
        DATA(text)		= i_message->get_text( ).
        DATA(message) = i_message_manager->create_message( ).

        IF contains( val   = text
                     regex = '^/' ).

          i_context->get_binding_manager(
                  )->bind_amc_message_consumer( i_application_id = 'ZAMC_TEST'
                                                i_channel_id     = text ).

        ENDIF.

      CATCH cx_apc_error.

    ENDTRY.

  ENDMETHOD.


  METHOD if_apc_wsp_extension~on_start.

  ENDMETHOD.

ENDCLASS.

