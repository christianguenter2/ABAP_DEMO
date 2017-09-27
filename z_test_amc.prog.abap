*&---------------------------------------------------------------------*
*& Report  Z_TEST_AMC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT z_test_amc.

DATA: okcode TYPE sy-ucomm.

CLASS lcl_receiver DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_amc_message_receiver_text.
ENDCLASS.

CLASS lcl_receiver IMPLEMENTATION.

  METHOD if_amc_message_receiver_text~receive.
    MESSAGE |receive handler| TYPE 'I'.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_test DEFINITION .
  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,
      start,
      stop,
      send.

  PRIVATE SECTION.
    CLASS-DATA:
      receiver  TYPE REF TO if_amc_message_receiver,
      ltr_lgpla TYPE rseloption,
      consumer  TYPE REF TO if_amc_message_consumer.
ENDCLASS.

CLASS lcl_test IMPLEMENTATION .

  METHOD class_constructor.

    receiver = NEW lcl_receiver( ).

    TRY.
        CALL METHOD (`\PROGRAM=CL_AMC_CHANNEL_MANAGER========CP\CLASS=LCL_SAPGUI_CHANNEL_MANAGER`)=>create_message_consumer
          EXPORTING
            i_application_id = 'Z_TEST'
            i_channel_id     = '/test'
          RECEIVING
            r_consumer       = consumer.
      CATCH cx_amc_error INTO DATA(error).
        MESSAGE error TYPE 'I'.
    ENDTRY.

  ENDMETHOD.

  METHOD start.


    TRY.

        consumer->start_message_delivery( receiver ).

      CATCH cx_amc_error INTO DATA(error).
        MESSAGE error TYPE 'I'.
    ENDTRY.

  ENDMETHOD.

  METHOD stop.

    TRY.
        consumer->stop_message_delivery( receiver ).

      CATCH cx_amc_error INTO DATA(error).
        MESSAGE error TYPE 'I'.
    ENDTRY.

  ENDMETHOD.

  METHOD send.

    DATA: lo_producer TYPE REF TO if_amc_message_producer_text.

    TRY.
        lo_producer ?= cl_amc_channel_manager=>create_message_producer(
                         i_application_id       = 'Z_TEST'
                         i_channel_id           = '/test' ).

        lo_producer->send( |Test| ).

      CATCH cx_amc_error INTO DATA(error).
        MESSAGE error TYPE 'I'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  lcl_test=>start( ).

  CALL SCREEN 0100.

  INCLUDE z_test_amc_user_command_010i01.
