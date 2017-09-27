*&---------------------------------------------------------------------*
*& Report z_test_fm_error_message
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_fm_error_message.

CLASS test_error_message DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS: start.

  PRIVATE SECTION.

    METHODS _do_something.

ENDCLASS.

CLASS test_error_message IMPLEMENTATION.

  METHOD start.

    sy-subrc = 8.

    CALL FUNCTION 'Z_TEST_ERROR_MESSAGE'
      EXCEPTIONS
        error_message = 1.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE sy-msgty.
    ENDIF.

  ENDMETHOD.


  METHOD _do_something.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_error_message( )->start( ).
