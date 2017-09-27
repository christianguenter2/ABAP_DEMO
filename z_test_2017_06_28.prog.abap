*&---------------------------------------------------------------------*
*& Report z_test_2017_06_28
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_06_28.

CLASS lcx_file_error DEFINITION CREATE PUBLIC
                     INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    CLASS-METHODS:
      raise_file_open_error
        RAISING
          lcx_file_error,

      raise_file_close_error
        RAISING
          lcx_file_error,

      raise_data_read_not_handled
        RAISING
          lcx_file_error.

    METHODS:
      constructor
        IMPORTING
          textid   LIKE textid OPTIONAL
          previous LIKE previous OPTIONAL
          msg      TYPE symsg OPTIONAL,

      get_text REDEFINITION.

  PRIVATE SECTION.
    CLASS-METHODS:
      raise_syst
        RAISING
          lcx_file_error.

    DATA: m_msg TYPE symsg.

    METHODS:
      _get_msg_text
        RETURNING VALUE(r_text) TYPE string.

ENDCLASS.

CLASS lcl_file_api DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          i_file TYPE csequence,

      read
        IMPORTING
          i_chunk_size TYPE i DEFAULT 10
        RAISING
          lcx_file_error.

    EVENTS:
      data_read
        EXPORTING
          VALUE(er_processed) TYPE REF TO abap_bool
          VALUE(et_data) TYPE stringtab.

  PRIVATE SECTION.
    DATA:
      m_file TYPE string.

    METHODS:
      _open_dataset
        RAISING
          lcx_file_error,

      _close_dataset
        RAISING
          lcx_file_error,

      _raise_event_data_read
        IMPORTING
          it_data TYPE stringtab
        RAISING
          lcx_file_error.

ENDCLASS.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start
        RAISING
          lcx_file_error.

  PRIVATE SECTION.
    METHODS:
      handle_file_read FOR EVENT data_read OF lcl_file_api
        IMPORTING
            er_processed
            et_data.

ENDCLASS.

CLASS lcx_file_error IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid = textid previous = previous ).

    m_msg = msg.

  ENDMETHOD.

  METHOD raise_file_open_error.

    MESSAGE e160(sr) WITH 'File open error'
                     INTO DATA(dummy).
    raise_syst( ).

  ENDMETHOD.

  METHOD raise_file_close_error.

    MESSAGE e160(sr) WITH 'File close error'
                     INTO DATA(dummy).
    raise_syst( ).

  ENDMETHOD.

  METHOD raise_data_read_not_handled.

    MESSAGE e160(sr) WITH 'No data read handler registered'
                     INTO DATA(dummy).
    raise_syst( ).

  ENDMETHOD.

  METHOD raise_syst.

    RAISE EXCEPTION TYPE lcx_file_error
      EXPORTING
        msg = VALUE #( msgty = sy-msgty
                       msgid = sy-msgid
                       msgno = sy-msgno
                       msgv1 = sy-msgv1
                       msgv2 = sy-msgv2
                       msgv3 = sy-msgv3
                       msgv4 = sy-msgv4 ).

  ENDMETHOD.

  METHOD get_text.

    result = COND #( WHEN m_msg IS NOT INITIAL THEN _get_msg_text(  )
                     ELSE super->get_text( ) ).


  ENDMETHOD.

  METHOD _get_msg_text.

    MESSAGE ID m_msg-msgid TYPE m_msg-msgty NUMBER m_msg-msgno
            WITH m_msg-msgv1 m_msg-msgv2 m_msg-msgv3 m_msg-msgv4
            INTO r_text.

  ENDMETHOD.


ENDCLASS.

CLASS lcl_file_api IMPLEMENTATION.

  METHOD constructor.

    m_file = i_file.

  ENDMETHOD.

  METHOD read.

    DATA: line    TYPE string,
          lt_data TYPE stringtab.

    _open_dataset( ).

    DO.

      READ DATASET m_file INTO line.
      IF sy-subrc = 0.
        INSERT line INTO TABLE lt_data.
      ELSE.
        EXIT.
      ENDIF.

      IF lines( lt_data ) = i_chunk_size.

        _raise_event_data_read( lt_data ).
        CLEAR: lt_data.

      ENDIF.

    ENDDO.

    IF lines( lt_data ) > 0.
      _raise_event_data_read( lt_data ).
    ENDIF.

    _close_dataset( ).

  ENDMETHOD.

  METHOD _close_dataset.

    CLOSE DATASET m_file.
    IF sy-subrc <> 0.
      lcx_file_error=>raise_file_close_error( ).
    ENDIF.

  ENDMETHOD.

  METHOD _open_dataset.

    OPEN DATASET m_file FOR INPUT IN TEXT MODE
                        ENCODING UTF-8.
    IF sy-subrc <> 0.
      lcx_file_error=>raise_file_open_error( ).
    ENDIF.

  ENDMETHOD.

  METHOD _raise_event_data_read.

    DATA: lr_handled TYPE REF TO abap_bool.

    CREATE DATA lr_handled.

    RAISE EVENT data_read
      EXPORTING
        er_processed = lr_handled
        et_data      = it_data.

    IF lr_handled->* = abap_false.
      lcx_file_error=>raise_data_read_not_handled( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

    DATA(file_reader) = NEW lcl_file_api( `\test` ).

    SET HANDLER handle_file_read FOR file_reader.

    file_reader->read( i_chunk_size = 10 ).

  ENDMETHOD.

  METHOD handle_file_read.

    er_processed->* = abap_true.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  TRY.
      NEW controller( )->start( ).
    CATCH lcx_file_error INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
