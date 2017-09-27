REPORT z_test_2017_06_29.

CLASS lcx_file_api_error DEFINITION CREATE PUBLIC
                         INHERITING FROM cx_static_check.

  PUBLIC SECTION.

    CLASS-METHODS:
      raise_file_open_error
        RAISING
          lcx_file_api_error,

      raise_file_close_error
        RAISING
          lcx_file_api_error,

      raise_syst
        RAISING
          lcx_file_api_error,

      raise_file_read_not_handled
        RAISING
          lcx_file_api_error.

    METHODS:
      constructor
        IMPORTING
          textid   LIKE textid OPTIONAL
          previous LIKE previous OPTIONAL
          msg      TYPE symsg OPTIONAL,

      get_text REDEFINITION.

  PRIVATE SECTION.
    DATA:
      m_msg TYPE symsg.

    METHODS:
      _get_msg_text
        RETURNING
          VALUE(r_text) TYPE string.

ENDCLASS.

CLASS lcl_file_api DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    EVENTS:
      data_chunk_read_finished
        EXPORTING
          VALUE(er_handled) TYPE REF TO abap_bool
          VALUE(et_data) TYPE stringtab.

    METHODS:
      constructor
        IMPORTING
          i_file_path  TYPE csequence
          i_chunk_size TYPE i DEFAULT 10,

      start
        RAISING
          lcx_file_api_error.

  PRIVATE SECTION.
    DATA:
      m_file_path  TYPE string,
      m_chunk_size TYPE i.

    METHODS:
      _open_dataset
        RAISING
          lcx_file_api_error,

      _close_dataset
        RAISING
          lcx_file_api_error,

      _raise_event_if_check_chunk
        CHANGING
          ct_data TYPE stringtab
        RAISING
          lcx_file_api_error,

      _raise_event_if_data
        IMPORTING
          it_data TYPE stringtab
        RAISING
          lcx_file_api_error,

      _process_dataset
        RAISING
          lcx_file_api_error,

      _raise_event_data_chunk_read
        IMPORTING
          it_data TYPE stringtab
        RAISING
          lcx_file_api_error.

ENDCLASS.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start
        RAISING
          lcx_file_api_error.

  PRIVATE SECTION.
    METHODS:
      handle_file_read FOR EVENT data_chunk_read_finished OF lcl_file_api
        IMPORTING
            er_handled
            et_data.

ENDCLASS.

CLASS lcx_file_api_error IMPLEMENTATION.

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

  METHOD raise_file_read_not_handled.

    MESSAGE e160(sr) WITH 'File read not handled'
                     INTO DATA(dummy).
    raise_syst( ).

  ENDMETHOD.

  METHOD raise_syst.

    RAISE EXCEPTION TYPE lcx_file_api_error
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

    result = COND #( WHEN m_msg IS NOT INITIAL THEN _get_msg_text( )
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

    m_file_path  = i_file_path.
    m_chunk_size = i_chunk_size.

  ENDMETHOD.

  METHOD start.

    _open_dataset( ).
    _process_dataset( ).
    _close_dataset( ).

  ENDMETHOD.

  METHOD _open_dataset.

    OPEN DATASET m_file_path FOR INPUT IN TEXT MODE
                             ENCODING UTF-8.
    IF sy-subrc <> 0.
      lcx_file_api_error=>raise_file_open_error( ).
    ENDIF.

  ENDMETHOD.

  METHOD _close_dataset.

    CLOSE DATASET m_file_path.
    IF sy-subrc <> 0.
      lcx_file_api_error=>raise_file_close_error( ).
    ENDIF.

  ENDMETHOD.

  METHOD _process_dataset.

    DATA: line    TYPE string,
          lt_data TYPE stringtab.

    DO.

      READ DATASET m_file_path INTO line.

      IF sy-subrc = 0.

        INSERT line INTO TABLE lt_data.
        _raise_event_if_check_chunk( CHANGING ct_data = lt_data ).

      ELSE.
        EXIT.
      ENDIF.

    ENDDO.

    _raise_event_if_data( lt_data ).

  ENDMETHOD.

  METHOD _raise_event_if_check_chunk.


    CHECK lines( ct_data ) > m_chunk_size.

    _raise_event_data_chunk_read( ct_data ).

    CLEAR: ct_data.

  ENDMETHOD.

  METHOD _raise_event_if_data.

    CHECK lines( it_data ) > 0.

    _raise_event_data_chunk_read( it_data ).

  ENDMETHOD.

  METHOD _raise_event_data_chunk_read.

    DATA: lr_handled TYPE REF TO abap_bool.
    CREATE DATA lr_handled.

    RAISE EVENT data_chunk_read_finished
      EXPORTING
        er_handled = lr_handled
        et_data 	 = it_data.

    IF lr_handled->* = abap_false.
      lcx_file_api_error=>raise_file_read_not_handled( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

    DATA(file_reader) = NEW lcl_file_api( i_file_path  = `/tmp/hs_err_pid10223788.log`
                                          i_chunk_size = 10 ).

    SET HANDLER handle_file_read FOR file_reader.

    file_reader->start( ).

    cl_demo_output=>display( ).

  ENDMETHOD.

  METHOD handle_file_read.

    er_handled->* = abap_true.
    cl_demo_output=>write( et_data ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  TRY.
      NEW controller( )->start( ).
    CATCH lcx_file_api_error INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
