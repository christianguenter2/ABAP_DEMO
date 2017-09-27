*&---------------------------------------------------------------------*
*& Report z_test_2017_07_12
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_07_12.

PARAMETERS: file TYPE string OBLIGATORY LOWER CASE DEFAULT `/tmp/10223746.0951.02.getResource.java.std.hs_err_pid10223746.chaitin.tier2.gdl`.

CLASS file_upload_callback DEFINITION DEFERRED.

INTERFACE lif_callback.
  METHODS: data_read
    IMPORTING
      it_data TYPE stringtab.
ENDINTERFACE.

CLASS lcx_file_error DEFINITION FINAL
                     INHERITING FROM cx_static_check.

  PUBLIC SECTION.

    CLASS-METHODS:
      raise_file_open_error
        RAISING
          lcx_file_error,

      raise_file_close_error
        RAISING
          lcx_file_error,

      raise_syst
        RAISING
          lcx_file_error.

    METHODS:
      constructor
        IMPORTING
          textid   LIKE textid OPTIONAL
          previous LIKE previous OPTIONAL
          msg      TYPE symsg,

      get_text REDEFINITION.

  PRIVATE SECTION.
    DATA: m_msg TYPE symsg.

    METHODS:
      _get_message
        RETURNING
          VALUE(r_text) TYPE string.

ENDCLASS.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start
        RAISING
          lcx_file_error.

ENDCLASS.

CLASS file_uploader DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          i_file       TYPE csequence
          io_callback  TYPE REF TO lif_callback
          i_chunk_size TYPE i,

      start_upload
        RAISING
          lcx_file_error.

  PRIVATE SECTION.
    DATA: m_file        TYPE string,
          mo_callback   TYPE REF TO lif_callback,
          m_chunck_size TYPE i,
          mt_data       TYPE stringtab.

    METHODS:
      _open_dataset
        RAISING
          lcx_file_error,

      _close_dataset
        RAISING
          lcx_file_error,

      _process_dataset
        IMPORTING
          i_line TYPE string,

      _execute_callback.

ENDCLASS.


CLASS file_upload_callback DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES: lif_callback.
    ALIASES: data_read FOR lif_callback~data_read.

    METHODS:
      finish.

ENDCLASS.

CLASS lcx_file_error IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid = textid previous = previous ).
    m_msg = msg.

  ENDMETHOD.

  METHOD raise_file_open_error.

    MESSAGE e547(42) WITH 'File open error'
                     INTO DATA(dummy).
    raise_syst( ).

  ENDMETHOD.


  METHOD raise_file_close_error.

    MESSAGE e547(42) WITH 'File close error'
                     INTO DATA(dummy).
    raise_syst( ).

  ENDMETHOD.

  METHOD raise_syst.

    RAISE EXCEPTION TYPE lcx_file_error
      EXPORTING
        msg = VALUE symsg(  msgty = sy-msgty
                            msgid = sy-msgid
                            msgno = sy-msgno
                            msgv1 = sy-msgv1
                            msgv2 = sy-msgv2
                            msgv3 = sy-msgv3
                            msgv4 = sy-msgv4 ).

  ENDMETHOD.

  METHOD get_text.

    result = COND #( WHEN m_msg IS NOT INITIAL THEN _get_message( )
                     ELSE super->get_text( ) ).

  ENDMETHOD.

  METHOD _get_message.

    MESSAGE ID m_msg-msgid TYPE m_msg-msgty NUMBER m_msg-msgno
            WITH m_msg-msgv1 m_msg-msgv2 m_msg-msgv3 m_msg-msgv4
            INTO r_text.

  ENDMETHOD.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

    DATA(lo_file_upload_callback) = NEW file_upload_callback( ).

    NEW file_uploader( i_file 			= file
                       io_callback	= lo_file_upload_callback
                       i_chunk_size = 10 )->start_upload( ).

    lo_file_upload_callback->finish( ).

  ENDMETHOD.

ENDCLASS.

CLASS file_uploader IMPLEMENTATION.

  METHOD constructor.

    m_file				= i_file.
    mo_callback 	= io_callback.
    m_chunck_size = i_chunk_size.

  ENDMETHOD.

  METHOD start_upload.

    DATA: line TYPE string.

    _open_dataset( ).

    DO.
      READ DATASET m_file INTO line.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      _process_dataset( line ).

    ENDDO.

    _execute_callback( ).

    _close_dataset( ).

  ENDMETHOD.

  METHOD _open_dataset.

    OPEN DATASET m_file FOR INPUT IN TEXT MODE ENCODING UTF-8.
    IF sy-subrc <> 0.
      lcx_file_error=>raise_file_open_error( ).
    ENDIF.

  ENDMETHOD.

  METHOD _close_dataset.

    CLOSE DATASET m_file.
    IF sy-subrc <> 0.
      lcx_file_error=>raise_file_close_error( ).
    ENDIF.

  ENDMETHOD.

  METHOD _process_dataset.

    INSERT i_line INTO TABLE mt_data.

    IF lines( mt_data ) = m_chunck_size.

      _execute_callback( ).
      CLEAR mt_data.

    ENDIF.

  ENDMETHOD.

  METHOD _execute_callback.

    CHECK lines( mt_data ) > 0.

    mo_callback->data_read( mt_data ).

  ENDMETHOD.

ENDCLASS.

CLASS file_upload_callback IMPLEMENTATION.

  METHOD lif_callback~data_read.

    cl_demo_output=>write( it_data ).

  ENDMETHOD.

  METHOD finish.

    cl_demo_output=>display( ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  TRY.
      NEW controller( )->start( ).

    CATCH lcx_file_error INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
