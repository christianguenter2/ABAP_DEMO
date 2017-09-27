REPORT z_test_archive2.

*----------------------------------------------------------------------*
*       CLASS lcl_test_archive DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test_archive DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      create
        RETURNING value(r_instance) TYPE REF TO lcl_test_archive.

    METHODS: start.

  PRIVATE SECTION.
    METHODS:
      _get_connections
        RETURNING value(rt_connections) TYPE toav0_t,

    _get_bin_data
      IMPORTING
        i_connection TYPE toav0
      RETURNING
        value(rt_bin_data) TYPE solix_tab,

    _send
      IMPORTING
        it_binary_data TYPE solix_tab.
ENDCLASS.                    "lcl_test_archive DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_test_archive IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test_archive IMPLEMENTATION.

  METHOD create.

    CREATE OBJECT r_instance.

  ENDMETHOD.                    "create

  METHOD start.

    DATA: connections TYPE STANDARD TABLE OF toav0,
          connection  LIKE LINE OF connections,
          binary_tab  TYPE solix_tab.

    connections = _get_connections( ).

    READ TABLE connections INTO connection
                           INDEX 1.
    CHECK sy-subrc = 0.

    binary_tab = _get_bin_data( connection ).

    _send( binary_tab ).

  ENDMETHOD.                    "start

  METHOD _get_connections.

    DATA: object_id  TYPE toav0-object_id.

    object_id = '100000000041'.

    CALL FUNCTION 'ARCHIV_GET_CONNECTIONS'
      EXPORTING
        objecttype    = '/SSC/00091'
        object_id     = object_id
      TABLES
        connections   = rt_connections    " Verknüpfungseinträge
      EXCEPTIONS
        nothing_found = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "_get_connection


  METHOD _get_bin_data.

    DATA: document_type   TYPE toadd-doc_type,
          length          TYPE sapb-length,
          binlength       TYPE sapb-length,
          archivobject    TYPE STANDARD TABLE OF docs ,
          binarchivobject TYPE STANDARD TABLE OF tbl1024 .

    document_type = i_connection-reserve.

    CALL FUNCTION 'ARCHIVOBJECT_GET_TABLE'
      EXPORTING
        archiv_id                = i_connection-archiv_id    " Identifikation des angesprochenen Ablagesystems
        document_type            = document_type
        archiv_doc_id            = i_connection-arc_doc_id
      IMPORTING
        length                   = length    " Anzahl der Sätze in der ARCHIVOBJECT-Tabelle
        binlength                = binlength    " Länge in Byte
      TABLES
        archivobject             = archivobject    " Tabelle, die abgelegtes Objekt enthält
        binarchivobject          = binarchivobject    " Binärer Inhalt
      EXCEPTIONS
        error_archiv             = 1
        error_communicationtable = 2
        error_kernel             = 3
        OTHERS                   = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    DATA: buffer        TYPE xstring,
          output_length TYPE i,
          input_length  TYPE i.

    input_length = binlength.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = input_length
      IMPORTING
        buffer       = buffer
      TABLES
        binary_tab   = binarchivobject
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = buffer
      IMPORTING
        output_length = output_length
      TABLES
        binary_tab    = rt_bin_data.

  ENDMETHOD.                    "_get_bin_data

  METHOD _send.

    DATA: lo_bcs      TYPE REF TO cl_bcs,
          lo_rec      TYPE REF TO cl_sapuser_bcs,
          lo_document TYPE REF TO cl_document_bcs,
          lo_doc      TYPE REF TO cl_document_bcs,
          error       TYPE REF TO cx_bcs.

    TRY.
        lo_bcs = cl_bcs=>create_persistent( ).
        lo_rec = cl_sapuser_bcs=>create( sy-uname ).

        lo_bcs->add_recipient(
          EXPORTING
            i_recipient      = lo_rec ).

        lo_bcs->add_recipient( cl_cam_address_bcs=>create_internet_address( 'christian.guenter@hansgrohe.com' ) ).

        lo_document = cl_document_bcs=>create_document(
            i_type        = 'PDF'
            i_subject     = 'Test'
            i_hex         = it_binary_data ).

        lo_bcs->set_document( lo_document ).

        lo_bcs->send( ).

      CATCH cx_bcs INTO error.
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    COMMIT WORK.
    MESSAGE 'Mail sent' TYPE 'S'.

  ENDMETHOD.                    "_send


ENDCLASS.                    "lcl_test_archive IMPLEMENTATION

START-OF-SELECTION.
  lcl_test_archive=>create( )->start( ).
