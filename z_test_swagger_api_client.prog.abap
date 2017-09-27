*&---------------------------------------------------------------------*
*& Report z_test_swagger_api_client
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_swagger_api_client.

PARAMETERS: get    TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X',
            post   TYPE abap_bool RADIOBUTTON GROUP r1,
            delete TYPE abap_bool RADIOBUTTON GROUP r1,
            trans  TYPE abap_bool RADIOBUTTON GROUP r1.

PARAMETERS: pet TYPE c LENGTH 20 OBLIGATORY DEFAULT 1.

CLASS lcx_error DEFINITION CREATE PUBLIC
                INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    CLASS-METHODS:
      raise_syst
        RAISING
          lcx_error,

      raise_text
        IMPORTING
          i_text TYPE string
        RAISING
          lcx_error.

    METHODS:
      constructor
        IMPORTING
          textid   LIKE textid OPTIONAL
          previous LIKE previous OPTIONAL
          msg      TYPE symsg OPTIONAL
          text     TYPE csequence OPTIONAL,

      get_text REDEFINITION.

  PRIVATE SECTION.
    DATA:
      m_msg  TYPE symsg,
      m_text TYPE string.

    METHODS:
      _get_msg_text
        RETURNING
          VALUE(r_message_text) TYPE string.

ENDCLASS.

CLASS lcx_error IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid = textid previous = previous ).
    m_msg  = msg.
    m_text = text.

  ENDMETHOD.

  METHOD raise_syst.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        msg = VALUE symsg( msgty = sy-msgty
                           msgid = sy-msgid
                           msgno = sy-msgno
                           msgv1 = sy-msgv1
                           msgv2 = sy-msgv2
                           msgv3 = sy-msgv3
                           msgv4 = sy-msgv4 ).

  ENDMETHOD.

  METHOD get_text.

    result = COND #( WHEN m_msg IS NOT INITIAL THEN _get_msg_text( )
                     WHEN m_text IS NOT INITIAL THEN m_text
                     ELSE super->get_text( ) ).

  ENDMETHOD.


  METHOD _get_msg_text.

    MESSAGE ID m_msg-msgid TYPE m_msg-msgty NUMBER m_msg-msgno
            WITH m_msg-msgv1 m_msg-msgv2 m_msg-msgv3 m_msg-msgv4
            INTO r_message_text.

  ENDMETHOD.



  METHOD raise_text.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        text = i_text.

  ENDMETHOD.

ENDCLASS.

INTERFACE lif_pet.

  TYPES: ty_id TYPE i,

         BEGIN OF ty_category,
           id   TYPE ty_id,
           name TYPE string,
         END OF ty_category,

         BEGIN OF ty_tag,
           id   TYPE ty_id,
           name TYPE string,
         END OF ty_tag,
         tty_tag TYPE STANDARD TABLE OF ty_tag
                      WITH NON-UNIQUE DEFAULT KEY,

         BEGIN OF ty_pet,
           id        TYPE ty_id,
           category  TYPE ty_category,
           photourls TYPE stringtab,
           tags      TYPE tty_tag,
           status    TYPE string,
         END OF ty_pet.

ENDINTERFACE.

CLASS client DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        RAISING
          lcx_error,

      run
        RAISING
          lcx_error.

  PRIVATE SECTION.
    DATA: mo_client TYPE REF TO if_http_client.

    METHODS:
      get
        RAISING
          lcx_error,
      post,
      delete,
      transform.

ENDCLASS.

CLASS client IMPLEMENTATION.

  METHOD constructor.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = 'http://petstore.swagger.io'
      IMPORTING
        client             = mo_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).

    IF sy-subrc <> 0.
      lcx_error=>raise_syst( ).
    ENDIF.

    cl_http_utility=>set_request_uri( request = mo_client->request
                                      uri     = '/v2/pet' ).

  ENDMETHOD.

  METHOD run.

    CASE abap_true .
      WHEN get.
        get( ).
        RETURN.
      WHEN post.
        post( ).
      WHEN delete.
        delete( ).
      WHEN trans.
        transform( ).
        RETURN.
      WHEN OTHERS.
        lcx_error=>raise_text( |Operation not supported!| ).
    ENDCASE.

    mo_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).

    IF sy-subrc <> 0.
      lcx_error=>raise_syst( ).
    ENDIF.

    mo_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).

    IF sy-subrc <> 0.
      lcx_error=>raise_syst( ).
    ENDIF.

    mo_client->response->get_status(
      IMPORTING
        code   = DATA(code)
        reason = DATA(reason) ).

    DATA(cdata) = mo_client->response->get_cdata( ).

    cl_demo_output=>write( code ).
    cl_demo_output=>write( reason ).
    cl_demo_output=>write( cdata ).
    cl_demo_output=>display(  ).

  ENDMETHOD.


  METHOD get.

    DATA: ls_pet TYPE lif_pet=>ty_pet.

    mo_client->request->set_method( method = if_http_request=>co_request_method_get ).
    mo_client->request->set_content_type( 'application/xml' ).
    mo_client->request->set_header_field( name  = 'accept'
                                          value = 'application/xml' ).

    cl_http_utility=>set_request_uri( request = mo_client->request
                                      uri     = '/v2/pet/' && pet ).

    mo_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).

    IF sy-subrc <> 0.
      lcx_error=>raise_syst( ).
    ENDIF.

    mo_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).

    IF sy-subrc <> 0.
      lcx_error=>raise_syst( ).
    ENDIF.

    mo_client->response->get_status(
      IMPORTING
        code   = DATA(code)
        reason = DATA(reason) ).

    CASE code.
      WHEN 200.

        DATA(data) = mo_client->response->get_data( ).

        CALL TRANSFORMATION ztest_pet_in SOURCE XML data
                                         RESULT pet = ls_pet.

        cl_demo_output=>display( ls_pet ).

      WHEN OTHERS.
        cl_demo_output=>display( reason ).
    ENDCASE.


  ENDMETHOD.


  METHOD post.

    DATA(new_pet) = '<?xml version="1.0" encoding="UTF-8"?>' &&
                    '<Pet>'                                  &&
                    '  <id>0</id>'                           &&
                    '  <Category>'                           &&
                    '    <id>0</id>'                         &&
                    '    <name>string</name>'                &&
                    '  </Category>'                          &&
                    '  <name>doggie</name>'                  &&
                    '  <photoUrl>'                           &&
                    '    <photoUrl>string</photoUrl>'        &&
                    '  </photoUrl>'                          &&
                    '  <tag>'                                &&
                    '    <Tag>'                              &&
                    '      <id>0</id>'                       &&
                    '      <name>string</name>'              &&
                    '    </Tag>'                             &&
                    '  </tag>'                               &&
                    '  <status>available</status>'           &&
                    '</Pet>'.

    mo_client->request->set_method( method = if_http_request=>co_request_method_post ).
    mo_client->request->set_content_type( 'application/xml' ).

    mo_client->request->set_cdata( data = new_pet ).


  ENDMETHOD.

  METHOD delete.

    mo_client->request->set_method( 'DELETE' ).

    cl_http_utility=>set_request_uri( request = mo_client->request
                                      uri     = '/v2/pet/' && pet ).

  ENDMETHOD.


  METHOD transform.

    DATA(pet) = VALUE lif_pet=>ty_pet( id        = '0815'
                                       category  = VALUE #( id   = '1'
                                                            name = 'Test Cat' )
                                       photourls = VALUE #( ( `www.test.de` ) )
                                       tags      = VALUE #( ( id   = '1'
                                                              name = 'first tag' ) )
                                       status    = 'available' ).


    CALL TRANSFORMATION id SOURCE pet = pet
                           RESULT XML DATA(xml).

    cl_demo_output=>display_xml( xml ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  TRY.
      NEW client( )->run( ).
    CATCH lcx_error INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
