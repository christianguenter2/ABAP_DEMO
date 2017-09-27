*&---------------------------------------------------------------------*
*& Report  Z_TEST_GOOGLE_CHART
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_google_chart.

DATA: lo_client    TYPE REF TO if_http_client,
      xstring      TYPE xstring,
      message      TYPE string,
      funcname     TYPE funcname,
      outputparams TYPE sfpoutputparams.

cl_http_client=>create_by_url(
  EXPORTING
    url                = 'http://chart.googleapis.com/chart?cht=p3&chd=t:60,40&chs=500x200&chl=Hello|World  '
    proxy_host         = '10.6.9.240'
    proxy_service      = '8080'
  IMPORTING
    client             = lo_client    " HTTP Client Abstraction
  EXCEPTIONS
    argument_not_found = 1
    plugin_not_active  = 2
    internal_error     = 3
    OTHERS             = 4 ).

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            DISPLAY LIKE sy-msgty.
  RETURN.
ENDIF.

lo_client->send(
  EXCEPTIONS
    http_communication_failure = 1
    http_invalid_state         = 2
    http_processing_failed     = 3
    http_invalid_timeout       = 4
    OTHERS                     = 5 ).

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            DISPLAY LIKE sy-msgty.
  RETURN.
ENDIF.

lo_client->receive(
  EXCEPTIONS
    http_communication_failure = 1
    http_invalid_state         = 2
    http_processing_failed     = 3
    OTHERS                     = 4 ).

IF sy-subrc <> 0.
  lo_client->get_last_error(
    IMPORTING
      message = message ).

  MESSAGE message TYPE 'S' DISPLAY LIKE 'E'.
  RETURN.
ENDIF.

xstring = lo_client->response->get_data( ).

CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
  EXPORTING
    i_name     = 'Z_TEST_DOCUMENT'
  IMPORTING
    e_funcname = funcname.    " Name des dem Formular zugeordneten Funktionsbausteins

CALL FUNCTION 'FP_JOB_OPEN'
  CHANGING
    ie_outputparams = outputparams    " Formularprozessierung Ausgabeparameter
  EXCEPTIONS
    cancel          = 1
    usage_error     = 2
    system_error    = 3
    internal_error  = 4
    OTHERS          = 5.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            DISPLAY LIKE sy-msgty.
  RETURN.
ENDIF.

CALL FUNCTION funcname
  EXPORTING
    i_bild         = xstring
  EXCEPTIONS
    usage_error    = 1
    system_error   = 2
    internal_error = 3
    OTHERS         = 4.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            DISPLAY LIKE sy-msgty.
  RETURN.
ENDIF.

CALL FUNCTION 'FP_JOB_CLOSE'
  EXCEPTIONS
    usage_error    = 1
    system_error   = 2
    internal_error = 3
    OTHERS         = 4.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            DISPLAY LIKE sy-msgty.
  RETURN.
ENDIF.
