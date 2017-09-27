*&---------------------------------------------------------------------*
*& Report  Z_TEST_SMW0
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_smw0.


DATA: query_string   TYPE STANDARD TABLE OF w3query,
      query          LIKE LINE OF query_string,
      html           TYPE STANDARD TABLE OF w3html,
      mime           TYPE STANDARD TABLE OF w3mime,
      return_code    TYPE w3param-ret_code,
      content_type   TYPE w3param-cont_type,
      content_length TYPE w3param-cont_len.

query-name  = '_OBJECT_ID'.
query-value = 'ZMM_DIMENSION_TABLE'.
INSERT query INTO TABLE query_string.

content_type = 'text/html'.

CALL FUNCTION 'WWW_GET_HTML_OBJECT'
  TABLES
    query_string        = query_string    " Parametertabelle
    html                = html    " HTML Seite
    mime                = mime    " Binäres Objekt
  CHANGING
    return_code         = return_code    " Returncode an ITS
    content_type        = content_type    " Typ des Inhalts
    content_length      = content_length    " Länge des Inhalts (muss bei binären Obj)
  EXCEPTIONS
    parameter_not_found = 1
    object_not_found    = 2
    OTHERS              = 3.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          DISPLAY LIKE sy-msgty.
ENDIF.

FIELD-SYMBOLS: <html> LIKE LINE OF html.
DATA: out TYPE string.

LOOP AT html ASSIGNING <html>.
  out = out && <html>-line.
ENDLOOP.

cl_demo_output=>display_html( out ).
