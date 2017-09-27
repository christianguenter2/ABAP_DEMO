*&---------------------------------------------------------------------*
*& Report  Z_TEST_CODEPAGE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_codepage.

DATA: maktx TYPE maktx,
      text  TYPE string.

maktx = 'AXOR STARCK V USŁUGA SERWISOWA 23%"”'.

CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
  EXPORTING
    intext  = maktx
  IMPORTING
    outtext = maktx
  EXCEPTIONS
    OTHERS  = 6.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

cl_demo_output=>write_data( maktx ).

text = 'Dies ist ein Test-Text mit deutschen Umlauten äöüÄÖÜ'.

CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
  EXPORTING
    intext  = text
  IMPORTING
    outtext = text
  EXCEPTIONS
    OTHERS  = 6.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

cl_demo_output=>write_data( text ).
cl_demo_output=>display( ).
