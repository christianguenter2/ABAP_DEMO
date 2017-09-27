*&---------------------------------------------------------------------*
*& Report  Z_TEST_POPUP_GET_VALUES
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_popup_get_values.

DATA: answer TYPE char01,
      value  TYPE spop-varvalue1.

CALL FUNCTION 'POPUP_TO_GET_ONE_VALUE'
  EXPORTING
    textline1      = 'Name der (komplexen) Variable/Feldsymbol'
    titel          = 'Name der (komplexen) Variable/Feldsymbol'
    valuelength    = '50'
  IMPORTING
    answer         = answer
    value1         = value
  EXCEPTIONS
    titel_too_long = 1
    OTHERS         = 2.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

cl_demo_output=>display_data( answer ).
