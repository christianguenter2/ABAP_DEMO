*&---------------------------------------------------------------------*
*& Report  Z_TEST_JSON2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT Z_TEST_JSON2.


START-OF-SELECTION.
  data: input TYPE string,
        text  TYPE string.
  input = `{"TEXT":"Hello ABAP, I'm JSON!"}`.
  CALL TRANSFORMATION id SOURCE XML input
                        RESULT text = text.
  WRITE: text.
