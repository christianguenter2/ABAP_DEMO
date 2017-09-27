*&---------------------------------------------------------------------*
*& Report z_test_reporting_4
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_reporting_4.

PARAMETERS: table TYPE string OBLIGATORY.

START-OF-SELECTION.
  TRY.
      zcl_abstract_reporting=>create( table )->start( ).
    CATCH zcx_invalid_type INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
