*&---------------------------------------------------------------------*
*& Report z_test_reporting2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_reporting2.

PARAMETERS: table TYPE string OBLIGATORY.

PARAMETERS: demo RADIOBUTTON GROUP r1 DEFAULT 'X',
            alv  RADIOBUTTON GROUP r1.

START-OF-SELECTION.
  TRY.



      zcl_demo_base_class=>create( i_table = table
                                   i_view  = NEW zcl_demo_view( ) )->start( ).
    CATCH zcx_demo_not_implemented INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
