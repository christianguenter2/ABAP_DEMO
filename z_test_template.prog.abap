*&---------------------------------------------------------------------*
*& Report z_test_template
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT z_test_template.

TABLES: t100.

SELECT-OPTIONS: s_arbgb FOR t100-arbgb.

PARAMETERS: p_table TYPE string OBLIGATORY,
            alv     RADIOBUTTON GROUP r1 DEFAULT 'X',
            demo    RADIOBUTTON GROUP r1,
            write   RADIOBUTTON GROUP r1.

START-OF-SELECTION.

  DATA: view TYPE REF TO zif_reporting_view.

  view = COND #( WHEN alv   = abap_true THEN NEW zcl_alv_reporting_view( )
                 WHEN demo  = abap_true THEN NEW zcl_demo_reporting_view( )
                 WHEN write = abap_true THEN NEW zcl_write_reporting_view( ) ).

  TRY.
      zcl_reporting_base=>create( i_table = p_table
                                  i_view  = view
                       )->start( ).

    CATCH zcx_invalid_type INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
