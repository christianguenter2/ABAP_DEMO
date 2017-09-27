*&---------------------------------------------------------------------*
*& Report  Z_TEST_ALV
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_alv.

TYPES: BEGIN OF ty_data,
         matnr TYPE matnr,
       END OF ty_data,
       tty_data TYPE STANDARD TABLE OF ty_data
                     WITH NON-UNIQUE DEFAULT KEY.


DATA: alv                 TYPE REF TO cl_salv_table,
      table               TYPE tty_data,
      lt_fieldcatalog_lvc TYPE lvc_t_fcat,
      lt_fieldcatalog_alv TYPE slis_t_fieldcat_alv.

cl_salv_table=>factory(
  IMPORTING
    r_salv_table   = alv
  CHANGING
    t_table        = table ).

cl_salv_controller_metadata=>get_lvc_fieldcatalog(
  EXPORTING
    r_columns      = alv->get_columns( )
    r_aggregations = alv->get_aggregations( )
  RECEIVING
    t_fieldcatalog = lt_fieldcatalog_lvc ).

CALL FUNCTION 'LVC_TRANSFER_TO_SLIS'
  EXPORTING
    it_fieldcat_lvc         = lt_fieldcatalog_lvc
  IMPORTING
    et_fieldcat_alv         = lt_fieldcatalog_alv
  EXCEPTIONS
    it_data_missing         = 1
    it_fieldcat_lvc_missing = 2
    OTHERS                  = 3.

cl_demo_output=>display_data( lt_fieldcatalog_alv ).
