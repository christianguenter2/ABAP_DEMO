REPORT z_test_complex_sel_dialog.

DATA(tab_and_field) = VALUE rstabfield( tablename = 'E070'
                                        fieldname = 'TRKORR' ).

DATA: range TYPE RANGE OF e070-trkorr.


CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
  EXPORTING
    tab_and_field     = tab_and_field    " Table Name, Field Name
  TABLES
    range             = range    " Contents Table
  EXCEPTIONS
    no_range_tab      = 1
    cancelled         = 2
    internal_error    = 3
    invalid_fieldname = 4
    OTHERS            = 5.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

cl_demo_output=>display( range ).
