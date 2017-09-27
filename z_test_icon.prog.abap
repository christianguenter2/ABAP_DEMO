REPORT z_test_icon.

TYPES: BEGIN OF ty_data,
         disp TYPE icon_d,
         text TYPE char255,
       END OF ty_data,
       tty_data TYPE STANDARD TABLE OF ty_data.

DATA: line   TYPE ty_data,
      table  TYPE tty_data,
      alv    TYPE REF TO cl_salv_table,
      error  TYPE REF TO cx_salv_error,
      column TYPE REF TO cl_salv_column.

line-disp = icon_display.
line-text = |Hallo Welt!|.
INSERT line INTO TABLE table.

TRY.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table   = alv
      CHANGING
        t_table        = table ).

    alv->get_columns( )->set_optimize( abap_true ).

    column ?= alv->get_columns( )->get_column( `DISP` ).
    column->set_medium_text( `DISP` ).
    column->set_optimized( ' '  ).
    column->set_output_length( 4 ).

    column ?= alv->get_columns( )->get_column( `TEXT` ).
    column->set_optimized( 'X'  ).

    alv->display( ).

  CATCH cx_salv_msg INTO error.
    MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
ENDTRY.
