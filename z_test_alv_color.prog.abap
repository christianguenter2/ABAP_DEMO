*&---------------------------------------------------------------------*
*& Report  Z_TEST_ALV_COLOR
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT Z_TEST_ALV_COLOR.

TYPES:
  BEGIN OF ts_data,
    col   TYPE lvc_col,    " color 1-7
    int   TYPE lvc_int,    " intensified 0-1
    inv   TYPE lvc_inv,    " inverse 0-1
    scol  TYPE lvc_t_scol, " table for cell coloring
  END OF ts_data,
  tt_data TYPE TABLE OF ts_data.

DATA:
  ls_data    TYPE ts_data,
  lt_data    TYPE tt_data,
  lv_index   TYPE i,
  ls_color   TYPE lvc_s_scol,
  lt_color   TYPE lvc_t_scol,
  lo_table   TYPE REF TO cl_salv_table,
  lo_columns TYPE REF TO cl_salv_columns_table,
  lo_column  TYPE REF TO cl_salv_column_list.

* empty row with default color
APPEND ls_data TO lt_data.

* rows with possible colors
DO 7 TIMES.

  ls_color-color-col = sy-index.   " 1-7

  DO 2 TIMES.

    lv_index = sy-index - 1.
    ls_color-color-int = lv_index.   " 0-1

    DO 2 TIMES.

      lv_index = sy-index - 1.
      ls_color-color-inv = lv_index.   " 0-1

      APPEND ls_color TO lt_color.

      ls_data-col   = ls_color-color-col.
      ls_data-int   = ls_color-color-int.
      ls_data-inv   = ls_color-color-inv.

      ls_data-scol = lt_color.
      APPEND ls_data TO lt_data.
      CLEAR lt_color.
    ENDDO.
  ENDDO.
ENDDO.

TRY.
    CALL METHOD cl_salv_table=>factory
      IMPORTING
        r_salv_table = lo_table
      CHANGING
        t_table      = lt_data.

    lo_columns = lo_table->get_columns( ).

*   set descriptions of columns
    lo_column ?= lo_columns->get_column( 'COL' ).
    lo_column->set_short_text( 'COL' ).

    lo_column ?= lo_columns->get_column( 'INT' ).
    lo_column->set_short_text( 'INT' ).

    lo_column ?= lo_columns->get_column( 'INV' ).
    lo_column->set_short_text( 'INV' ).

*   set the column with the information about colors of rows and fields
    lo_columns->set_color_column( 'SCOL' ).

    lo_table->display( ).

  CATCH cx_salv_msg.             " cl_salv_table=>factory
    WRITE: / 'cx_salv_msg exception'.
    STOP.

  CATCH cx_salv_data_error.      " cl_salv_filters->add_filter()
    WRITE: / 'cx_salv_data_error'.
    STOP.

  CATCH cx_salv_not_found.       " cl_salv_columns_table->get_column()
    WRITE: / 'cx_salv_not_found exception'.
    STOP.
ENDTRY.
