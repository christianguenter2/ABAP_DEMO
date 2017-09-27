*&---------------------------------------------------------------------*
*& Report z_test_2017_02_03
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_02_03.

CLASS test_alv DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      start.

  PRIVATE SECTION.

    CONSTANTS: BEGIN OF co_color,
                 BEGIN OF blue,
                   col TYPE	lvc_col VALUE 1,
                   int TYPE	lvc_int VALUE 1,
                   inv TYPE	lvc_inv VALUE 0,
                 END   OF blue,
                 BEGIN OF light_green,
                   col TYPE	lvc_col VALUE 5,
                   int TYPE	lvc_int VALUE 0,
                   inv TYPE	lvc_inv VALUE 0,
                 END OF light_green,
                 BEGIN OF yellow,
                   col TYPE	lvc_col VALUE 3,
                   int TYPE	lvc_int VALUE 1,
                   inv TYPE	lvc_inv VALUE 0,
                 END OF yellow,
               END OF co_color.

    TYPES: BEGIN OF ty_data.
        INCLUDE TYPE t100.
    TYPES: cell_type TYPE salv_t_int4_column,
           color     TYPE lvc_t_scol,
           END OF ty_data,
           tty_data TYPE STANDARD TABLE OF ty_data
                      WITH NON-UNIQUE DEFAULT KEY.

    DATA: t100_tab TYPE tty_data.

    METHODS:
      _select,
      _processing,
      _display,
      _handle_link_click FOR EVENT link_click OF if_salv_events_actions_table
        IMPORTING
            row column.

ENDCLASS.

CLASS test_alv IMPLEMENTATION.

  METHOD start.

    _select( ).
    _processing( ).
    _display( ).

  ENDMETHOD.

  METHOD _select.

    SELECT FROM t100
           FIELDS *
           INTO CORRESPONDING FIELDS OF TABLE @t100_tab
           UP TO 100 ROWS.

  ENDMETHOD.

  METHOD _processing.

    DATA(temp_tab) = VALUE tty_data( LET rnd = cl_abap_random_int=>create( seed = CONV #( sy-uzeit )
                                                                           min  = 0
                                                                           max  = 1 ) IN
                                     FOR t100 IN t100_tab
                                     ( VALUE #( BASE CORRESPONDING #( t100 )
                                                cell_type = VALUE salv_t_int4_column( ( columnname = 'TEXT'
                                                                                        value      =  COND #( WHEN sy-tabix MOD 2 = rnd->get_next( ) THEN if_salv_c_cell_type=>hotspot
                                                                                                              ELSE if_salv_c_cell_type=>text ) ) )
                                                color = VALUE lvc_t_scol( ( fname = 'TEXT'
                                                                            color = COND #( WHEN sy-tabix MOD 2 = rnd->get_next( ) THEN co_color-light_green
                                                                                            ELSE co_color-yellow ) ) ) ) ) ).

    t100_tab = temp_tab.

  ENDMETHOD.

  METHOD _display.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(alv)
          CHANGING
            t_table        = t100_tab ).

        DATA(columns) = CAST cl_salv_columns_table( alv->get_columns( ) ).

        columns->set_cell_type_column( `CELL_TYPE` ).
        columns->set_color_column( `COLOR` ).

        DATA(event) = alv->get_event( ).
        SET HANDLER _handle_link_click FOR event.

        alv->display( ).

      CATCH cx_salv_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD _handle_link_click.

    MESSAGE |row { row } col { column }| TYPE 'I'.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_alv( )->start( ).
