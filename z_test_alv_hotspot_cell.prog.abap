*&---------------------------------------------------------------------*
*& Report z_test_alv_hotspot_cell
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_alv_hotspot_cell.

CLASS test_alv DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS test_alv IMPLEMENTATION.

  METHOD start.

    TYPES: BEGIN OF ty_data.
        INCLUDE TYPE t100.
    TYPES:
      cell_type TYPE salv_t_int4_column,
      color     TYPE lvc_t_scol,
      END OF ty_data,
      tty_data TYPE STANDARD TABLE OF ty_data
                    WITH NON-UNIQUE DEFAULT KEY.

    DATA: t100_tab TYPE tty_data.

    SELECT FROM t100
           FIELDS *
           INTO CORRESPONDING FIELDS OF TABLE @t100_tab
           UP TO 100 ROWS.

    DATA(rnd) = cl_abap_random_int=>create( seed = CONV #( sy-uzeit )
                                            min  = 0
                                            max  = 1 ).

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).

      IF sy-tabix MOD 2 = 0.

        INSERT VALUE salv_s_int4_column( columnname = `TEXT`
                                         value      = if_salv_c_cell_type=>hotspot )
               INTO TABLE <t100>-cell_type.


        INSERT VALUE salv_s_int4_column( columnname = `ARBGB`
                                         value      = if_salv_c_cell_type=>button )
               INTO TABLE <t100>-cell_type.

      ENDIF.

      IF sy-tabix MOD 2 = rnd->get_next( ).

        INSERT VALUE lvc_s_scol( fname = 'SPRSL'
                                 color = VALUE #( col = 5 int = 1 inv = 0 ) )
               INTO TABLE <t100>-color.

      ENDIF.

    ENDLOOP.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(alv)     " Basisklasse einfache ALV Tabellen
          CHANGING
            t_table        = t100_tab ).

        alv->get_columns( )->set_cell_type_column( `CELL_TYPE` ).
        alv->get_columns( )->set_color_column( `COLOR` ).

        alv->display( ).

      CATCH cx_salv_error INTO DATA(error).    "
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.


  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_alv( )->start( ).
