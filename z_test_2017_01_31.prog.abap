*&---------------------------------------------------------------------*
*& Report z_test_2017_01_31
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_01_31.

CLASS test_hotspot DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

  PRIVATE SECTION.
    METHODS:
      on_link_click FOR EVENT link_click OF if_salv_events_actions_table IMPORTING row column.

ENDCLASS.

CLASS test_hotspot IMPLEMENTATION.

  METHOD start.

    TYPES: BEGIN OF ty_data.
        INCLUDE TYPE t100.
    TYPES: cell_type TYPE salv_t_int4_column,
           END OF ty_data,
           tty_data TYPE STANDARD TABLE OF ty_data
                    WITH NON-UNIQUE DEFAULT KEY.

    DATA: t100_tab TYPE tty_data.

    SELECT FROM t100
           FIELDS *
           INTO CORRESPONDING FIELDS OF TABLE @t100_tab
           UP TO 100 ROWS.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(alv)
          CHANGING
            t_table        = t100_tab ).


        DATA(rnd) = cl_abap_random_int=>create( seed = CONV #( sy-uzeit )
                                                min  = 0
                                                max  = 1 ).

        LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).

          IF sy-tabix MOD 2 = rnd->get_next( ).

            INSERT VALUE salv_s_int4_column( columnname = `TEXT`
                                             value      =   if_salv_c_cell_type=>hotspot )
                   INTO TABLE <t100>-cell_type.

          ENDIF.

        ENDLOOP.

        alv->get_columns( )->set_cell_type_column( `CELL_TYPE` ).

        DATA(event) = alv->get_event( ).
        SET HANDLER on_link_click FOR event.

        alv->display( ).

      CATCH cx_salv_error INTO DATA(error).    "
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD on_link_click.

    MESSAGE |row { row } column { column }| TYPE 'I'.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_hotspot( )->start( ).
