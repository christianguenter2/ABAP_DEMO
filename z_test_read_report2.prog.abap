*&---------------------------------------------------------------------*
*& Report z_test_read_report2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_read_report2.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""Teststa23412515942347918374819


TYPES: BEGIN OF ty_data,
         netwr TYPE vbrk-netwr,
         waerk TYPE vbrk-waerk,
       END OF ty_data,
       tty_data TYPE STANDARD TABLE OF ty_data
                     WITH NON-UNIQUE DEFAULT KEY.

DATA(table) = VALUE tty_data( ( netwr = '97.28' waerk = 'JPY' ) ).
DATA: line LIKE LINE OF table.

TRY.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table   = DATA(alv)
      CHANGING
        t_table        = table ).

    DATA(parser) = NEW zcl_bc_abap_parser( 'Z_TEST_READ_REPORT2' ).

    DATA(struct_descr) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( line ) ).

    LOOP AT struct_descr->get_components( ) ASSIGNING FIELD-SYMBOL(<component>).

      DATA(object) = <component>-type->get_ddic_object( ).

      CHECK object[ 1 ]-dtyp = 'CURR'.

      DATA(type_name) = parser->get_type( i_structure_name = struct_descr->get_relative_name( )
                                          i_field_name     = <component>-name ).

      DATA(ref_info) = parser->get_reference_information( type_name ).

      DATA(field) = parser->get_field_for_type( i_structure_name = struct_descr->get_relative_name( )
                                                i_type           = ref_info-tabname && '-' && ref_info-fieldname ).

      alv->get_columns( )->get_column( |{ <component>-name }| )->set_currency_column( |{ field }| ).

    ENDLOOP.


    alv->display( ).

  CATCH cx_salv_msg INTO DATA(error).
    MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
ENDTRY.
