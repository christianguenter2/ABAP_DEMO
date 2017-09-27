REPORT z_test_alv_currency.

TYPES: BEGIN OF ty_data,
         netwr  TYPE vbrk-netwr,
         waerk  TYPE vbrk-waerk,
         netwr2 TYPE vbrk-netwr,
         waerk2 TYPE vbrk-waerk,
       END OF ty_data,
       tty_data TYPE STANDARD TABLE OF ty_data
                     WITH NON-UNIQUE DEFAULT KEY.

DATA: alv             TYPE REF TO cl_salv_table,
      table           TYPE tty_data,
      line            LIKE LINE OF table,
      lo_struct_descr TYPE REF TO cl_abap_structdescr,
      components      TYPE cl_abap_structdescr=>component_table,
      object          TYPE dd_x031l_table,
      object2         TYPE dd_x031l_table,
      parser          TYPE REF TO zcl_bc_abap_parser,
      type_name       TYPE string,
      ref_info        TYPE zcl_bc_abap_parser=>ty_reference_information,
      field           TYPE string.

FIELD-SYMBOLS: <component>  LIKE LINE OF components,
               <component2> LIKE LINE OF components,
               <object>     LIKE LINE OF object,
               <object2>    LIKE LINE OF object.

line-netwr  = '97.28'.
line-waerk  = 'JPY'.
line-netwr2 = '100'.
line-waerk2 = 'EUR'.
INSERT line INTO TABLE table.

lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( line ).

components = lo_struct_descr->get_components( ).

TRY.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table   = alv
      CHANGING
        t_table        = table ).

    CREATE OBJECT parser
      EXPORTING
        i_repid = cl_abap_syst=>get_current_program( ).

    LOOP AT components ASSIGNING <component>.
      object = <component>-type->get_ddic_object( ).

      READ TABLE object ASSIGNING <object>
                        INDEX 1.
      CHECK sy-subrc = 0.

      IF <object>-dtyp = 'CURR'.

*        type_name = parser->get_type( i_structure_name = lo_struct_descr->get_relative_name( )
*                                      i_field_name     = <component>-name ).
*
*        ref_info = parser->get_reference_information( type_name ).
*
*        field = parser->get_field_for_type( i_structure_name = lo_struct_descr->get_relative_name( )
*                                            i_type           = ref_info-tabname && '-' && ref_info-fieldname ).

        field = parser->get_currency_field_for_field( i_structure_name = lo_struct_descr->get_relative_name( )
                                                      i_field_name     = <component>-name ).

        alv->get_columns( )->get_column( |{ <component>-name }| )->set_currency_column( |{ field }| ).

*        LOOP AT components ASSIGNING <component2>.
*          object2 = <component2>-type->get_ddic_object( ).
*
*          READ TABLE object2 ASSIGNING <object2>
*                             INDEX 1.
*          CHECK sy-subrc = 0.
*
*          IF <object2>-dtyp = 'CUKY'.
*            alv->get_columns( )->get_column( |{ <component>-name }| )->set_currency_column( |{ <component2>-name }| ).
*          ENDIF.
*
*        ENDLOOP.

      ENDIF.

    ENDLOOP.

    alv->display( ).

  CATCH cx_salv_msg.    " ALV: Allg. Fehlerklasse  mit Meldung
ENDTRY.
