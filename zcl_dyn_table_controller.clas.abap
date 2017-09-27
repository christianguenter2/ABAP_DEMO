class ZCL_DYN_TABLE_CONTROLLER definition
  public
  final
  create public .

public section.

  class-methods CREATE_DYNAMIC_TABLE
    importing
      !I_COLS type I
    returning
      value(R_DREF) type ref to DATA .
  class-methods FILL_TABLE
    importing
      !I_COLS type I
      !I_ROWS type I
    changing
      !CT_TABLE type STANDARD TABLE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DYN_TABLE_CONTROLLER IMPLEMENTATION.


METHOD create_dynamic_table.
  DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr,
        lo_table_descr TYPE REF TO cl_abap_tabledescr,
        lt_components TYPE cl_abap_structdescr=>component_table.

  FIELD-SYMBOLS: <component> LIKE LINE OF lt_components.

  DO i_cols TIMES.
    APPEND INITIAL LINE TO lt_components ASSIGNING <component>.
    <component>-name = to_upper( 'COL' && sy-index ).
    <component>-type ?= cl_abap_datadescr=>describe_by_name( p_name = 'STRING' ).
  ENDDO.

  lo_struct_descr = cl_abap_structdescr=>create(
                          p_components = lt_components ).

  lo_table_descr  = cl_abap_tabledescr=>create(
                          p_line_type  = lo_struct_descr ).

  CREATE DATA r_dref TYPE HANDLE lo_table_descr.
ENDMETHOD.


METHOD fill_table.
  DATA: fname TYPE string,
        lo_random TYPE REF TO cl_abap_random_int.
  FIELD-SYMBOLS: <row> TYPE any,
                 <field> TYPE any.

  lo_random = cl_abap_random_int=>create( ).

  DO i_rows TIMES.
    APPEND INITIAL LINE TO ct_table ASSIGNING <row>.
    DO i_cols TIMES.
      fname = to_upper( '<row>-col' && sy-index ).
      ASSIGN (fname) TO <field>.
      <field> = lo_random->get_next( ).
    ENDDO.
  ENDDO.
ENDMETHOD.
ENDCLASS.
