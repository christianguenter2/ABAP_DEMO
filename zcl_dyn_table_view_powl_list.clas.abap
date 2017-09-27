class ZCL_DYN_TABLE_VIEW_POWL_LIST definition
  public
  inheriting from ZCL_DYN_TABLE_VIEW_ABS_FACTORY
  final
  create public .

public section.

  interfaces IF_POWL_FEEDER .

  methods ZIF_DYN_TABLE_VIEW~DISPLAY_TABLE
    redefinition .
protected section.
private section.

  data TABLE_REF type ref to DATA .
ENDCLASS.



CLASS ZCL_DYN_TABLE_VIEW_POWL_LIST IMPLEMENTATION.


method IF_POWL_FEEDER~GET_ACTIONS.
endmethod.


method IF_POWL_FEEDER~GET_ACTION_CONF.
endmethod.


method IF_POWL_FEEDER~GET_DETAIL_COMP.
endmethod.


method IF_POWL_FEEDER~GET_FIELD_CATALOG.
endmethod.


method IF_POWL_FEEDER~GET_OBJECTS.
  FIELD-SYMBOLS: <table> TYPE ANY TABLE .

  ASSIGN table_ref->* to <table>.
  e_results = <table>.
endmethod.


METHOD if_powl_feeder~get_object_definition.
  DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr,
        lo_table_descr TYPE REF TO cl_abap_tabledescr,
        component_table TYPE cl_abap_structdescr=>component_table,
        cols TYPE i.

  FIELD-SYMBOLS: <table> TYPE ANY TABLE.

  IMPORT table_cols = cols
         FROM SHARED BUFFER indx(ar)
         ID 'ZPOWL_TABLE_COLS'.

  table_ref = zcl_dyn_table_controller=>create_dynamic_table( i_cols = cols ).

  ASSIGN table_ref->* TO <table>.

  IMPORT ct_table = <table>
         FROM SHARED BUFFER indx(ar)
         ID 'ZPOWL_TABLE'.

  e_object_def ?= cl_abap_tabledescr=>describe_by_data( p_data = <table> ).
ENDMETHOD.


method IF_POWL_FEEDER~GET_SEL_CRITERIA.
endmethod.


method IF_POWL_FEEDER~HANDLE_ACTION.
endmethod.


METHOD zif_dyn_table_view~display_table.
  DATA: parameters TYPE  tihttpnvp,
        parameter LIKE LINE OF parameters,
        lo_table_descr TYPE REF TO cl_abap_tabledescr,
        lo_struct_descr TYPE REF TO cl_abap_structdescr,
        component_table TYPE cl_abap_structdescr=>component_table,
        cols TYPE i.

  lo_table_descr ?= cl_abap_tabledescr=>describe_by_data( p_data = ct_table ).
  lo_struct_descr ?= lo_table_descr->get_table_line_type( ).
  component_table = lo_struct_descr->get_components( ).

  EXPORT ct_table = ct_table
         TO SHARED BUFFER indx(ar)
         ID 'ZPOWL_TABLE'.

  cols = lines( component_table ).
  EXPORT table_cols = cols
         TO SHARED BUFFER indx(ar)
         ID 'ZPOWL_TABLE_COLS'.

  parameter-name  = 'APPLID'.
  parameter-value = 'Z_DYN_TABLE_POWL'.
  INSERT parameter INTO TABLE parameters.

  CALL FUNCTION 'WDY_EXECUTE_IN_BROWSER'
    EXPORTING
*     protocol            =     " Gewünschtes Protokoll
      application         = 'POWL'    " Web-Dynpro-Anwendung
      parameters          = parameters    " HTTP-Framework (iHTTP) Tabelle Name/Wert-Paare
    EXCEPTIONS
      invalid_application = 1
      browser_not_started = 2
      action_cancelled    = 3
      OTHERS              = 4.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.
ENDMETHOD.
ENDCLASS.
