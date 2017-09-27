
CLASS ltd_source_object DEFINITION FINAL CREATE PRIVATE FOR TESTING.

  PUBLIC SECTION.

    CLASS-METHODS:

      create IMPORTING i_selection     TYPE string
                       i_include       TYPE programm
                       i_trobjtype     TYPE trobjtype ##needed
                       i_wbobjtype     TYPE wbobjtype OPTIONAL
                       i_main_program  TYPE programm OPTIONAL
             RETURNING VALUE(r_result) TYPE REF TO ltd_source_object.

    INTERFACES if_quickfix_source_object PARTIALLY IMPLEMENTED.

  PRIVATE SECTION.

    DATA: source_code  TYPE string_table,
          selection    TYPE string,
          wbobjtype    TYPE wbobjtype,
          include      TYPE programm,
          trobjtype    TYPE trobjtype,
          main_program TYPE programm.

ENDCLASS.

CLASS ltd_source_object IMPLEMENTATION.

  METHOD if_quickfix_source_object~set_source_code.
    cl_abap_unit_assert=>fail( 'should not be called' ).
  ENDMETHOD.


  METHOD if_quickfix_source_object~set_source_code_as_string.
    cl_abap_unit_assert=>fail( 'should not be called' ).
  ENDMETHOD.


  METHOD if_quickfix_source_object~get_source_code_as_string.
    cl_abap_unit_assert=>fail( 'should not be called' ).
  ENDMETHOD.


  METHOD if_quickfix_source_object~get_source_code.

    source_code = me->source_code.

  ENDMETHOD.


  METHOD if_quickfix_source_object~get_wb_object.
    result = cl_wb_object=>create_from_transport_key( p_object = 'PROG' p_obj_name = 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE' ).
  ENDMETHOD.


  METHOD if_quickfix_source_object~get_object_type.
    result = me->wbobjtype.
  ENDMETHOD.


  METHOD create.
    CREATE OBJECT r_result.

    r_result->source_code = cl_art_source_repository=>get_instance( )->read_source( i_include = i_include ).
    r_result->selection   = i_selection.
    r_result->include     = i_include.
    r_result->main_program = i_main_program.
    IF i_wbobjtype IS INITIAL.
      r_result->wbobjtype = zTH_ART_CONTRIBUTOR=>workbench_object_types-report.
    ELSE.
      r_result->wbobjtype   = i_wbobjtype.
    ENDIF.
    r_result->trobjtype   = r_result->wbobjtype-objtype_tr.
  ENDMETHOD.




  METHOD if_quickfix_source_object~get_source_position.

    DATA: main_program TYPE string,
          include      TYPE string,
          start_row    TYPE i,
          start_col    TYPE i,
          end_row      TYPE i,
          end_col      TYPE i.


    IF me->include IS INITIAL AND me->main_program IS INITIAL.
      main_program = 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE'.
      include = 'DUMMY_REPORT4SOURCE_OBJ_DOUBLE'.
    ELSE.
      IF me->include CS '='.
        IF me->include(2) = 'IF'.
          main_program = cl_oo_classname_service=>get_interfacepool_name( cl_oo_classname_service=>get_intfname_by_include( me->include ) ).
        ELSE.
          main_program = cl_oo_classname_service=>get_classpool_name( cl_oo_classname_service=>get_clsname_by_include( me->include ) ).
        ENDIF.
      ELSEIF me->main_program IS INITIAL.
        main_program = me->include.
      ELSE.
        main_program = me->main_program.
      ENDIF.
      include = me->include.
    ENDIF.

    start_row = segment( val = segment( val = me->selection index = 1 sep = '-' ) index = 1 sep = ',' ).
    start_col = segment( val = segment( val = me->selection index = 1 sep = '-' ) index = 2 sep = ',' ).

    TRY.
        end_row   = segment( val = segment( val = me->selection index = 2 sep = '-' ) index = 1 sep = ',' ).
        end_col   = segment( val = segment( val = me->selection index = 2 sep = '-' ) index = 2 sep = ',' ).
      CATCH cx_sy_strg_par_val.

    ENDTRY.

    result = cl_pst_source_position=>create(
      i_main_prog = main_program
      i_include = include
      i_start_row = start_row
      i_start_col = start_col
      i_end_row   = end_row
      i_end_col   = end_col
    ).

  ENDMETHOD.

  METHOD if_quickfix_source_object~get_source_uri.

    DATA: uri_mapper       TYPE REF TO if_adt_uri_mapper,
          start_row        TYPE i,
          start_col        TYPE i,
          end_row          TYPE i,
          end_col          TYPE i,
          object_reference TYPE REF TO cl_adt_object_reference.

    start_row = segment( val = segment( val = me->selection index = 1 sep = '-' ) index = 1 sep = ',' ).
    start_col = segment( val = segment( val = me->selection index = 1 sep = '-' ) index = 2 sep = ',' ).

    TRY.
        end_row   = segment( val = segment( val = me->selection index = 2 sep = '-' ) index = 1 sep = ',' ).
        end_col   = segment( val = segment( val = me->selection index = 2 sep = '-' ) index = 2 sep = ',' ).
      CATCH cx_sy_strg_par_val.
    ENDTRY.

    uri_mapper = cl_adt_tools_core_factory=>get_instance( )->get_uri_mapper( ).
    TRY.
        object_reference = uri_mapper->map_include_to_objref(
            program            = main_program
            include            = include
            line               = start_row
            line_offset        = start_col
            end_line           = end_row
            end_offset         = end_col ).
      CATCH cx_adt_uri_mapping.    "
        RETURN.
    ENDTRY.

    uri = object_reference->ref_data-uri.

  ENDMETHOD.


  METHOD if_quickfix_source_object~get_include.
    include = me->include.
  ENDMETHOD.

ENDCLASS.

CLASS ltd_structure_source_object DEFINITION FINAL CREATE PRIVATE FOR TESTING.

  PUBLIC SECTION.

    CLASS-METHODS:

      create IMPORTING i_selection     TYPE string
                       i_source        type string_table
             RETURNING VALUE(r_result) TYPE REF TO ltd_structure_source_object.

    INTERFACES if_quickfix_source_object PARTIALLY IMPLEMENTED.

  PRIVATE SECTION.

    DATA: source_code  TYPE string_table,
          selection    TYPE string.

ENDCLASS.

CLASS ltd_structure_source_object IMPLEMENTATION.

  METHOD create.

    r_result = new ltd_structure_source_object( ).
    r_result->source_code = i_source.
    r_result->selection   = i_selection.

  ENDMETHOD.

  METHOD if_quickfix_source_object~get_description.
    cl_abap_unit_assert=>fail( 'should not be called' ).
  ENDMETHOD.

  METHOD if_quickfix_source_object~get_include.
    cl_abap_unit_assert=>fail( 'should not be called' ).
  ENDMETHOD.

  METHOD if_quickfix_source_object~get_name.
    name = 'TEST_SBD_DUMMY_STRUCTURE'.
  ENDMETHOD.

  METHOD if_quickfix_source_object~get_object_type.

    DATA: wbobjtype TYPE wbobjtype.

    wbobjtype-objtype_tr = 'TABL'.
    wbobjtype-subtype_wb = 'DS'.

    result = wbobjtype.

  ENDMETHOD.

  METHOD if_quickfix_source_object~get_source_code.
    source_code = me->source_code.
  ENDMETHOD.

  METHOD if_quickfix_source_object~get_source_code_as_string.
    cl_abap_unit_assert=>fail( 'should not be called' ).
  ENDMETHOD.

  METHOD if_quickfix_source_object~get_source_position.
    DATA: start_row    TYPE i,
          start_col    TYPE i,
          end_row      TYPE i,
          end_col      TYPE i.

    start_row = segment( val = segment( val = me->selection index = 1 sep = '-' ) index = 1 sep = ',' ).
    start_col = segment( val = segment( val = me->selection index = 1 sep = '-' ) index = 2 sep = ',' ).

    TRY.
        end_row   = segment( val = segment( val = me->selection index = 2 sep = '-' ) index = 1 sep = ',' ).
        end_col   = segment( val = segment( val = me->selection index = 2 sep = '-' ) index = 2 sep = ',' ).
      CATCH cx_sy_strg_par_val.

    ENDTRY.

    result = cl_pst_source_position=>create(
      i_main_prog = ''
      i_include = ''
      i_start_row = start_row
      i_start_col = start_col
      i_end_row   = end_row
      i_end_col   = end_col
    ).

  ENDMETHOD.

  METHOD if_quickfix_source_object~get_source_uri.
    cl_abap_unit_assert=>fail( 'should not be called' ).
  ENDMETHOD.

  METHOD if_quickfix_source_object~get_wb_object.
    result = cl_wb_object=>create_from_transport_key( p_object = 'TABL' p_obj_name = 'TEST_SBD_DUMMY_STRUCTURE' ).
  ENDMETHOD.

  METHOD if_quickfix_source_object~set_source_code.
    cl_abap_unit_assert=>fail( 'should not be called' ).
  ENDMETHOD.

  METHOD if_quickfix_source_object~set_source_code_as_string.
    cl_abap_unit_assert=>fail( 'should not be called' ).
  ENDMETHOD.

ENDCLASS.
