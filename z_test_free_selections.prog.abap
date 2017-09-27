*&---------------------------------------------------------------------*
*& Report z_test_free_selections
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_free_selections.

PARAMETERS dbtab TYPE tabname DEFAULT 'SPFLI'.

CLASS demo DEFINITION.
  PUBLIC SECTION.
    METHODS main.

  PRIVATE SECTION.
    METHODS check_existence_and_authority
      RETURNING VALUE(checked_dbtab) TYPE tabname.

ENDCLASS.

CLASS demo IMPLEMENTATION.
  METHOD main.

    DATA: selid     TYPE  rsdynsel-selid,
          field_tab TYPE TABLE OF rsdsfields,
          table_tab TYPE TABLE OF rsdstabs,
          cond_tab  TYPE rsds_twhere,
          dref      TYPE REF TO data.

    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE,
                   <cond>  LIKE LINE OF cond_tab.

    DATA(checked_dbtab) = check_existence_and_authority( ).

    table_tab = VALUE #( ( prim_tab = checked_dbtab ) ).

    CALL FUNCTION 'FREE_SELECTIONS_INIT'
      EXPORTING
        kind         = 'T'
      IMPORTING
        selection_id = selid
      TABLES
        tables_tab   = table_tab
      EXCEPTIONS
        OTHERS       = 4.

    IF sy-subrc <> 0.
      MESSAGE 'Error in initialization' TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE PROGRAM.
    ENDIF.

    CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
      EXPORTING
        selection_id  = selid
        title         = 'Free Selection'
        as_window     = 'X'
      IMPORTING
        where_clauses = cond_tab
      TABLES
        fields_tab    = field_tab
      EXCEPTIONS
        OTHERS        = 4.

    IF sy-subrc <> 0.
      MESSAGE 'No free selection created' TYPE 'I'.
      LEAVE PROGRAM.
    ENDIF.

    ASSIGN cond_tab[ tablename = checked_dbtab ] TO <cond>.
    IF sy-subrc <> 0.
      MESSAGE 'Error in condition' TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE PROGRAM.
    ENDIF.

    CREATE DATA dref TYPE TABLE OF (checked_dbtab).
    ASSIGN dref->* TO <table>.

    TRY.
        SELECT *
               FROM (checked_dbtab)
               WHERE (<cond>-where_tab)
               INTO TABLE @<table>.
      CATCH cx_sy_dynamic_osql_error.
        MESSAGE 'Error in dynamic Open SQL' TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE PROGRAM.
    ENDTRY.

    TRY.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = DATA(alv)
          CHANGING  t_table      = <table> ).
        alv->display( ).
      CATCH cx_salv_msg.
        MESSAGE 'Error in ALV display' TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD check_existence_and_authority.

    checked_dbtab = dbtab.

    TRY.
      CATCH cx_abap_not_a_table.
        MESSAGE 'Database table not found' TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE PROGRAM.
      CATCH cx_abap_not_in_package.
        MESSAGE 'Only tables from the flight data model are allowed'
                 TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE PROGRAM.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW demo( )->main( ).
