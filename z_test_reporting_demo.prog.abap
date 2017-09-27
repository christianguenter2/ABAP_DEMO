*&---------------------------------------------------------------------*
*& Report z_test_reporting_demo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_reporting_demo.

PARAMETERS: table TYPE string OBLIGATORY,
            demo  TYPE char01 RADIOBUTTON GROUP r1 DEFAULT 'X',
            alv   TYPE char01 RADIOBUTTON GROUP r1.

INTERFACE lif_view.
  METHODS: display IMPORTING ir_table TYPE REF TO data.

ENDINTERFACE.

CLASS lcx_invalid_type DEFINITION INHERITING FROM cx_no_check.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          i_table  TYPE csequence OPTIONAL
          textid   LIKE textid OPTIONAL
          previous LIKE previous OPTIONAL,

      get_text REDEFINITION.

  PRIVATE SECTION.
    DATA _table TYPE string.

ENDCLASS.


CLASS test_reporting_demo DEFINITION CREATE PUBLIC ABSTRACT.

  PUBLIC SECTION.
    CLASS-METHODS:
      create
        IMPORTING
          i_table           TYPE csequence
          i_view            TYPE REF TO lif_view
        RETURNING
          VALUE(r_instance) TYPE REF TO test_reporting_demo.

    METHODS:
      constructor
        IMPORTING
          i_view TYPE REF TO lif_view,

      start.

  PROTECTED SECTION.

    METHODS:
      _select ABSTRACT,
      _do_some_processing ABSTRACT.

    DATA: table TYPE REF TO data.

  PRIVATE SECTION.
    DATA: lo_view TYPE REF TO lif_view.

    METHODS:
      _display.

ENDCLASS.

CLASS t100_reporting DEFINITION CREATE PUBLIC
                     INHERITING FROM test_reporting_demo.

  PROTECTED SECTION.

    METHODS:

      _select REDEFINITION,
      _do_some_processing REDEFINITION.

  PRIVATE SECTION.

    DATA: t100_tab TYPE STANDARD TABLE OF t100.

ENDCLASS.

CLASS t000_reporting DEFINITION CREATE PUBLIC
                     INHERITING FROM test_reporting_demo.

  PROTECTED SECTION.
    METHODS: _select REDEFINITION,
      _do_some_processing REDEFINITION.

    DATA: t000_tab TYPE STANDARD TABLE OF t000
                        WITH NON-UNIQUE DEFAULT KEY.

ENDCLASS.

CLASS lcl_demo_view DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS lcl_alv_view DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS lcl_view_factory DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      create
        RETURNING VALUE(r_instance) TYPE REF TO lif_view.

ENDCLASS.

CLASS lcx_invalid_type IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid = textid previous = previous ).
    _table = i_table.

  ENDMETHOD.

  METHOD get_text.

    result = COND #( WHEN _table IS NOT INITIAL THEN |Invalig table { _table }|
                     ELSE |Invalid View| ).

  ENDMETHOD.

ENDCLASS.

CLASS test_reporting_demo IMPLEMENTATION.

  METHOD constructor.

    me->lo_view = i_view.

  ENDMETHOD.

  METHOD create.

    r_instance = COND #( WHEN i_table = `T100` THEN NEW t100_reporting( i_view )
                         WHEN i_table = `T000` THEN NEW t000_reporting( i_view )
                         ELSE THROW lcx_invalid_type( i_table = i_table ) ).

  ENDMETHOD.

  METHOD start.

    _select( ).
    _do_some_processing( ).
    _display( ).

  ENDMETHOD.

  METHOD _display.

    lo_view->display( table ).

  ENDMETHOD.

ENDCLASS.

CLASS t100_reporting IMPLEMENTATION.

  METHOD _do_some_processing.

    table = REF #( t100_tab ).

  ENDMETHOD.

  METHOD _select.

    SELECT *
           FROM t100
           INTO TABLE @t100_tab
           UP TO 100 ROWS.

  ENDMETHOD.

ENDCLASS.

CLASS t000_reporting IMPLEMENTATION.

  METHOD _do_some_processing.

    table = REF #( t000_tab ).

  ENDMETHOD.

  METHOD _select.

    SELECT *
           FROM t000
           INTO TABLE t000_tab.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_demo_view IMPLEMENTATION.

  METHOD lif_view~display.

    ASSIGN ir_table->* TO FIELD-SYMBOL(<table>).
    ASSERT sy-subrc = 0.

    cl_demo_output=>display( <table> ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_alv_view IMPLEMENTATION.

  METHOD lif_view~display.

    ASSIGN ir_table->* TO FIELD-SYMBOL(<table>).
    ASSERT sy-subrc = 0.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(alv)
          CHANGING
            t_table        = <table> ).

        alv->display( ).

      CATCH cx_salv_msg INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_view_factory IMPLEMENTATION.

  METHOD create.

    r_instance = COND #( WHEN demo = abap_true THEN NEW lcl_demo_view( )
                         WHEN alv  = abap_true THEN NEW lcl_alv_view( )
                         ELSE THROW lcx_invalid_type( ) ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  TRY.

      test_reporting_demo=>create( i_table = table
                                   i_view  = NEW lcl_view_factory( )->create( ) )->start( ).

    CATCH lcx_invalid_type INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
