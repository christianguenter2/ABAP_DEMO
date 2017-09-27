*&---------------------------------------------------------------------*
*& Report z_test_reporting
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_reporting.

PARAMETERS: p_table TYPE string OBLIGATORY,
            p_demo  RADIOBUTTON GROUP r1 DEFAULT 'X',
            p_alv   RADIOBUTTON GROUP r1,
            p_write RADIOBUTTON GROUP r1,
            p_web   RADIOBUTTON GROUP r1.

CLASS lcx_not_yet_implemented DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        textid   LIKE textid OPTIONAL
        previous LIKE previous OPTIONAL
        i_table  TYPE csequence OPTIONAL
        i_text   TYPE csequence OPTIONAL,

      get_text REDEFINITION.

  PRIVATE SECTION.
    DATA _table TYPE string.
    DATA _text TYPE string.

ENDCLASS.

INTERFACE lif_view.
  METHODS display
    IMPORTING
      ir_table TYPE REF TO data.

ENDINTERFACE.

INTERFACE lif_reporting.
  METHODS: start.

ENDINTERFACE.

CLASS lcl_write_view DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES: lif_view.

ENDCLASS.

CLASS lcl_alv_view DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS lcl_demo_view DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS lcl_view_factory DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING
        i_alv          TYPE abap_bool
        i_demo         TYPE abap_bool
        i_write        TYPE abap_bool
      RETURNING
        VALUE(ro_view) TYPE REF TO lif_view
      RAISING
        lcx_not_yet_implemented.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_view_factory IMPLEMENTATION.

  METHOD create.

    ro_view = COND #( WHEN p_alv   = abap_true THEN NEW lcl_alv_view( )
                      WHEN p_demo  = abap_true THEN NEW lcl_demo_view( )
                      WHEN p_write = abap_true THEN NEW lcl_write_view( )
                      ELSE THROW lcx_not_yet_implemented( i_text = |View| ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_alv_view IMPLEMENTATION.

  METHOD lif_view~display.

    ASSIGN ir_table->* TO FIELD-SYMBOL(<table>).
    ASSERT sy-subrc = 0.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(lo_alv)
          CHANGING
            t_table        = <table> ).

        lo_alv->display( ).

      CATCH cx_salv_msg INTO DATA(error).

        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_demo_view IMPLEMENTATION.

  METHOD lif_view~display.

    ASSIGN ir_table->* TO FIELD-SYMBOL(<table>).
    ASSERT sy-subrc = 0.

    cl_demo_output=>display( <table> ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_write_view IMPLEMENTATION.

  METHOD lif_view~display.

    FIELD-SYMBOLS: <table> TYPE ANY TABLE.

    ASSIGN ir_table->* TO <table>.
    ASSERT sy-subrc = 0.

    LOOP AT <table> ASSIGNING FIELD-SYMBOL(<line>).
      WRITE: / <line>.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lcx_not_yet_implemented IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid = textid previous = previous ).
    _table = i_table.
    _text  = i_text.

  ENDMETHOD.

  METHOD get_text.

    result = COND #( WHEN _table IS NOT INITIAL THEN |Reporting table { _table } not yet implemented!|
                     ELSE |{ _text } not yet implemented| ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_abstract_reporting DEFINITION ABSTRACT.

  PUBLIC SECTION.
    INTERFACES: lif_reporting.
    ALIASES: start FOR lif_reporting~start.

    CLASS-METHODS create
      IMPORTING
        i_table           TYPE csequence  OPTIONAL
        i_view            TYPE REF TO lif_view
      RETURNING
        VALUE(r_instance) TYPE REF TO lif_reporting
      RAISING
        lcx_not_yet_implemented.

    METHODS:
      constructor
        IMPORTING
          i_view TYPE REF TO lif_view,
      _select ABSTRACT,
      _processing ABSTRACT.

  PROTECTED SECTION.
    DATA: table TYPE REF TO data.

  PRIVATE SECTION.
    DATA: _view TYPE REF TO lif_view.
    METHODS _display.

ENDCLASS.


CLASS lcl_t100_reporting DEFINITION CREATE PUBLIC
                         INHERITING FROM lcl_abstract_reporting.

  PUBLIC SECTION.
    METHODS:
      _select     REDEFINITION,
      _processing REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: t100_tab TYPE STANDARD TABLE OF t100.

ENDCLASS.


CLASS lcl_t000_reporting DEFINITION CREATE PUBLIC
                         INHERITING FROM lcl_abstract_reporting.

  PUBLIC SECTION.
    METHODS:
      _select     REDEFINITION,
      _processing REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: t000_tab TYPE STANDARD TABLE OF t000.

ENDCLASS.

CLASS lcl_abstract_reporting IMPLEMENTATION.

  METHOD constructor.

    _view = i_view.

  ENDMETHOD.

  METHOD create.

    r_instance = COND #( WHEN i_table = 'T100' THEN NEW lcl_t100_reporting( i_view )
                         WHEN i_table = 'T000' THEN NEW lcl_t000_reporting( i_view )
                         ELSE THROW lcx_not_yet_implemented( i_table  = i_table ) ).

  ENDMETHOD.

  METHOD lif_reporting~start.

    _select( ).
    _processing( ).
    _display( ).

  ENDMETHOD.


  METHOD _display.

    _view->display( table ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_t100_reporting IMPLEMENTATION.

  METHOD _processing.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).
      " Do some processing
    ENDLOOP.

    table = REF #( t100_tab ).

  ENDMETHOD.

  METHOD _select.

    SELECT *
           FROM t100
           INTO TABLE @t100_tab
           UP TO 100 ROWS.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_t000_reporting IMPLEMENTATION.

  METHOD _select.

    SELECT *
           FROM t000
           INTO TABLE @t000_tab.

  ENDMETHOD.

  METHOD _processing.

    table = REF #( t000_tab ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  TRY.
      DATA(lo_view) = lcl_view_factory=>create( i_alv   = p_alv
                                                i_demo  = p_demo
                                                i_write = p_write ).

      lcl_abstract_reporting=>create( i_table = p_table
                                      i_view  = lo_view )->start( ).

    CATCH lcx_not_yet_implemented INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
