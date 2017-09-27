*&---------------------------------------------------------------------*
*& Report z_test_template_3
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_template_3.

PARAMETERS: table TYPE string OBLIGATORY,
            alv   RADIOBUTTON GROUP r1 DEFAULT 'X',
            demo  RADIOBUTTON GROUP r1.

INTERFACE lif_view.
  METHODS: display IMPORTING ir_data TYPE REF TO data.
ENDINTERFACE.

CLASS lcl_demo_view DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS lcl_demo_view IMPLEMENTATION.

  METHOD lif_view~display.

    ASSIGN ir_data->* TO FIELD-SYMBOL(<table>).
    ASSERT sy-subrc = 0.

    cl_demo_output=>display( <table> ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_alv_view DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS lcl_alv_view IMPLEMENTATION.

  METHOD lif_view~display.

    ASSIGN ir_data->* TO FIELD-SYMBOL(<table>).
    ASSERT sy-subrc = 0.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(alv)    " Basisklasse einfache ALV Tabellen
          CHANGING
            t_table        = <table> ).

      CATCH cx_salv_msg INTO DATA(error).

        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.

    alv->display( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcx_invalid_type DEFINITION CREATE PUBLIC INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        textid   LIKE textid OPTIONAL
        previous LIKE previous OPTIONAL
        table    TYPE csequence.

    METHODS: get_text REDEFINITION.

  PRIVATE SECTION.
    DATA _table TYPE string.

ENDCLASS.

CLASS lcx_invalid_type IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid = textid previous = previous ).

    _table = table.

  ENDMETHOD.

  METHOD get_text.

    result = | Table { _table } not yet implemented! |.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_abstract_reporting DEFINITION ABSTRACT.

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING
        i_table           TYPE csequence
        i_view            TYPE REF TO lif_view
      RETURNING
        VALUE(r_instance) TYPE REF TO lcl_abstract_reporting
      RAISING
        lcx_invalid_type.

    METHODS: constructor
      IMPORTING
        i_view TYPE REF TO lif_view,
      start.

  PROTECTED SECTION.
    METHODS:
      _display,
      _processing ABSTRACT,
      _select     ABSTRACT.

    DATA: table TYPE REF TO data.

  PRIVATE SECTION.
    DATA: _view TYPE REF TO lif_view.

ENDCLASS.

CLASS lcl_t100 DEFINITION CREATE PUBLIC INHERITING FROM lcl_abstract_reporting.

  PROTECTED SECTION.
    METHODS:
      _select     REDEFINITION,
      _processing REDEFINITION.

  PRIVATE SECTION.
    DATA: t100_tab TYPE STANDARD TABLE OF t100.

ENDCLASS.

CLASS lcl_t000 DEFINITION INHERITING FROM lcl_abstract_reporting.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS:
      _processing REDEFINITION,
      _select     REDEFINITION.

  PRIVATE SECTION.
    DATA: t000_tab TYPE STANDARD TABLE OF t000.

ENDCLASS.

CLASS lcl_t000 IMPLEMENTATION.

  METHOD _processing.

    table = REF #( t000_tab ).

  ENDMETHOD.

  METHOD _select.

    SELECT *
           FROM t000
           INTO TABLE @t000_tab.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_abstract_reporting IMPLEMENTATION.

  METHOD constructor.

    me->_view = i_view.

  ENDMETHOD.

  METHOD start.

    _select( ).
    _processing( ).
    _display( ).

  ENDMETHOD.

  METHOD create.

    r_instance = SWITCH #( i_table WHEN 'T100' THEN NEW lcl_t100( i_view )
                                   WHEN 'T000' THEN NEW lcl_t000( i_view )
                                   ELSE THROW lcx_invalid_type( i_table ) ).

  ENDMETHOD.

  METHOD _display.

    _view->display( table ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_t100 IMPLEMENTATION.

  METHOD _processing.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).
      " do some processing
    ENDLOOP.

    table = REF #( t100_tab ).

  ENDMETHOD.

  METHOD _select.

    SELECT *
           FROM t100
           UP TO 100 ROWS
           INTO TABLE @t100_tab.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA: view TYPE REF TO lif_view.

  view = COND #( WHEN alv  = abap_true THEN NEW lcl_alv_view( )
                 WHEN demo = abap_true THEN NEW lcl_demo_view( ) ).

  TRY.
      lcl_abstract_reporting=>create( i_table = table
                                      i_view  = view )->start( ).
    CATCH lcx_invalid_type INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
