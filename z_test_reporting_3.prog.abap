*&---------------------------------------------------------------------*
*& Report z_test_reporting_3
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_reporting_3.

PARAMETERS: table TYPE string OBLIGATORY,
            demo  RADIOBUTTON GROUP r1 DEFAULT 'X',
            alv   RADIOBUTTON GROUP r1,
            fiori RADIOBUTTON GROUP r1.

INTERFACE lif_reporting.
  METHODS:
    start.
ENDINTERFACE.

INTERFACE lif_view.
  METHODS:
    display
      IMPORTING
        i_table TYPE REF TO data.
ENDINTERFACE.

CLASS lcx_not_implemented DEFINITION CREATE PUBLIC
                          INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          textid   LIKE textid OPTIONAL
          previous LIKE previous OPTIONAL
          table    TYPE csequence OPTIONAL
          text     TYPE csequence OPTIONAL,

      get_text REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA _table TYPE string.
    DATA _text TYPE string.

ENDCLASS.

CLASS lcl_abstract_reporting DEFINITION ABSTRACT.

  PUBLIC SECTION.

    INTERFACES: lif_reporting.
    ALIASES: start FOR lif_reporting~start.
    CLASS-METHODS create
      IMPORTING
        i_table           TYPE csequence
        i_view            TYPE any OPTIONAL
      RETURNING
        VALUE(r_instance) TYPE REF TO lif_reporting
      RAISING
        lcx_not_implemented.
    METHODS constructor
      IMPORTING
        i_view TYPE REF TO lif_view.

  PROTECTED SECTION.
    METHODS:
      _display,
      _select ABSTRACT,
      _processing ABSTRACT.

    DATA: table TYPE REF TO data.

  PRIVATE SECTION.
    DATA: _view TYPE REF TO lif_view.

ENDCLASS.

CLASS lcl_t100_reporting DEFINITION CREATE PUBLIC
                         INHERITING FROM lcl_abstract_reporting.

  PUBLIC SECTION.

  PROTECTED SECTION.

    METHODS:
      _select REDEFINITION,
      _processing REDEFINITION.

  PRIVATE SECTION.

    DATA: t100_tab TYPE STANDARD TABLE OF t100.

ENDCLASS.

CLASS lcl_t000_reporting DEFINITION FINAL
                         INHERITING FROM lcl_abstract_reporting.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS:
      _select REDEFINITION,
      _processing REDEFINITION.

  PRIVATE SECTION.
    DATA: t000_tab TYPE STANDARD TABLE OF t000.

ENDCLASS.

CLASS lcl_demo_view DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES: lif_view.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_alv_view DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES: lif_view.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcx_not_implemented IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid = textid previous = previous ).
    _table = table.
    _text = text.

  ENDMETHOD.

  METHOD get_text.

    IF _table IS NOT INITIAL.

      result = |{ _table } not yet implemented!|.

    ELSE.

      result = |{ _text } not yet implemented!|.

    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_abstract_reporting IMPLEMENTATION.

  METHOD constructor.

    _view = i_view.

  ENDMETHOD.

  METHOD create.

    r_instance = COND #( WHEN i_table = |T100| THEN NEW lcl_t100_reporting( i_view )
                         WHEN i_table = |T000| THEN NEW lcl_t000_reporting( i_view )
                         ELSE THROW lcx_not_implemented( table = i_table ) ).

  ENDMETHOD.

  METHOD _display.

    _view->display( table ).

  ENDMETHOD.

  METHOD lif_reporting~start.

    _select( ).
    _processing( ).
    _display( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_t100_reporting IMPLEMENTATION.

  METHOD _processing.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).
      " do some processing here
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

  METHOD _processing.

    table = REF #( t000_tab ).

  ENDMETHOD.

  METHOD _select.

    SELECT *
           FROM t000
           INTO TABLE @t000_tab.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_demo_view IMPLEMENTATION.

  METHOD lif_view~display.

    ASSIGN i_table->* TO FIELD-SYMBOL(<table>).
    ASSERT sy-subrc = 0.

    cl_demo_output=>display( <table> ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_alv_view IMPLEMENTATION.

  METHOD lif_view~display.

    ASSIGN i_table->* TO FIELD-SYMBOL(<table>).
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

START-OF-SELECTION.

  TRY.
      DATA: view TYPE REF TO lif_view.

      view = COND #( WHEN demo = abap_true THEN NEW lcl_demo_view( )
                     WHEN alv  = abap_true THEN NEW lcl_alv_view( )
                     ELSE THROW lcx_not_implemented( text = |View| ) ).

      lcl_abstract_reporting=>create( i_table = table
                                      i_view  = view )->start( ).

    CATCH lcx_not_implemented INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
