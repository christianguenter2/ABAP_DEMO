REPORT z_test_mvc.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_alv  TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X',
            p_demo TYPE abap_bool RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK b1.

INTERFACE lif_view.
  METHODS:
    display
      IMPORTING
        it_data TYPE ANY TABLE.
ENDINTERFACE.

INTERFACE lif_model.
  METHODS:
    get_data
      EXPORTING
        et_data TYPE ANY TABLE.
ENDINTERFACE.

CLASS lcl_alv_view DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_view.

  PRIVATE SECTION.
    DATA: lo_alv TYPE REF TO cl_salv_table.
ENDCLASS.

CLASS lcl_alv_view IMPLEMENTATION.
  METHOD lif_view~display.
    DATA: dref TYPE REF TO data.

    CREATE DATA dref LIKE it_data.
    ASSIGN dref->* TO FIELD-SYMBOL(<table>).

    <table> = it_data.

    cl_salv_table=>factory(
      IMPORTING
        r_salv_table   = lo_alv
      CHANGING
        t_table        = <table> ).

    lo_alv->display( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_demo_view DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_view.
ENDCLASS.

CLASS lcl_demo_view IMPLEMENTATION.
  METHOD lif_view~display.
    cl_demo_output=>display_data( it_data ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_view_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: get_view RETURNING VALUE(r_view) TYPE REF TO lif_view.
ENDCLASS.

CLASS lcl_view_factory IMPLEMENTATION.
  METHOD get_view.
    r_view = COND #( WHEN p_alv = abap_true THEN NEW lcl_alv_view( )
                     WHEN p_demo = abap_true THEN NEW lcl_demo_view( )
                     ELSE THROW cx_no_data_found( ) ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_model DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_model.
    TYPES: BEGIN OF ty_data,
             i TYPE i,
             s TYPE string,
           END OF ty_data,
           tty_data TYPE STANDARD TABLE OF ty_data WITH NON-UNIQUE EMPTY KEY.
ENDCLASS.

CLASS lcl_model IMPLEMENTATION.
  METHOD lif_model~get_data.
    et_data = VALUE tty_data( ( i = 1 s = 'Hallo Welt' )
                              ( i = 2 s = 'Dies ist ein Test' ) ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_controller DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          i_view  TYPE REF TO lif_view
          i_model TYPE REF TO lif_model,
      start.

  PRIVATE SECTION.
    DATA: _view  TYPE REF TO lif_view,
          _model TYPE REF TO lif_model.
ENDCLASS.

CLASS lcl_controller IMPLEMENTATION.
  METHOD constructor.
    _view  = i_view.
    _model = i_model.
  ENDMETHOD.

  METHOD start.
    DATA: lt_data TYPE lcl_model=>tty_data.

    _model->get_data(
      IMPORTING
        et_data = lt_data ).

    _view->display( lt_data ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_controller( i_view  = lcl_view_factory=>get_view( )
                      i_model = NEW lcl_model( ) )->start( ).
