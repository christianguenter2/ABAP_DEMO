*&---------------------------------------------------------------------*
*& Report z_test_reporting_demo_2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_reporting_demo_2.

PARAMETERS: alv  TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X',
            demo TYPE abap_bool RADIOBUTTON GROUP r1.

INTERFACE lif_view.
  METHODS: display
    IMPORTING it_table TYPE REF TO data.
ENDINTERFACE.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          i_view TYPE REF TO lif_view,

      start.

  PRIVATE SECTION.

    DATA: t000_tab TYPE STANDARD TABLE OF t000,
          view     TYPE REF TO lif_view.

    METHODS:
      _select,
      _display.

ENDCLASS.


CLASS lcl_demo_view DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES:
      lif_view.

ENDCLASS.

CLASS lcl_alv_view DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES:
      lif_view.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD constructor.

    me->view = i_view.

  ENDMETHOD.

  METHOD start.

    _select( ).
    _display( ).

  ENDMETHOD.

  METHOD _display.

    view->display( REF #( t000_tab ) ).

  ENDMETHOD.

  METHOD _select.

    SELECT *
           FROM t000
           INTO TABLE @t000_tab
           UP TO 100 ROWS.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_demo_view IMPLEMENTATION.

  METHOD lif_view~display.

    FIELD-SYMBOLS: <table> TYPE any.

    ASSIGN it_table->* TO <table>.
    ASSERT sy-subrc = 0.

    CLEAR: <table>.

    cl_demo_output=>display( <table> ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_alv_view IMPLEMENTATION.

  METHOD lif_view~display.

    ASSIGN it_table->* TO FIELD-SYMBOL(<table>).
    ASSERT sy-subrc = 0.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(alv)
          CHANGING
            t_table        = <table> ).

        alv->display( ).

      CATCH cx_salv_msg INTO DATA(error).    "
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA: view TYPE REF TO lif_view.

  view = COND #( WHEN demo = abap_true THEN NEW lcl_demo_view( )
                 ELSE NEW lcl_alv_view( ) ).

  NEW controller( view )->start( ).
