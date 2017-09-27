*&---------------------------------------------------------------------*
*& Report z_test_2016_11_21
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2016_11_21.

PARAMETERS: alv  TYPE char01 RADIOBUTTON GROUP r1 DEFAULT 'X',
            demo TYPE char01 RADIOBUTTON GROUP r1.

INTERFACE lif_view.

  METHODS:
    display
      IMPORTING
        irt_data TYPE REF TO data.

ENDINTERFACE.

CLASS demo_view DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS alv_view DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:

      constructor
        IMPORTING
          i_view TYPE REF TO lif_view,

      start.

  PRIVATE SECTION.

    DATA: table_data TYPE STANDARD TABLE OF t100,
          _view      TYPE REF TO lif_view.

    METHODS:
      _select,
      _display.

ENDCLASS.

CLASS demo_view IMPLEMENTATION.

  METHOD lif_view~display.

    ASSIGN irt_data->* TO FIELD-SYMBOL(<table>).
    ASSERT sy-subrc = 0.

    cl_demo_output=>display( <table> ).

  ENDMETHOD.

ENDCLASS.

CLASS alv_view IMPLEMENTATION.

  METHOD lif_view~display.

    ASSIGN irt_data->* TO FIELD-SYMBOL(<table>).
    ASSERT sy-subrc = 0.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(alv)     " Basisklasse einfache ALV Tabellen
          CHANGING
            t_table        = <table> ).

        alv->display( ).

      CATCH cx_salv_msg INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD constructor.

    me->_view = i_view.

  ENDMETHOD.

  METHOD start.

    _select( ).
    _display( ).

  ENDMETHOD.

  METHOD _select.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @table_data
           UP TO 100 ROWS.

  ENDMETHOD.

  METHOD _display.

    _view->display( REF #( table_data ) ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA: view TYPE REF TO lif_view.

  view = COND #( WHEN alv  = abap_true THEN NEW alv_view( )
                 WHEN demo = abap_true THEN NEW demo_view( ) ).

  NEW controller( view )->start( ).
