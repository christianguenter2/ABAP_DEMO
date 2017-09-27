*&---------------------------------------------------------------------*
*& Report z_test_2017_07_20
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_07_20.

PARAMETERS: demo TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X',
            alv  TYPE abap_bool RADIOBUTTON GROUP r1.

INTERFACE lif_view.

  METHODS:
    display
      IMPORTING
        it_data TYPE INDEX TABLE.

ENDINTERFACE.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          io_view TYPE REF TO lif_view,

      start.

  PRIVATE SECTION.
    DATA: t100_tab TYPE STANDARD TABLE OF t100,
          mo_view  TYPE REF TO lif_view.

    METHODS:
      _select,
      _process,
      _display.

ENDCLASS.

CLASS demo_view DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS alv_view DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD constructor.

    mo_view = io_view.

  ENDMETHOD.

  METHOD start.

    _select( ).
    _process( ).
    _display( ).

  ENDMETHOD.

  METHOD _select.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @t100_tab
           UP TO 100 ROWS.

  ENDMETHOD.

  METHOD _process.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).

    ENDLOOP.

  ENDMETHOD.

  METHOD _display.

    mo_view->display( t100_tab ).

  ENDMETHOD.

ENDCLASS.

CLASS demo_view IMPLEMENTATION.

  METHOD lif_view~display.

    cl_demo_output=>display( it_data ).

  ENDMETHOD.

ENDCLASS.

CLASS alv_view IMPLEMENTATION.

  METHOD lif_view~display.

    DATA: dref TYPE REF TO data.

    CREATE DATA dref LIKE it_data.
    ASSIGN dref->* TO FIELD-SYMBOL(<table>).

    <table> = it_data.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(alv)
          CHANGING
            t_table      = <table> ).

        alv->display( ).

      CATCH cx_salv_msg INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW controller( io_view = COND #( WHEN demo = abap_true THEN NEW demo_view( )
                                    WHEN alv  = abap_true THEN NEW alv_view( ) )
   )->start( ).
