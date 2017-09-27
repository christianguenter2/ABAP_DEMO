*&---------------------------------------------------------------------*
*& Report z_test_2017_08_31
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_08_31.

PARAMETERS: alv   TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X',
            demo  TYPE abap_bool RADIOBUTTON GROUP r1,
            fiori TYPE abap_bool RADIOBUTTON GROUP r1.

INTERFACE lif_view.

  METHODS: display
    IMPORTING
      it_data TYPE INDEX TABLE.

ENDINTERFACE.

CLASS lcx_invalid_view DEFINITION CREATE PUBLIC
                       INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    METHODS:
      get_text REDEFINITION.

ENDCLASS.

CLASS lcx_invalid_view IMPLEMENTATION.

  METHOD get_text.

    result = |Invalid view|.

  ENDMETHOD.

ENDCLASS.

CLASS alv_view DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES: lif_view.
    ALIASES: display FOR lif_view~display.

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
            t_table 		 = <table> ).

        alv->display( ).

      CATCH cx_salv_msg INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS demo_view DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      lif_view.

ENDCLASS.

CLASS demo_view IMPLEMENTATION.

  METHOD lif_view~display.

    cl_demo_output=>display( it_data ).

  ENDMETHOD.

ENDCLASS.

CLASS view_factory DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_view
        RETURNING
          VALUE(ro_view) TYPE REF TO lif_view
        RAISING
          lcx_invalid_view.

ENDCLASS.

CLASS view_factory IMPLEMENTATION.

  METHOD get_view.

    ro_view = COND #( WHEN alv  = abap_true THEN NEW alv_view( )
                      WHEN demo = abap_true THEN NEW demo_view( )
                      ELSE THROW lcx_invalid_view( ) ).

  ENDMETHOD.

ENDCLASS.

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

START-OF-SELECTION.
  TRY.
      NEW controller( view_factory=>get_view( ) )->start( ).
    CATCH cx_root INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
