*&---------------------------------------------------------------------*
*& Report z_test_2016_12_19
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2016_12_19.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-000.
PARAMETERS: demo TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X',
            alv  TYPE abap_bool RADIOBUTTON GROUP r1,
            ui5  TYPE abap_bool RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-001.
PARAMETERS: t100 TYPE abap_bool RADIOBUTTON GROUP r2 DEFAULT 'X',
            t000 TYPE abap_bool RADIOBUTTON GROUP r2,
            t001 TYPE abap_bool RADIOBUTTON GROUP r2,
            mara TYPE abap_bool RADIOBUTTON GROUP r2.
SELECTION-SCREEN END OF BLOCK b2.

INTERFACE lif_view.

  METHODS:
    display
      IMPORTING
        it_data TYPE REF TO data.

ENDINTERFACE.

INTERFACE lif_data_provider.

  METHODS:
    get_type
      RETURNING
        VALUE(r_type) TYPE REF TO cl_abap_tabledescr,

    get_data
      EXPORTING
        et_data TYPE STANDARD TABLE.

ENDINTERFACE.

CLASS demo_view DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS alv_view DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS dummy_view DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS t100_provider DEFINITION .

  PUBLIC SECTION.
    INTERFACES: lif_data_provider.

  PRIVATE SECTION.
    DATA: lt_data TYPE STANDARD TABLE OF t100.

ENDCLASS.

CLASS t000_provider DEFINITION .

  PUBLIC SECTION.
    INTERFACES: lif_data_provider.

  PRIVATE SECTION.
    DATA: lt_data TYPE STANDARD TABLE OF t000.

ENDCLASS.

CLASS t001_provider DEFINITION .

  PUBLIC SECTION.
    INTERFACES: lif_data_provider.

  PRIVATE SECTION.
    DATA: lt_data TYPE STANDARD TABLE OF t001.

ENDCLASS.

CLASS dummy_provider DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_string,
             text TYPE string,
           END OF ty_string,
           tty_string TYPE STANDARD TABLE OF ty_string
                           WITH NON-UNIQUE DEFAULT KEY.

    INTERFACES: lif_data_provider.

  PRIVATE SECTION.
    DATA: lt_data TYPE tty_string.

ENDCLASS.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          i_view          TYPE REF TO lif_view
          i_data_provider TYPE REF TO lif_data_provider,

      start.

  PRIVATE SECTION.

    DATA: view          TYPE REF TO lif_view,
          table         TYPE REF TO data,
          data_provider TYPE REF TO lif_data_provider.

    METHODS:
      _select,
      _display,
      _processing.

ENDCLASS.

CLASS demo_view IMPLEMENTATION.

  METHOD lif_view~display.

    ASSIGN it_data->* TO FIELD-SYMBOL(<table>).

    cl_demo_output=>write( <table> ).
    cl_demo_output=>display( ).

  ENDMETHOD.

ENDCLASS.

CLASS alv_view IMPLEMENTATION.

  METHOD lif_view~display.

    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.

    ASSIGN it_data->* TO <table>.

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

CLASS dummy_view IMPLEMENTATION.

  METHOD lif_view~display.

    MESSAGE |View nicht implementiert| TYPE 'I'.

  ENDMETHOD.

ENDCLASS.

CLASS t100_provider IMPLEMENTATION.

  METHOD lif_data_provider~get_data.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @lt_data
           UP TO 100 ROWS.

    et_data = lt_data.

  ENDMETHOD.

  METHOD lif_data_provider~get_type.

    r_type ?= cl_abap_tabledescr=>describe_by_data( lt_data ).

  ENDMETHOD.

ENDCLASS.

CLASS t000_provider IMPLEMENTATION.

  METHOD lif_data_provider~get_data.

    SELECT FROM t000
           FIELDS *
           INTO TABLE @lt_data.

    et_data = lt_data.

  ENDMETHOD.

  METHOD lif_data_provider~get_type.

    r_type ?= cl_abap_tabledescr=>describe_by_data( lt_data ).

  ENDMETHOD.

ENDCLASS.

CLASS t001_provider IMPLEMENTATION.

  METHOD lif_data_provider~get_data.

    SELECT FROM t001
           FIELDS *
           INTO TABLE @lt_data.

    et_data = lt_data.

  ENDMETHOD.

  METHOD lif_data_provider~get_type.

    r_type ?= cl_abap_tabledescr=>describe_by_data( lt_data ).

  ENDMETHOD.

ENDCLASS.

CLASS dummy_provider IMPLEMENTATION.

  METHOD lif_data_provider~get_data.

    INSERT VALUE #( text = `kein Datenprovider vorhanden` ) INTO TABLE lt_data.
    et_data = lt_data.

  ENDMETHOD.

  METHOD lif_data_provider~get_type.

    r_type ?= cl_abap_tabledescr=>describe_by_data( lt_data ).

  ENDMETHOD.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD constructor.

    view          = i_view.
    data_provider = i_data_provider.

  ENDMETHOD.

  METHOD start.

    _select( ).
    _processing( ).
    _display( ).

  ENDMETHOD.

  METHOD _display.

    ASSIGN table->* TO FIELD-SYMBOL(<table>).
    view->display( REF #( <table> ) ).

  ENDMETHOD.

  METHOD _select.

    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.

    DATA(type) = data_provider->get_type( ).

    CREATE DATA table TYPE HANDLE type.
    ASSIGN table->* TO <table>.

    data_provider->get_data(
      IMPORTING
        et_data = <table> ).

  ENDMETHOD.

  METHOD _processing.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA: view          TYPE REF TO lif_view,
        data_provider TYPE REF TO lif_data_provider.

  view = COND #( WHEN demo = abap_true THEN NEW demo_view( )
                 WHEN alv  = abap_true THEN NEW alv_view( )
                 ELSE NEW dummy_view( ) ).

  data_provider = COND #( WHEN t100 = abap_true THEN NEW t100_provider( )
                          WHEN t000 = abap_true THEN NEW t000_provider( )
                          WHEN t001 = abap_true THEN NEW t001_provider( )
                          ELSE NEW dummy_provider( ) ).

  NEW controller( i_view          = view
                  i_data_provider = data_provider )->start( ).
